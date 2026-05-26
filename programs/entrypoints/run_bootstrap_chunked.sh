#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_ROOT}/../.." && pwd)"
SCRIPT_REL="${SCRIPT_ROOT#${REPO_ROOT}/}"
TARGET_ROOT="${DEFERIPRONE_OUTPUT_ROOT:-${SCRIPT_ROOT}}"
TARGET_ROOT="$(realpath -m "${TARGET_ROOT}")"
TARGET_REL="${TARGET_ROOT#${REPO_ROOT}/}"
TARGET_PROJECT="/project/${TARGET_REL}"
ROOT_BASENAME="$(basename "${TARGET_ROOT}")"
cd "${REPO_ROOT}"

SCENARIO="${1:-scenario1}"
START_AT="${START_AT:-1}"
END_AT="${END_AT:-500}"
CHUNK_SIZE="${CHUNK_SIZE:-5}"
B="${B:-500}"
SEED_BASE="${SEED_BASE:-4869}"
BOOT_BASE_EXTENDS="${BOOT_BASE_EXTENDS:-base.json}"

CLIENT1_CPUS="${CLIENT1_CPUS:-6}"
CLIENT2_CPUS="${CLIENT2_CPUS:-6}"
CLIENT3_CPUS="${CLIENT3_CPUS:-6}"
SERVER_CPUS="${SERVER_CPUS:-3}"
CLIENT1_GRAD_CORES="${CLIENT1_GRAD_CORES:-6}"
CLIENT2_GRAD_CORES="${CLIENT2_GRAD_CORES:-6}"
CLIENT3_GRAD_CORES="${CLIENT3_GRAD_CORES:-6}"
SERVER_POST_CORES="${SERVER_POST_CORES:-3}"
RUN_TIMEOUT="${RUN_TIMEOUT:-1200}"

case "${SCENARIO}" in
  scenario1|scenario2) ;;
  *)
    echo "Unsupported scenario: ${SCENARIO}" >&2
    exit 2
    ;;
esac

if [[ ! "${START_AT}" =~ ^[0-9]+$ || ! "${END_AT}" =~ ^[0-9]+$ || ! "${CHUNK_SIZE}" =~ ^[0-9]+$ ]]; then
  echo "START_AT, END_AT, and CHUNK_SIZE must be positive integers" >&2
  exit 2
fi
if (( START_AT < 1 || END_AT < START_AT || CHUNK_SIZE < 1 )); then
  echo "Invalid trial range or chunk size" >&2
  exit 2
fi

if [[ "${SCENARIO}" == "scenario1" ]]; then
  SITE1_SUBNET="${SITE1_SUBNET:-172.83.11.0/24}"
  SITE2_SUBNET="${SITE2_SUBNET:-172.83.12.0/24}"
  SITE3_SUBNET="${SITE3_SUBNET:-172.83.13.0/24}"
else
  SITE1_SUBNET="${SITE1_SUBNET:-172.84.11.0/24}"
  SITE2_SUBNET="${SITE2_SUBNET:-172.84.12.0/24}"
  SITE3_SUBNET="${SITE3_SUBNET:-172.84.13.0/24}"
fi

if [[ ! -f "${TARGET_ROOT}/configs/resolved/${BOOT_BASE_EXTENDS}" ]]; then
  echo "Missing config: ${TARGET_ROOT}/configs/resolved/${BOOT_BASE_EXTENDS}" >&2
  exit 2
fi
if [[ ! -f "${TARGET_ROOT}/data/${SCENARIO}/bootstrap/manifest.csv" ]]; then
  echo "Missing bootstrap manifest: ${TARGET_ROOT}/data/${SCENARIO}/bootstrap/manifest.csv" >&2
  echo "Run materialize_bootstrap_trials.sh first." >&2
  exit 2
fi

mkdir -p \
  "${TARGET_ROOT}/configs/resolved" \
  "${TARGET_ROOT}/cache/${SCENARIO}/client1" \
  "${TARGET_ROOT}/cache/${SCENARIO}/client2" \
  "${TARGET_ROOT}/cache/${SCENARIO}/client3" \
  "${TARGET_ROOT}/runs/${SCENARIO}/bootstrap/federated/${SCENARIO}/raw_runs" \
  "${TARGET_ROOT}/runs/${SCENARIO}/bootstrap/federated/reporting" \
  "${TARGET_ROOT}/runs/${SCENARIO}/bootstrap/federated/aggregate" \
  "${TARGET_ROOT}/runs/${SCENARIO}/bootstrap/federated/logs"

cat > "${TARGET_ROOT}/configs/resolved/bootstrap_base.json" <<JSON
{
  "extends": ["${BOOT_BASE_EXTENDS}"],
  "bootstrap": {
    "B_per_client": ${B},
    "seed_base": ${SEED_BASE}
  },
  "comm": {
    "init_max_tries": 20,
    "init_pause": 5,
    "init_timeout": 300,
    "run_max_tries": 3,
    "run_pause": 1,
    "run_timeout": ${RUN_TIMEOUT}
  },
  "logging": {
    "scenario": "raw_runs",
    "save_par_log": true
  }
}
JSON

cat > "${TARGET_ROOT}/configs/resolved/${SCENARIO}_bootstrap.json" <<JSON
{
  "extends": ["bootstrap_base.json"],
  "clients": {
    "http://client1:8000": "${TARGET_PROJECT}/data/${SCENARIO}/base/data1_1.csv",
    "http://client2:8000": "${TARGET_PROJECT}/data/${SCENARIO}/base/data1_2.csv",
    "http://client3:8000": "${TARGET_PROJECT}/data/${SCENARIO}/base/data1_3.csv"
  },
  "logging": {
    "base_dir": "${TARGET_PROJECT}/runs/${SCENARIO}/bootstrap/federated",
    "scenario": "${SCENARIO}/raw_runs",
    "save_par_log": true
  },
  "bootstrap": {
    "B_per_client": ${B},
    "seed_base": ${SEED_BASE},
    "pre_generated_root": "${TARGET_PROJECT}/data/${SCENARIO}/bootstrap"
  }
}
JSON

cat > "${TARGET_ROOT}/compose.bootstrap.cache.yml" <<YAML
services:
  client1:
    volumes:
      - ${TARGET_ROOT}/cache/${SCENARIO}/client1:/project/cache

  client2:
    volumes:
      - ${TARGET_ROOT}/cache/${SCENARIO}/client2:/project/cache

  client3:
    volumes:
      - ${TARGET_ROOT}/cache/${SCENARIO}/client3:/project/cache
YAML

BOOT_CFG="../${TARGET_REL}/configs/resolved/${SCENARIO}_bootstrap.json"
COMPOSE_REANALYSIS="${SCRIPT_REL}/runtime/compose/compose.reanalysis.yml"
COMPOSE_CACHE="${TARGET_ROOT}/compose.bootstrap.cache.yml"
LOG_DIR="${TARGET_ROOT}/runs/${SCENARIO}/bootstrap/federated/logs"

for (( START=START_AT; START<=END_AT; START+=CHUNK_SIZE )); do
  END=$(( START + CHUNK_SIZE - 1 ))
  if (( END > END_AT )); then END="${END_AT}"; fi
  COUNT=$(( END - START + 1 ))
  TAG="$(printf '%s' "${ROOT_BASENAME}" | tr '[:upper:]' '[:lower:]' | tr -c 'a-z0-9_-' '_' | cut -c1-28 | sed 's/[_-]*$//')"
  PROJ="$(printf 'defboot_%s_%s_%s_%s' "${TAG}" "${SCENARIO}" "${START}" "${END}" | cut -c1-62 | sed 's/[_-]*$//')"
  LOG_PATH="${LOG_DIR}/chunk_${START}_${END}.log"

  echo "[federated bootstrap] ${SCENARIO} trials ${START}-${END}; log=${LOG_PATH}"
  set +e
  env \
    UID="$(id -u)" \
    GID="$(id -g)" \
    SCENARIO="${SCENARIO}" \
    BOOT_CONFIG="${BOOT_CFG}" \
    BOOT_TRIAL_START="${START}" \
    BOOT_TRIAL_COUNT="${COUNT}" \
    CLIENT1_CPUS="${CLIENT1_CPUS}" \
    CLIENT2_CPUS="${CLIENT2_CPUS}" \
    CLIENT3_CPUS="${CLIENT3_CPUS}" \
    SERVER_CPUS="${SERVER_CPUS}" \
    CLIENT1_GRAD_CORES="${CLIENT1_GRAD_CORES}" \
    CLIENT2_GRAD_CORES="${CLIENT2_GRAD_CORES}" \
    CLIENT3_GRAD_CORES="${CLIENT3_GRAD_CORES}" \
    SERVER_POST_CORES="${SERVER_POST_CORES}" \
    SITE1_SUBNET="${SITE1_SUBNET}" \
    SITE2_SUBNET="${SITE2_SUBNET}" \
    SITE3_SUBNET="${SITE3_SUBNET}" \
    docker compose -p "${PROJ}" \
      -f compose/base.yml \
      -f "${COMPOSE_REANALYSIS}" \
      -f compose/modes/bootstrap.yml \
      -f "${COMPOSE_CACHE}" \
      up --build --abort-on-container-exit \
      2>&1 | tee "${LOG_PATH}"
  status=${PIPESTATUS[0]}
  set -e

  env \
    UID="$(id -u)" \
    GID="$(id -g)" \
    SCENARIO="${SCENARIO}" \
    BOOT_CONFIG="${BOOT_CFG}" \
    BOOT_TRIAL_START="${START}" \
    BOOT_TRIAL_COUNT="${COUNT}" \
    CLIENT1_CPUS="${CLIENT1_CPUS}" \
    CLIENT2_CPUS="${CLIENT2_CPUS}" \
    CLIENT3_CPUS="${CLIENT3_CPUS}" \
    SERVER_CPUS="${SERVER_CPUS}" \
    CLIENT1_GRAD_CORES="${CLIENT1_GRAD_CORES}" \
    CLIENT2_GRAD_CORES="${CLIENT2_GRAD_CORES}" \
    CLIENT3_GRAD_CORES="${CLIENT3_GRAD_CORES}" \
    SERVER_POST_CORES="${SERVER_POST_CORES}" \
    SITE1_SUBNET="${SITE1_SUBNET}" \
    SITE2_SUBNET="${SITE2_SUBNET}" \
    SITE3_SUBNET="${SITE3_SUBNET}" \
    docker compose -p "${PROJ}" \
      -f compose/base.yml \
      -f "${COMPOSE_REANALYSIS}" \
      -f compose/modes/bootstrap.yml \
      -f "${COMPOSE_CACHE}" \
      down \
      >> "${LOG_PATH}" 2>&1 || true

  if [[ "${status}" -ne 0 ]]; then
    echo "Federated bootstrap failed for ${SCENARIO} trials ${START}-${END}. See ${LOG_PATH}" >&2
    exit "${status}"
  fi
done
