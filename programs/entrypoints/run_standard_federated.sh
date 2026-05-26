#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${ROOT_DIR}/../.." && pwd)"
ROOT_PROJECT="/project/${ROOT_DIR#${REPO_ROOT}/}"
BASE_COMPOSE="${REPO_ROOT}/compose/base.yml"
COMPOSE_OVERRIDE="${ROOT_DIR}/runtime/compose/compose.reanalysis.yml"
ROOT_BASENAME="$(basename "${ROOT_DIR}")"
FORMATTER="${ROOT_PROJECT}/format_federated_outputs.R"
EVAL_FED="${ROOT_PROJECT}/evaluate_federated_pooled_metrics.R"
IMAGE="${FEDPOPPK_IMAGE:-fedpoppk/base:0.1.0}"
UID_GID="$(id -u):$(id -g)"

CLIENT1_CPUS="${CLIENT1_CPUS:-3}"
CLIENT2_CPUS="${CLIENT2_CPUS:-3}"
CLIENT3_CPUS="${CLIENT3_CPUS:-3}"
SERVER_CPUS="${SERVER_CPUS:-2}"
CLIENT1_GRAD_CORES="${CLIENT1_GRAD_CORES:-3}"
CLIENT2_GRAD_CORES="${CLIENT2_GRAD_CORES:-3}"
CLIENT3_GRAD_CORES="${CLIENT3_GRAD_CORES:-3}"
SERVER_POST_CORES="${SERVER_POST_CORES:-2}"

host_to_project_path() {
  local path="$1"
  path="${path#${REPO_ROOT}/}"
  printf '/project/%s\n' "$path"
}

run_rscript() {
  docker run --rm --user "${UID_GID}" -e HOME=/tmp -v "${REPO_ROOT}:/project" -w /project "${IMAGE}" \
    bash -lc "$1"
}

for scenario in scenario1 scenario2; do
  config_path="${ROOT_PROJECT}/configs/resolved/${scenario}_standard.json"
  scenario_dir="${ROOT_DIR}/runs/${scenario}/standard/federated"
  raw_dir="${scenario_dir}/${scenario}/raw_runs"
  reporting_dir="${scenario_dir}/reporting"
  log_path="${scenario_dir}/compose_up.log"
  project_name="${ROOT_BASENAME}_${scenario}_std"

  mkdir -p "${raw_dir}" "${reporting_dir}"

  set +e
  env \
    UID="$(id -u)" \
    GID="$(id -g)" \
    SCENARIO="${scenario}" \
    SERVER_CONFIG_PATH="${config_path}" \
    CLIENT1_CPUS="${CLIENT1_CPUS}" \
    CLIENT2_CPUS="${CLIENT2_CPUS}" \
    CLIENT3_CPUS="${CLIENT3_CPUS}" \
    SERVER_CPUS="${SERVER_CPUS}" \
    CLIENT1_GRAD_CORES="${CLIENT1_GRAD_CORES}" \
    CLIENT2_GRAD_CORES="${CLIENT2_GRAD_CORES}" \
    CLIENT3_GRAD_CORES="${CLIENT3_GRAD_CORES}" \
    SERVER_POST_CORES="${SERVER_POST_CORES}" \
    docker compose -p "${project_name}" -f "${BASE_COMPOSE}" -f "${COMPOSE_OVERRIDE}" up --build --abort-on-container-exit \
      > "${log_path}" 2>&1
  status=$?
  set -e

  env \
    UID="$(id -u)" \
    GID="$(id -g)" \
    SCENARIO="${scenario}" \
    SERVER_CONFIG_PATH="${config_path}" \
    CLIENT1_CPUS="${CLIENT1_CPUS}" \
    CLIENT2_CPUS="${CLIENT2_CPUS}" \
    CLIENT3_CPUS="${CLIENT3_CPUS}" \
    SERVER_CPUS="${SERVER_CPUS}" \
    CLIENT1_GRAD_CORES="${CLIENT1_GRAD_CORES}" \
    CLIENT2_GRAD_CORES="${CLIENT2_GRAD_CORES}" \
    CLIENT3_GRAD_CORES="${CLIENT3_GRAD_CORES}" \
    SERVER_POST_CORES="${SERVER_POST_CORES}" \
    docker compose -p "${project_name}" -f "${BASE_COMPOSE}" -f "${COMPOSE_OVERRIDE}" down \
      >> "${log_path}" 2>&1 || true

  if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
  fi

  latest_run_host="$(find "${raw_dir}" -mindepth 1 -maxdepth 1 -type d -name 'run_*' | sort | tail -n 1)"
  if [[ -z "${latest_run_host}" ]]; then
    echo "No run directory found under ${raw_dir}" >&2
    exit 1
  fi

  printf '%s\n' "${latest_run_host}" > "${scenario_dir}/latest_run.txt"
  run_rscript "set -euo pipefail; Rscript ${FORMATTER} $(host_to_project_path "${latest_run_host}") $(host_to_project_path "${reporting_dir}")"
  run_rscript "set -euo pipefail; Rscript ${EVAL_FED} ${scenario}"
done
