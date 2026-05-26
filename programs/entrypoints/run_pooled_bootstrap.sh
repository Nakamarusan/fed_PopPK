#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_ROOT}/../.." && pwd)"
SCRIPT_PROJECT="/project/${SCRIPT_ROOT#${REPO_ROOT}/}"
TARGET_ROOT="${DEFERIPRONE_OUTPUT_ROOT:-${SCRIPT_ROOT}}"
TARGET_ROOT="$(realpath -m "${TARGET_ROOT}")"
TARGET_PROJECT="/project/${TARGET_ROOT#${REPO_ROOT}/}"
IMAGE="${FEDPOPPK_IMAGE:-fedpoppk/base:0.1.0}"
UID_GID="$(id -u):$(id -g)"

SCENARIO="${1:-}"
if [[ -z "${SCENARIO}" ]]; then
  echo "usage: $0 <scenario1|scenario2>" >&2
  exit 1
fi
if [[ "${SCENARIO}" != "scenario1" && "${SCENARIO}" != "scenario2" ]]; then
  echo "scenario must be scenario1 or scenario2" >&2
  exit 1
fi

WORKERS="${WORKERS:-10}"
TRIAL_START="${TRIAL_START:-1}"
TRIAL_END="${TRIAL_END:-500}"
CONTAINER_CPUS="${CONTAINER_CPUS:-${WORKERS}}"

CFG_PROJECT="${TARGET_PROJECT}/configs/resolved/${SCENARIO}_standard.json"
TRIAL_PROJECT="${TARGET_PROJECT}/data/${SCENARIO}/bootstrap"
OUT_PROJECT="${TARGET_PROJECT}/runs/${SCENARIO}/bootstrap/centralized"
mkdir -p "${TARGET_ROOT}/runs/${SCENARIO}/bootstrap/centralized"

docker run --rm \
  --cpus="${CONTAINER_CPUS}" \
  --user "${UID_GID}" \
  -e HOME=/tmp \
  -e OMP_NUM_THREADS=1 \
  -e OPENBLAS_NUM_THREADS=1 \
  -e MKL_NUM_THREADS=1 \
  -e VECLIB_MAXIMUM_THREADS=1 \
  -e NUMEXPR_NUM_THREADS=1 \
  -v "${REPO_ROOT}:/project" \
  -w /project \
  "${IMAGE}" \
  bash -lc "Rscript ${SCRIPT_PROJECT}/run_pooled_bootstrap.R \
    --scenario=${SCENARIO} \
    --config=${CFG_PROJECT} \
    --trial-root=${TRIAL_PROJECT} \
    --output-root=${OUT_PROJECT} \
    --trial-start=${TRIAL_START} \
    --trial-end=${TRIAL_END} \
    --workers=${WORKERS}"
