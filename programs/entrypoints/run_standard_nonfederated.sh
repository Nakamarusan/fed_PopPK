#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${ROOT_DIR}/../.." && pwd)"
ROOT_PROJECT="/project/${ROOT_DIR#${REPO_ROOT}/}"
IMAGE="${FEDPOPPK_IMAGE:-fedpoppk/base:0.1.0}"
UID_GID="$(id -u):$(id -g)"

run_rscript() {
  docker run --rm --user "${UID_GID}" -e HOME=/tmp -v "${REPO_ROOT}:/project" -w /project "${IMAGE}" \
    bash -lc "$1"
}

mkdir -p \
  "${ROOT_DIR}/runs/scenario1/standard/centralized/raw" \
  "${ROOT_DIR}/runs/scenario1/standard/centralized/reporting" \
  "${ROOT_DIR}/runs/scenario2/standard/centralized/raw" \
  "${ROOT_DIR}/runs/scenario2/standard/centralized/reporting" \
  "${ROOT_DIR}/reports/tables" \
  "${ROOT_DIR}/reports/main"

for scenario in scenario1 scenario2; do
  run_rscript "set -euo pipefail; Rscript ${ROOT_PROJECT}/run_base_centralized_fit.R --input-dir ${ROOT_PROJECT}/data/${scenario}/base --output-dir ${ROOT_PROJECT}/runs/${scenario}/standard/centralized/raw --federated-config ${ROOT_PROJECT}/configs/resolved/${scenario}_standard.json" \
    > "${ROOT_DIR}/runs/${scenario}/standard/centralized/raw/run.log" 2>&1

  cp "${ROOT_DIR}/runs/${scenario}/standard/centralized/raw/fit_estimates.csv" \
    "${ROOT_DIR}/runs/${scenario}/standard/centralized/reporting/fit_estimates.csv"
  cp "${ROOT_DIR}/runs/${scenario}/standard/centralized/raw/fit_estimates.md" \
    "${ROOT_DIR}/runs/${scenario}/standard/centralized/reporting/fit_estimates.md"
  cp "${ROOT_DIR}/runs/${scenario}/standard/centralized/raw/pooled_summary.csv" \
    "${ROOT_DIR}/runs/${scenario}/standard/centralized/reporting/pooled_summary.csv"
done
