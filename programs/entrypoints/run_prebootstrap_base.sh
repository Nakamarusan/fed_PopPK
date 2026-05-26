#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${ROOT_DIR}/../.." && pwd)"
ROOT_PROJECT="/project/${ROOT_DIR#${REPO_ROOT}/}"
IMAGE="${FEDPOPPK_IMAGE:-fedpoppk/base:0.1.0}"
UID_GID="$(id -u):$(id -g)"
LOG_DIR="${ROOT_DIR}/runs/prebootstrap"

run_rscript() {
  docker run --rm --user "${UID_GID}" -e HOME=/tmp -v "${REPO_ROOT}:/project" -w /project "${IMAGE}" \
    bash -lc "$1"
}

mkdir -p "${LOG_DIR}"

"${ROOT_DIR}/run_generate_and_plot.sh" \
  > "${LOG_DIR}/00_generate_data_and_plot.log" 2>&1

"${ROOT_DIR}/run_standard_nonfederated.sh" \
  > "${LOG_DIR}/01_run_standard_nonfederated.log" 2>&1

"${ROOT_DIR}/run_standard_federated.sh" \
  > "${LOG_DIR}/02_run_standard_federated.log" 2>&1

run_rscript "set -euo pipefail; Rscript ${ROOT_PROJECT}/materialize_prebootstrap_report.R" \
  > "${LOG_DIR}/03_materialize_prebootstrap_report.log" 2>&1

run_rscript "set -euo pipefail; Rscript ${ROOT_PROJECT}/materialize_reports_figures.R" \
  > "${LOG_DIR}/04_materialize_reports_figures.log" 2>&1

run_rscript "set -euo pipefail; Rscript ${ROOT_PROJECT}/materialize_reports.R" \
  > "${LOG_DIR}/05_materialize_reports.log" 2>&1

run_rscript "set -euo pipefail; Rscript ${ROOT_PROJECT}/materialize_supplementary.R" \
  > "${LOG_DIR}/06_materialize_supplementary.log" 2>&1

printf '%s\n' "${LOG_DIR}"
