#!/usr/bin/env bash
set -euo pipefail

REPO="${REPO:-$(pwd -P)}"
ROOT="${ROOT:-${REPO}}"
OUT="${OUT:-${REPO}/results}"
CONFIG_DIR="${CONFIG_DIR:-${REPO}/programs/configs/resolved}"
TRIAL_DATA_ROOT="${TRIAL_DATA_ROOT:-${REPO}/data}"
SCENARIO="${1:-scenario2}"

IMAGE="${FEDPOPPK_IMAGE:-fedpoppk/deferiprone:20260521}"
TIMEOUT_SEC="${TIMEOUT_SEC:-1200}"
PARALLEL="${PARALLEL:-6}"
CONTAINER_CPUS="${CONTAINER_CPUS:-1}"
TRIAL_START="${TRIAL_START:-1}"
TRIAL_END="${TRIAL_END:-500}"
MAXIT="${MAXIT:-100}"
PGTOL="${PGTOL:-0}"
FACTR="${FACTR:-1e9}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
SCRIPT_PROJECT="/project/${SCRIPT_DIR#${REPO}/}"

if [[ "${SCENARIO}" != "scenario1" && "${SCENARIO}" != "scenario2" ]]; then
  echo "usage: $0 <scenario1|scenario2>" >&2
  exit 1
fi

RAW_DIR="${OUT}/runs/${SCENARIO}/bootstrap/centralized/raw"
CANON_OUT="${OUT}/runs/${SCENARIO}/bootstrap/centralized"
TMP_ROOT="${CANON_OUT}/rerun_missing_work"
mkdir -p "${RAW_DIR}" "${TMP_ROOT}" "${CANON_OUT}/aggregate" "${CANON_OUT}/reporting"

if [[ -n "${MISSING_TRIALS:-}" ]]; then
  missing="${MISSING_TRIALS}"
else
  missing=""
  for trial in $(seq "${TRIAL_START}" "${TRIAL_END}"); do
    trial4="$(printf "%04d" "${trial}")"
    if [[ ! -f "${RAW_DIR}/pooled_bootstrap_trial_${trial4}.json" ]]; then
      missing="${missing} ${trial}"
    fi
  done
  missing="${missing# }"
fi

if [[ -z "${missing// }" ]]; then
  echo "No missing trials for ${SCENARIO}."
else
  echo "Rerunning missing ${SCENARIO} trials: ${missing}"
fi

rerun_one_trial() {
  local trial="$1"
  local trial4
  trial4="$(printf "%04d" "${trial}")"
  local tmp="${TMP_ROOT}/trial_${trial4}"
  local tmp_project="/project/${tmp#${REPO}/}"
  local config_project="/project/${CONFIG_DIR#${REPO}/}"
  local trial_data_project="/project/${TRIAL_DATA_ROOT#${REPO}/}"
  local src="${tmp}/raw/pooled_bootstrap_trial_${trial4}.json"
  local dst="${RAW_DIR}/pooled_bootstrap_trial_${trial4}.json"

  rm -rf "${tmp}"
  mkdir -p "${tmp}/logs"

  local status=0
  timeout --kill-after=60s "${TIMEOUT_SEC}s" \
    docker run --rm \
      --cpus="${CONTAINER_CPUS}" \
      --user "$(id -u):$(id -g)" \
      -e HOME=/tmp \
      -e OMP_NUM_THREADS=1 \
      -e OPENBLAS_NUM_THREADS=1 \
      -e MKL_NUM_THREADS=1 \
      -v "${REPO}:/project" \
      -w /project \
      "${IMAGE}" \
      bash -lc "
        Rscript /project/programs/entrypoints/run_pooled_bootstrap.R \
          --scenario=${SCENARIO} \
          --config=${config_project}/${SCENARIO}_bootstrap_centralized.json \
          --trial-root=${trial_data_project}/${SCENARIO}/bootstrap \
          --output-root=${tmp_project} \
          --trial-start=${trial} \
          --trial-end=${trial} \
          --workers=1 \
          --maxit=${MAXIT} \
          --pgtol=${PGTOL} \
          --factr=${FACTR}
      " > "${tmp}/logs/run.log" 2>&1 || status=$?

  if [[ -s "${src}" ]]; then
    cp "${src}" "${dst}"
    echo "trial ${trial4}: wrote result"
  else
    docker run --rm \
      --user "$(id -u):$(id -g)" \
      -e SCENARIO="${SCENARIO}" \
      -v "${REPO}:/project" \
      -w /project \
      "${IMAGE}" \
      Rscript "${SCRIPT_PROJECT}/write_failed_marker.R" "${trial}" "${status}" "${TIMEOUT_SEC}" "/project/${dst#${REPO}/}"
    echo "trial ${trial4}: wrote failed marker"
  fi
}

export REPO ROOT OUT CONFIG_DIR TRIAL_DATA_ROOT SCENARIO IMAGE TIMEOUT_SEC PARALLEL CONTAINER_CPUS
export TRIAL_START TRIAL_END MAXIT PGTOL FACTR RAW_DIR CANON_OUT TMP_ROOT SCRIPT_DIR SCRIPT_PROJECT
export -f rerun_one_trial

if [[ -n "${missing// }" ]]; then
  printf "%s\n" ${missing} | xargs -r -n 1 -P "${PARALLEL}" bash -c 'rerun_one_trial "$1"' _
fi

docker run --rm \
  --user "$(id -u):$(id -g)" \
  -e SCENARIO="${SCENARIO}" \
  -e CANON_OUT="/project/${CANON_OUT#${REPO}/}" \
  -e RAW_DIR="/project/${RAW_DIR#${REPO}/}" \
  -e TRIAL_START="${TRIAL_START}" \
  -e TRIAL_END="${TRIAL_END}" \
  -e MAXIT="${MAXIT}" \
  -e PGTOL="${PGTOL}" \
  -e FACTR="${FACTR}" \
  -v "${REPO}:/project" \
  -w /project \
  "${IMAGE}" \
  Rscript "${SCRIPT_PROJECT}/aggregate_centralized_bootstrap.R"
