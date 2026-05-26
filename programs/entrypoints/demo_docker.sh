#!/usr/bin/env bash
set -euo pipefail

IMAGE="${IMAGE:-fedpoppk/deferiprone:20260521}"
SCENARIO="${SCENARIO:-scenario1}"
DEMO_MAXIT="${DEMO_MAXIT:-1}"
DEMO_PGTOL="${DEMO_PGTOL:-1000000}"
export DEMO_MAXIT
export DEMO_PGTOL

if [[ "${SCENARIO}" != "scenario1" && "${SCENARIO}" != "scenario2" ]]; then
  echo "SCENARIO must be scenario1 or scenario2" >&2
  exit 2
fi

if ! docker image inspect "${IMAGE}" >/dev/null 2>&1; then
  cat >&2 <<EOF
Docker image not found: ${IMAGE}

Build it first:
  docker build -f programs/code/Dockerfile.base -t ${IMAGE} programs/code
EOF
  exit 2
fi

DEMO_ROOT="results/demo/${SCENARIO}_federated"
CONFIG_DIR="${DEMO_ROOT}/configs/resolved"
COMPOSE_FILE="${DEMO_ROOT}/compose.yml"
LOG_FILE="${DEMO_ROOT}/compose.log"
PROJECT_NAME="fedpoppk_demo_${SCENARIO}"
HOST_UID="$(id -u)"
HOST_GID="$(id -g)"
PROJECT_ROOT="$(pwd -P)"

mkdir -p "${CONFIG_DIR}" "${DEMO_ROOT}/runs" "${DEMO_ROOT}/cache/client1" "${DEMO_ROOT}/cache/client2" "${DEMO_ROOT}/cache/client3"
cp programs/configs/resolved/*.json "${CONFIG_DIR}/"

docker run --rm \
  --user "${HOST_UID}:${HOST_GID}" \
  -e DEMO_MAXIT="${DEMO_MAXIT}" \
  -e DEMO_PGTOL="${DEMO_PGTOL}" \
  -v "${PROJECT_ROOT}:/project" \
  -w /project \
  "${IMAGE}" \
  Rscript -e '
    suppressPackageStartupMessages(library(jsonlite))
    args <- commandArgs(trailingOnly = TRUE)
    config_path <- args[[1]]
    demo_root <- args[[2]]
    payload <- fromJSON(config_path, simplifyVector = FALSE)
    if (is.null(payload$logging)) payload$logging <- list()
    payload$logging$base_dir <- paste0("/project/", demo_root, "/runs")
    payload$logging$scenario <- "raw_runs"
    if (is.null(payload$optimControl)) payload$optimControl <- list()
    payload$optimControl$maxit <- as.integer(Sys.getenv("DEMO_MAXIT", "1"))
    payload$optimControl$pgtol <- as.numeric(Sys.getenv("DEMO_PGTOL", "1000000"))
    write_json(payload, config_path, auto_unbox = TRUE, pretty = TRUE)
  ' "/project/${CONFIG_DIR}/${SCENARIO}_standard.json" "${DEMO_ROOT}"

cat > "${COMPOSE_FILE}" <<YAML
services:
  client1:
    image: ${IMAGE}
    user: "${HOST_UID}:${HOST_GID}"
    working_dir: /project
    entrypoint: ["Rscript", "-e", "pr <- plumber::plumb('/project/apps/client/plumber.R'); pr\$\$run(host='0.0.0.0', port=8000)"]
    environment:
      PORT: 8000
      CLIENT_MODE: standard
      HOME: /tmp
      GRAD_CORES: 1
      RENV_PROJECT: /project
      RENV_PATHS_ROOT: /tmp/renv
      OMP_NUM_THREADS: 1
      OPENBLAS_NUM_THREADS: 1
      MKL_NUM_THREADS: 1
      VECLIB_MAXIMUM_THREADS: 1
    volumes:
      - ${PROJECT_ROOT}/programs/code/apps:/project/apps:ro
      - ${PROJECT_ROOT}/programs/code/R:/project/R:ro
      - ${PROJECT_ROOT}/data:/project/data:ro
      - ${PROJECT_ROOT}/${DEMO_ROOT}/cache/client1:/project/cache

  client2:
    image: ${IMAGE}
    user: "${HOST_UID}:${HOST_GID}"
    working_dir: /project
    entrypoint: ["Rscript", "-e", "pr <- plumber::plumb('/project/apps/client/plumber.R'); pr\$\$run(host='0.0.0.0', port=8000)"]
    environment:
      PORT: 8000
      CLIENT_MODE: standard
      HOME: /tmp
      GRAD_CORES: 1
      RENV_PROJECT: /project
      RENV_PATHS_ROOT: /tmp/renv
      OMP_NUM_THREADS: 1
      OPENBLAS_NUM_THREADS: 1
      MKL_NUM_THREADS: 1
      VECLIB_MAXIMUM_THREADS: 1
    volumes:
      - ${PROJECT_ROOT}/programs/code/apps:/project/apps:ro
      - ${PROJECT_ROOT}/programs/code/R:/project/R:ro
      - ${PROJECT_ROOT}/data:/project/data:ro
      - ${PROJECT_ROOT}/${DEMO_ROOT}/cache/client2:/project/cache

  client3:
    image: ${IMAGE}
    user: "${HOST_UID}:${HOST_GID}"
    working_dir: /project
    entrypoint: ["Rscript", "-e", "pr <- plumber::plumb('/project/apps/client/plumber.R'); pr\$\$run(host='0.0.0.0', port=8000)"]
    environment:
      PORT: 8000
      CLIENT_MODE: standard
      HOME: /tmp
      GRAD_CORES: 1
      RENV_PROJECT: /project
      RENV_PATHS_ROOT: /tmp/renv
      OMP_NUM_THREADS: 1
      OPENBLAS_NUM_THREADS: 1
      MKL_NUM_THREADS: 1
      VECLIB_MAXIMUM_THREADS: 1
    volumes:
      - ${PROJECT_ROOT}/programs/code/apps:/project/apps:ro
      - ${PROJECT_ROOT}/programs/code/R:/project/R:ro
      - ${PROJECT_ROOT}/data:/project/data:ro
      - ${PROJECT_ROOT}/${DEMO_ROOT}/cache/client3:/project/cache

  server:
    image: ${IMAGE}
    user: "${HOST_UID}:${HOST_GID}"
    working_dir: /project
    depends_on:
      - client1
      - client2
      - client3
    entrypoint: ["Rscript", "/project/apps/server/main.R", "--config", "/project/${CONFIG_DIR}/${SCENARIO}_standard.json"]
    environment:
      HOME: /tmp
      RENV_PROJECT: /project
      RENV_PATHS_ROOT: /tmp/renv
      SCENARIO: ${SCENARIO}
      SERVER_POST_CORES: 1
      OMP_NUM_THREADS: 1
      OPENBLAS_NUM_THREADS: 1
      MKL_NUM_THREADS: 1
      VECLIB_MAXIMUM_THREADS: 1
    volumes:
      - ${PROJECT_ROOT}/programs/code/apps:/project/apps:ro
      - ${PROJECT_ROOT}/programs/code/R:/project/R:ro
      - ${PROJECT_ROOT}/programs:/project/programs:ro
      - ${PROJECT_ROOT}/data:/project/data:ro
      - ${PROJECT_ROOT}/results:/project/results
YAML

cleanup() {
  docker compose -p "${PROJECT_NAME}" -f "${COMPOSE_FILE}" down >/dev/null 2>&1 || true
}
trap cleanup EXIT

docker compose -p "${PROJECT_NAME}" -f "${COMPOSE_FILE}" up --abort-on-container-exit --exit-code-from server > "${LOG_FILE}" 2>&1

latest_run="$(find "${DEMO_ROOT}/runs" -mindepth 1 -maxdepth 4 -type d -name 'run_*' | sort | tail -n 1)"
if [[ -z "${latest_run}" ]]; then
  echo "Federated demo finished but no run directory was found. See ${LOG_FILE}" >&2
  exit 1
fi

echo "Federated demo completed."
echo "Run directory: ${latest_run}"
echo "Result: ${latest_run}/result.json"
echo "Log: ${LOG_FILE}"
