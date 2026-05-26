#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${ROOT}/../.." && pwd)"
REL_ROOT="$(realpath --relative-to="${REPO_ROOT}" "${ROOT}")"
IMAGE="${FEDPOPPK_IMAGE:-fedpoppk/base:0.1.0}"

docker run --rm \
  --user "$(id -u):$(id -g)" \
  -e HOME=/tmp \
  -v "${REPO_ROOT}:/project" \
  -w /project \
  "${IMAGE}" \
  Rscript "/project/${REL_ROOT}/generate_data_and_plot.R"
