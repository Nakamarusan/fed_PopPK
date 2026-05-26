#!/usr/bin/env bash
set -euo pipefail

REPO="/project"
ROOT="${REPO}/temp/deferiprone_single_oral_25mgkg_designcheck_20260511"
OUT="${ROOT}/validation/design_change_n403030_wtmclustT10_nomix_site403030_nondesignquota_precisionchol_basefitbootinit_preboot_20260521"
BOOT_INIT="${OUT}/configs/resolved/base_bootstrap_init_from_centralized.json"

cd "${REPO}"

echo "[check] bootstrap init config: ${BOOT_INIT}"
jq -e '
  .optimControl.maxit == 100 and
  .optimControl.pgtol == 0 and
  .optimControl.factr == 1000000000 and
  .omegaBlocks[0].parameterization == "precision_chol"
' "${BOOT_INIT}" >/dev/null

echo "[cleanup] previous federated bootstrap outputs in target folder"
rm -rf \
  "${OUT}/runs/scenario1/bootstrap/federated" \
  "${OUT}/runs/scenario2/bootstrap/federated" \
  "${OUT}/cache/scenario1" \
  "${OUT}/cache/scenario2" \
  "${OUT}/compose.bootstrap.cache.yml"

docker network ls --format "{{.Name}}" \
  | rg "^defboot_design_change.*scenario[12]" \
  | xargs -r docker network rm || true

for SCENARIO in scenario1 scenario2; do
  echo "[start] federated bootstrap: ${SCENARIO}"
  mkdir -p "${OUT}/runs/${SCENARIO}/bootstrap/federated/logs"

  DEFERIPRONE_OUTPUT_ROOT="${OUT}" \
  BOOT_BASE_EXTENDS=base_bootstrap_init_from_centralized.json \
  START_AT=1 \
  END_AT=500 \
  CHUNK_SIZE=5 \
  B=500 \
  SEED_BASE=4869 \
  RUN_TIMEOUT=900 \
  CLIENT1_CPUS=6 \
  CLIENT2_CPUS=6 \
  CLIENT3_CPUS=6 \
  SERVER_CPUS=3 \
  CLIENT1_GRAD_CORES=6 \
  CLIENT2_GRAD_CORES=6 \
  CLIENT3_GRAD_CORES=6 \
  SERVER_POST_CORES=3 \
  bash "${ROOT}/run_bootstrap_chunked.sh" "${SCENARIO}" \
    2>&1 | tee "${OUT}/runs/${SCENARIO}/bootstrap/federated/logs/run_bootstrap_chunked_${SCENARIO}.top.log"

  echo "[done] federated bootstrap: ${SCENARIO}"
done

echo "[complete] federated bootstrap scenario1 and scenario2"
