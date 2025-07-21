# ① 通信ヘルパー＆設定パーサ読込
source("server_comm.R",       chdir = TRUE)
source("server_objective.R",  chdir = TRUE)
source("server_optimize.R",   chdir = TRUE)
source("server_config.R",     chdir = TRUE)

library(jsonlite)
library(logger)

# 無制約変換関数
transform_init_par <- function(init_par) {
  transformed <- init_par
  
  if (!is.null(init_par[["etaLcl"]])) transformed[["etaLcl"]] <- log(init_par[["etaLcl"]]^2)
  if (!is.null(init_par[["etaLvc"]])) transformed[["etaLvc"]] <- log(init_par[["etaLvc"]]^2)
  if (!is.null(init_par[["CcPropSd"]])) transformed[["CcPropSd"]] <- log(init_par[["CcPropSd"]])
  
  rho_name <- "(etaLcl,etaLvc)"
  if (!is.null(init_par[[rho_name]])) {
    rho <- init_par[[rho_name]]
    if (abs(rho) >= 1) rho <- sign(rho) * 0.999
    transformed[[rho_name]] <- atanh(rho)
  }

  transformed
}

# ② server.json／CLI から設定をロード
cfg <- parse_and_load_config()

# ③ クライアントが立ち上がるまで少し待機
Sys.sleep(5)

# ④ /init を一度だけ実行（制約前スケール → 無制約スケールに修正）
send_init(
  client_map = cfg$clients,
  modelInfo  = cfg$modelInfo,
  initPar    = as.list(transform_init_par(cfg$initPar))  # 修正済み
)

# ⑤ 最適化のため無制約スケールに変換（unlist は optimize 用に必要）
init_par_transformed <- unlist(transform_init_par(cfg$initPar))

# ⑥ federated L-BFGS 最適化
res <- server_optimize(
  init_par    = init_par_transformed,
  client_map  = cfg$clients,
  common_info = list(modelInfo = cfg$modelInfo),
  opts        = cfg$optimControl,
  comm_fn     = poll_clients,
  agg_fn      = aggregate_responses
)

# ⑦ 結果を出力
cat(toJSON(res, auto_unbox = TRUE, digits = 10))
