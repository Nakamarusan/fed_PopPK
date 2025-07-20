# server_main.R

# ① 通信ヘルパー＆設定パーサ読込
# RPC ヘルパー
source("server_comm.R",       chdir = TRUE)
# 目的関数集約
source("server_objective.R",  chdir = TRUE)
# 全体最適化ループ
source("server_optimize.R",   chdir = TRUE)
source("server_objective.R",   chdir = TRUE) 
source("server_config.R",   chdir = TRUE) 
library(jsonlite)
library(logger)

# ② server.json／CLI から設定をロード
cfg <- parse_and_load_config()
# cfg$clients      : named vector baseURL → dataPath
# cfg$modelInfo    : list(compartment, administration, iiv, res)
# cfg$initPar      : named list of initial parameters
# cfg$optimControl : list(nloptr オプション)
# cfg$max_iter     : integer

# ③ クライアントが立ち上がるまで少し待機
Sys.sleep(5)

# ④ /init を一度だけ実行
send_init(
  client_map = cfg$clients,
  modelInfo  = cfg$modelInfo,
  initPar    = cfg$initPar
)

# ⑤ federated L-BFGS 最適化
res <- server_optimize(
  init_par    = unlist(cfg$initPar),
  client_map  = cfg$clients,
  common_info = list(modelInfo = cfg$modelInfo),
  opts        = cfg$optimControl,
  comm_fn     = poll_clients,
  agg_fn      = aggregate_responses
)

# ⑥ 結果を出力
cat(toJSON(res, auto_unbox = TRUE, digits = 10))
