# server_run.R -------------------------------------------------
suppressPackageStartupMessages({
  library(jsonlite)
  library(logger)
  library(purrr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# RPC ヘルパー
source("server_comm.R",       chdir = TRUE)
# 目的関数集約
source("server_objective.R",  chdir = TRUE)
# 全体最適化ループ
source("server_optimize.R",   chdir = TRUE)
source("server_objective.R",   chdir = TRUE) 
#' JSON 設定を読み込んでフェデレーション最適化を実行
#'
#' @param config_path Path to the JSON file from orchestrator
run_server_from_config <- function(config_path,
                                   max_iter_override = NULL) {

  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  cfg <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)

  # 必須チェック
  required <- c("clients", "modelInfo", "initPar")
  miss <- setdiff(required, names(cfg))
  if (length(miss)) {
    stop("Config missing field(s): ", paste(miss, collapse = ", "))
  }

  # maxeval 上書き
  cfg$optimControl <- cfg$optimControl %||% list()
  if (!is.null(max_iter_override)) {
    cfg$optimControl$maxeval <- as.integer(max_iter_override)
  }

  # 1) 初期化 RPC
  log_info("Initializing %d clients …", length(cfg$clients))
  send_init(
    client_map = cfg$clients,
    modelInfo  = cfg$modelInfo,
    initPar    = cfg$initPar
  )

  # 2) 全体最適化ループ
  result <- server_optimize(
    init_par    = unlist(cfg$initPar),     # named numeric
    client_map  = cfg$clients,             # named character
    common_info = list(modelInfo = cfg$modelInfo),
    opts        = cfg$optimControl,
    comm_fn     = poll_clients,
    agg_fn      = aggregate_responses
  )

  log_info("Optimisation finished (status=%d)", result$convergence)
  result
}

# CLI 実行時
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1) stop("Usage: server_run.R <config.json>")
  out <- run_server_from_config(args[1])
  cat(jsonlite::toJSON(out, auto_unbox = TRUE, digits = 10))
}
