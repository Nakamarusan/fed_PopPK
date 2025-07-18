# client/R/client_optimize.R
# ── 依存パッケージ ─────────────────────────────────────────────
suppressPackageStartupMessages({
  library(jsonlite)
  library(data.table)
})

# ── 共通ユーティリティ読み込み ────────────────────────────────
# data_loader.R        : load_data()
# construct_model_from_JSON.R : construct_model_from_JSON()
# compute_obj_grad.R   : compute_obj_grad()
# run_federated_optimization.R: run_federated_optimization()
source("data_loader.R",               chdir = TRUE)
source("construct_model_from_JSON.R", chdir = TRUE)
source("compute_obj_grad.R",          chdir = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x   # rlang が無い環境向け

#' Run full optimisation **inside the container**  
#' サーバー側から渡された JSON ファイルを 1 つ読み込んで最適化を実行。
#' @param config_path Character. Path to config JSON.
#' @return list(par, value, convergence, history)
client_optimize <- function(config_path){
  # 1) 設定読み込み --------------------------------------------------------
  cfg <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)

  # 2) データ読み込み ------------------------------------------------------
  dt  <- load_data(list(dataPath = cfg$dataPath))

  # 3) JSON の modelInfo をそのまま渡す --------------------------------------
  model_info <- cfg$modelInfo

  # 4) 初期値ベクトル（**名前付き**） -------------------------------------
  init_par <- unlist(cfg$initPar)
  if (is.null(names(init_par)) || any(names(init_par) == ""))
    stop("initPar must be a *named* numeric list in JSON")

  # 5) 最適化 --------------------------------------------------------------
  #    run_federated_optimization() の中で
  #      ・compute_obj_grad(p, model_info, dt) を使って
  #      ・数値勾配付きの L-BFGS-B 最適化を回します
  res <- run_federated_optimization(
           init_par    = init_par,
           model_info  = model_info,
           dt          = dt,
           control     = cfg$optimControl %||% list(maxit = 200)
         )

  # 6) 返却 ---------------------------------------------------------------
  list(
    par         = res$opt$par,
    value       = res$opt$value,
    convergence = res$opt$convergence,
    history     = res$history
  )
}

# ── CLI 用 ────────────────────────────────────────────────────
if (!interactive()){
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1)
    stop("Usage: client_optimize.R <config.json>")
  out <- client_optimize(args[1])
  cat(jsonlite::toJSON(out, auto_unbox = TRUE, digits = 10))
}
