# client_optimize.R

suppressPackageStartupMessages({
  library(jsonlite)
  library(data.table)
  library(rxode2)
  library(nlmixr2lib)
  library(nlmixr2)
  library(numDeriv)
})

#── 必要なモジュール関数を読み込む ───────────────────────────────
source("R/json_parser.R",               chdir = TRUE)
source("R/data_loader.R",               chdir = TRUE)
source("R/construct_model_from_JSON.R", chdir = TRUE)
source("R/update_model_params.R",       chdir = TRUE)
source("R/compute_obj_grad.R",          chdir = TRUE)
source("R/run_federated_optimization.R", chdir = TRUE)

#' Run full client-side optimization and return results as a list
#'
#' @param config_path Path to JSON config file provided by server
#' @return A list with elements:
#'   - par         : named vector of estimated parameters
#'   - value       : final objective function value
#'   - convergence : optim() convergence code
#'   - history     : data.frame of iteration history
client_optimize <- function(config_path) {
  # 1) Read configuration
  cfg_full   <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)
  model_info <- parse_model_json_basic(config_path)
  
  # 2) Load client data
  dt <- load_data(list(dataPath = cfg_full$dataPath))
  
  # 3) Build model (rxUi) based on JSON
  rxUi_mod <- construct_model_from_JSON(model_info)
  
  # 4) Run optimization
  res <- run_federated_optimization(
    init_par = unlist(cfg_full$initPar),
    model    = rxUi_mod,
    dt       = dt,
    control  = cfg_full$optimControl %||% list(maxit = 200)
  )
  
  # 5) Return results
  list(
    par         = res$opt$par,
    value       = res$opt$value,
    convergence = res$opt$convergence,
    history     = res$history
  )
}

#── CLI 用フック: スクリプトとして実行された場合のみ動作 ─────────────────
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1) {
    stop("Usage: client_optimize.R <config.json>")
  }
  result <- client_optimize(args[1])
  cat(jsonlite::toJSON(result, auto_unbox = TRUE, digits = 10))
}
