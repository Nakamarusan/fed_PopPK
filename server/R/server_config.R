# server/R/server_config.R

library(optparse)
library(jsonlite)
library(jsonvalidate)
library(logger)

.option_list <- list(
  make_option(c("-c", "--config"),   type = "character",
              help = "Path to server‑side JSON config", metavar = "FILE"),
  make_option(c("-m", "--max-iter"), type = "integer", default = 100,
              help = "Max optimisation iterations [default %default]",
              metavar = "INT")
)

parse_server_args <- function() {
  opts <- parse_args(OptionParser(option_list = .option_list))
  if (is.null(opts$config))
    stop("Error: --config <FILE> is required.")
  list(config_path = opts$config, max_iter = opts$`max-iter`)
}

load_server_config <- function(path) {
  if (!file.exists(path)) stop("Config file not found: ", path)
  cfg <- fromJSON(path, simplifyVector = TRUE)
  required <- c("clients", "initPar", "modelInfo")
  miss <- setdiff(required, names(cfg))
  if (length(miss))
    stop("Missing field(s) in configuration: ", paste(miss, collapse = ", "))
  cfg
}

#' パース＆ロード
#' @return list(
#'   clients: named character vector (URL → dataPath),
#'   initPar: named list,
#'   modelInfo: list,
#'   optimControl: list,
#'   max_iter: integer
#' )
parse_and_load_config <- function() {
  args <- parse_server_args()
  cfg  <- load_server_config(args$config_path)
  str(cfg$clients)
  # ── clients を named character vector に変換 ──────────────
  clients_vec <- unlist(cfg$clients, use.names = TRUE)
  print(clients_vec)

  list(
    clients      = clients_vec,
    initPar      = cfg$initPar,
    modelInfo    = cfg$modelInfo,
    optimControl = cfg$optimControl %||% list(),
    max_iter     = args$max_iter
  )
}
