#!/usr/bin/env Rscript
# server_main.R

suppressPackageStartupMessages({
  library(optparse)
  library(jsonlite)
  library(logger)
})

# Define command‐line options
option_list <- list(
  make_option(c("-c","--config"),
              type    = "character",
              help    = "Path to config JSON",
              metavar = "FILE"),
  make_option(c("-m","--max-iter"),
              type    = "integer",
              default = NA,
              help    = "Override maxeval",
              metavar = "N")
)

# Parse arguments
opt_parser <- OptionParser(option_list = option_list)
opts       <- parse_args(opt_parser)

if (is.null(opts$config) || !nzchar(opts$config)) {
  stop("Please specify --config <path>")
}

# Load internal modules
source("/project/R/server_comm.R")
source("/project/R/server_objective.R")
source("/project/R/server_optimize.R")
source("/project/R/server_run.R")

# Run
log_info("Starting server run with config={config}", config = opts$config)
res <- run_server_from_config(opts$config, opts$`max-iter`)

# Emit JSON result to stdout
cat(
  toJSON(res, auto_unbox = TRUE, digits = 10),
  "\n"
)
