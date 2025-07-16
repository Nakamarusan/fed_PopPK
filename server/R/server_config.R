# R/server_config.R

# Required packages:
# install.packages(c("optparse","jsonlite","jsonvalidate","logger"))
library(optparse)
library(jsonlite)
library(jsonvalidate)
library(logger)

# Path to the JSON schema for validation (adjust if needed)
SCHEMA_FILE <- "R/server_config_schema.json"

#── Command‐line options definition ──────────────────────────────────────────
.option_list <- list(
  make_option(c("-c", "--config"),
              type    = "character",
              default = NULL,
              help    = "Path to the server configuration JSON file",
              metavar = "FILE"),
  make_option(c("-m", "--max-iter"),
              type    = "integer",
              default = 100,
              help    = "Maximum number of federation iterations [default %default]",
              metavar = "INT")
)

#' Parse command-line options
#'
#' @return A list with elements:
#'   - config_path: path to JSON config
#'   - max_iter   : maximum number of iterations
parse_server_args <- function() {
  parser <- OptionParser(option_list = .option_list)
  opts   <- parse_args(parser)
  if (is.null(opts$config)) {
    stop("Error: --config <FILE> is required.")
  }
  list(
    config_path = opts$config,
    max_iter    = opts$`max-iter`
  )
}

#' Load and validate the server configuration file
#'
#' @param path Path to JSON config file
#' @return A named list with the configuration
load_server_config <- function(path) {
  if (!file.exists(path)) {
    stop("Configuration file not found: ", path)
  }

  # Parse JSON
  cfg <- fromJSON(path, simplifyVector = TRUE)

  # Validate against schema, if available
  if (file.exists(SCHEMA_FILE)) {
    validator <- json_validator(SCHEMA_FILE)
    if (!validator(path)) {
      stop("Configuration JSON failed schema validation: ", path)
    }
    log_info("Configuration JSON passed schema validation.")
  } else {
    log_warn("Schema file not found ({}). Skipping JSON schema validation.", SCHEMA_FILE)
  }

  # Check required fields
  required <- c("clients", "initPar")
  missing  <- setdiff(required, names(cfg))
  if (length(missing) > 0) {
    stop("Missing required fields in configuration: ", paste(missing, collapse = ", "))
  }

  cfg
}

#' Parse CLI args and load server config
#'
#' @return A list with:
#'   - clients      : vector of client URLs
#'   - initPar      : named list of initial parameters
#'   - optimControl : optional optimizer settings (from JSON)
#'   - max_iter     : maximum iterations (from CLI)
parse_and_load_config <- function() {
  args <- parse_server_args()
  cfg  <- load_server_config(args$config_path)

  list(
    clients      = cfg$clients,
    initPar      = cfg$initPar,
    optimControl = cfg$optimControl %||% list(),
    max_iter     = args$max_iter
  )
}