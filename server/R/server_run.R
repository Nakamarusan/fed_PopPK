# R/server_run.R

#’ Run federated optimization given a config file
#’
#’ @param config_path Path to JSON config file
#’ @param max_iter_override Optional integer to override optimControl$maxeval
#’ @return A list with elements par, value, convergence
#’ @export
run_server_from_config <- function(config_path, max_iter_override = NULL) {
  # 1) Read JSON
  cfg <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)

  # 2) Validate required fields
  required <- c("clients", "initPar", "optimControl")
  miss <- setdiff(required, names(cfg))
  if (length(miss)) {
    stop("Missing required fields: ", paste(miss, collapse = ", "))
  }

  # 3) Override maxeval if requested
  if (!is.null(max_iter_override)) {
    cfg$optimControl$maxeval <- max_iter_override
  }

  # 4) Call the optimization engine
  server_optimize(
    init_par     = as.numeric(unlist(cfg$initPar)),
    client_urls  = cfg$clients,
    payload_base = cfg[setdiff(names(cfg), "clients")],
    opts         = cfg$optimControl
  )
}
