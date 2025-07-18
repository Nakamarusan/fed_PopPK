# R/server_optimize.R

# Dependencies
# install.packages(c("nloptr","logger"))
library(nloptr)
library(logger)
#' Run federated optimization on server
#'
#' @param init_par  Numeric vector of initial parameters (unconstrained)
#' @param client_urls Character vector of client endpoints
#' @param payload_base Named list of other fields to include in payload (e.g. modelInfo)
#' @param opts      List of nloptr options (see ?nloptr::nloptr)
#' @param comm_fn   Function to call clients (default poll_clients)
#' @param agg_fn    Function to aggregate responses (default aggregate_responses)
#' @return A list with
#'   - par     : optimized parameters
#'   - value   : final objective
#'   - convergence : status code from optimizer
server_optimize <- function(init_par,
                            client_urls,
                            payload_base = list(),
                            opts = list(
                              algorithm    = "NLOPT_LD_LBFGS",
                              print_level  = 5,
                              maxeval      = 100,
                              xtol_rel     = 1e-6
                            ),
                            comm_fn = poll_clients,
                            agg_fn  = aggregate_responses) {
  log_info("iter {it}: calling clients with p={paste(round(p,3),collapse=',')}")

  # wrapper for nloptr
  eval_grad_fn <- function(p) {
    # build payload with parameters
    payload <- c(
      payload_base,
      list(p = p)
    )

    # 1) obtain list of {objf, grad} from clients
    responses <- comm_fn(client_urls, payload,
                         timeout   = opts$timeout     %||% 30,
                         max_tries = opts$max_tries   %||% 3,
                         pause     = opts$pause       %||% 1)

    # 2) aggregate
    agg    <- agg_fn(responses)
    obj    <- as.numeric(agg$objf)
    grad   <- as.numeric(agg$grad)
    stopifnot(is.double(obj),  is.finite(obj))
    stopifnot(is.double(grad), length(grad) == length(p))

    # nloptr expects list( objective, gradient )
    list("objective" = obj, "gradient" = grad)
  }

  # call nloptr
  res <- nloptr::nloptr(
    x0         = as.numeric(init_par),
    eval_f     = function(x) eval_grad_fn(x)$objective,
    eval_grad_f= function(x) eval_grad_fn(x)$gradient,
    opts       = opts
  )

  list(
    par         = res$solution,
    value       = res$objective,
    convergence = res$status
  )
}
