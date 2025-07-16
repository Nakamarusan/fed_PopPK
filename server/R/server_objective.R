# R/server_objective.R

#' Aggregate client results into global objective and gradient
#'
#' @param responses List of client responses. Each element is a list with
#'   - objf: numeric scalar (–2LL or objective contribution)
#'   - grad: numeric vector of the same length across clients
#' @return A list with
#'   - objf: sum of all clients’ objf
#'   - grad: sum of all clients’ grad (element‐wise)
#' @export
aggregate_responses <- function(responses) {
  if (!is.list(responses) || length(responses) == 0) {
    stop("`responses` must be a non-empty list")
  }

  # Extract all objf values and grads
  obj_vals <- vapply(responses, function(r) {
    if (!is.numeric(r$objf) || length(r$objf) != 1) {
      stop("Each response$objf must be a numeric scalar")
    }
    r$objf
  }, numeric(1))

  grads <- lapply(responses, function(r) {
    if (!is.numeric(r$grad)) {
      stop("Each response$grad must be numeric")
    }
    r$grad
  })

  # Check that all grads have the same length
  lengths <- vapply(grads, length, integer(1))
  if (length(unique(lengths)) != 1) {
    stop("All `grad` vectors must have the same length")
  }

  # Sum them
  objf_sum <- sum(obj_vals)
  grad_sum <- Reduce(`+`, grads)

  list(objf = objf_sum, grad = grad_sum)
}
