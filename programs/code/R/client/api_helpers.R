# Shared helpers for client-side API handlers.
suppressPackageStartupMessages({
  library(jsonlite)
})

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})
fedpoppk_source("R/common/utils.R")
fedpoppk_source("R/common/constants.R")

.parse_json_payload <- function(req, simplify = FALSE) {
  tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = simplify),
    error = function(e) NULL
  )
}

.normalize_param_vector <- function(p_in, param_names) {
  if (is.null(p_in)) {
    return(list(ok = FALSE, error = "missing 'p'"))
  }
  if (is.null(param_names) || !length(param_names)) {
    return(list(ok = FALSE, error = "parameter specification is not initialized"))
  }

  if (is.list(p_in)) {
    p_vec <- tryCatch(unlist(p_in, use.names = TRUE), error = function(e) NULL)
    if (is.null(p_vec) || is.null(names(p_vec)) || any(!nzchar(names(p_vec)))) {
      return(list(ok = FALSE, error = "parameter vector must be named"))
    }
  } else {
    p_vec <- suppressWarnings(as.numeric(p_in))
    if (!length(p_vec)) {
      return(list(ok = FALSE, error = "bad `p`"))
    }
    names(p_vec) <- param_names[seq_along(p_vec)]
  }

  missing <- setdiff(param_names, names(p_vec))
  if (length(missing)) {
    return(list(
      ok = FALSE,
      error = paste0("missing parameters: ", paste(missing, collapse = ", "))
    ))
  }

  p_vec <- suppressWarnings(as.numeric(p_vec[param_names]))
  if (any(!is.finite(p_vec))) {
    return(list(ok = FALSE, error = "parameter vector contains non-finite values"))
  }
  names(p_vec) <- param_names
  storage.mode(p_vec) <- "double"

  list(ok = TRUE, value = p_vec)
}

.format_obj_grad <- function(ans, objf_penalty = fedpoppk_const("penalty_objf", 1e15), include_ok = FALSE) {
  obj_val <- suppressWarnings(as.numeric(ans$objf))
  if (!length(obj_val) || !is.finite(obj_val)) {
    obj_val <- objf_penalty
  }

  grad_list <- NULL
  if (!is.null(ans$grad)) {
    grad_vec <- suppressWarnings(as.numeric(ans$grad))
    if (length(grad_vec) == length(ans$grad) && all(is.finite(grad_vec))) {
      names(grad_vec) <- names(ans$grad)
      grad_list <- lapply(grad_vec, jsonlite::unbox)
      names(grad_list) <- names(grad_vec)
    }
  }

  out <- list(
    objf = jsonlite::unbox(obj_val),
    grad = grad_list
  )
  if (isTRUE(include_ok)) out$ok <- TRUE
  out
}
