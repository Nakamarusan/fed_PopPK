# api_contract.R
# Runtime request contract checks for client /init and /run endpoints.

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

fedpoppk_source("R/common/utils.R")

.is_scalar_flag <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

client_validate_init_payload <- function(payload) {
  if (is.null(payload) || !is.list(payload)) {
    stop("init payload must be a JSON object")
  }
  if (is.null(payload$modelInfo) || !is.list(payload$modelInfo)) {
    stop("init payload requires object field 'modelInfo'")
  }
  if (is.null(payload$initPar) && is.null(payload$paramSpec) && is.null(payload$parameters)) {
    stop("init payload requires 'initPar' or 'paramSpec/parameters'")
  }
  if (is.null(payload$dataPath) || !nzchar(as.character(payload$dataPath[[1L]] %||% ""))) {
    stop("init payload requires non-empty 'dataPath'")
  }
  if (!is.null(payload$return_grad) && !.is_scalar_flag(payload$return_grad)) {
    stop("'return_grad' must be boolean when provided")
  }
  invisible(TRUE)
}

client_validate_run_payload <- function(payload) {
  if (is.null(payload) || !is.list(payload)) {
    stop("run payload must be a JSON object")
  }
  if (is.null(payload$p)) {
    stop("run payload requires field 'p'")
  }
  if (!is.null(payload$return_grad) && !.is_scalar_flag(payload$return_grad)) {
    stop("'return_grad' must be boolean when provided")
  }
  if (!is.null(payload$richardson_eps)) {
    eps <- suppressWarnings(as.numeric(payload$richardson_eps[[1L]]))
    if (!is.finite(eps) || eps <= 0) {
      stop("'richardson_eps' must be a positive number when provided")
    }
  }
  if (!is.null(payload$resample) && !is.list(payload$resample)) {
    stop("'resample' must be an object when provided")
  }
  invisible(TRUE)
}
