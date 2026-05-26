# param_spec.R
# Normalizes parameter specifications received by client APIs.
# Supports both explicit paramSpec and legacy initPar/lower/upper payloads.
local({
  candidates <- c("/project/R/common/utils.R", "R/common/utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

.scalar_num <- function(x, default) {
  if (is.null(x)) return(default)
  if (is.list(x) && length(x) == 0L) return(default)
  vals <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (!length(vals)) return(default)
  if (is.na(vals[[1L]])) return(default)
  vals[[1L]]
}

.first_chr <- function(x, default = "") {
  if (is.null(x)) return(default)
  if (is.list(x) && length(x) == 0L) return(default)
  vals <- as.character(unlist(x, use.names = FALSE))
  if (!length(vals)) return(default)
  vals[[1L]]
}

.default_transform <- function(lower, upper, role = NULL) {
  if (is.finite(lower) && is.finite(upper)) return("logit")
  if (identical(role, "eta") || identical(role, "residual")) {
    if (is.finite(lower) && !is.finite(upper)) return("lower_exp")
    if (!is.finite(lower) && !is.finite(upper)) return("exp")
  }
  if (is.finite(lower) && !is.finite(upper)) return("lower_exp")
  "identity"
}

.infer_role <- function(param_name) {
  if (grepl("^eta", param_name)) return("eta")
  if (grepl("Sd$", param_name) || grepl("PropSd|AddSd", param_name)) return("residual")
  "fixed"
}

.normalize_param_list <- function(param_list) {
  if (!is.list(param_list) || !length(param_list)) {
    stop("paramSpec must be a non-empty list")
  }
  names_vec <- vapply(param_list, function(x) x$name %||% "", character(1))
  if (any(!nzchar(names_vec))) stop("every parameter needs a non-empty name")
  if (any(duplicated(names_vec))) stop("parameter names must be unique")

  init_vals <- vapply(param_list, function(x) .scalar_num(x$init, NA_real_), numeric(1))
  if (any(!is.finite(init_vals))) stop("each parameter must specify a finite init value")

  lower_vals <- vapply(param_list, function(x) .scalar_num(x$lower, -Inf), numeric(1))
  upper_vals <- vapply(param_list, function(x) .scalar_num(x$upper, Inf), numeric(1))

  role_vals <- vapply(param_list, function(x) {
    role_chr <- .first_chr(x$role, default = "")
    if (is.na(role_chr) || !nzchar(role_chr)) return(NA_character_)
    role_chr
  }, character(1))
  for (i in seq_along(role_vals)) {
    if (is.na(role_vals[[i]]) || !nzchar(role_vals[[i]])) role_vals[[i]] <- .infer_role(names_vec[[i]])
  }

  transform_vals <- vapply(param_list, function(x) {
    tr_chr <- .first_chr(x$transform, default = "")
    if (is.na(tr_chr) || !nzchar(tr_chr)) return(NA_character_)
    tr_chr
  }, character(1))
  for (i in seq_along(transform_vals)) {
    if (is.na(transform_vals[[i]]) || !nzchar(transform_vals[[i]])) {
      transform_vals[[i]] <- .default_transform(lower_vals[[i]], upper_vals[[i]], role = role_vals[[i]])
    }
  }

  list(
    names = names_vec,
    init = setNames(init_vals, names_vec),
    lower = setNames(lower_vals, names_vec),
    upper = setNames(upper_vals, names_vec),
    transform = setNames(transform_vals, names_vec),
    role = setNames(role_vals, names_vec)
  )
}

.param_spec_from_payload <- function(payload) {
  if (!is.null(payload$paramSpec)) {
    spec <- .normalize_param_list(payload$paramSpec)
    omega_blocks <- payload$omegaBlocks %||% payload$omega_blocks %||% NULL
    if (!is.null(omega_blocks) && length(omega_blocks)) {
      spec$omega_blocks <- omega_blocks
    }
    return(spec)
  }
  init_par <- payload$initPar
  if (is.null(init_par)) stop("initPar is required when paramSpec is missing")
  init_vec <- unlist(init_par, use.names = TRUE)
  storage.mode(init_vec) <- "double"
  if (is.null(names(init_vec)) || any(!nzchar(names(init_vec)))) {
    stop("initPar must be a named numeric vector")
  }
  lower_cfg <- payload$lower %||% list()
  upper_cfg <- payload$upper %||% list()
  param_list <- lapply(names(init_vec), function(nm) {
    list(
      name = nm,
      init = init_vec[[nm]],
      lower = lower_cfg[[nm]] %||% -Inf,
      upper = upper_cfg[[nm]] %||% Inf,
      transform = NA_character_,
      role = .infer_role(nm)
    )
  })
  spec <- .normalize_param_list(param_list)
  omega_blocks <- payload$omegaBlocks %||% payload$omega_blocks %||% NULL
  if (!is.null(omega_blocks) && length(omega_blocks)) {
    spec$omega_blocks <- omega_blocks
  }
  spec
}
