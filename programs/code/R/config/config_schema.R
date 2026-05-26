# config_schema.R
# JSON configuration schema checks used at runtime and CI.

local({
  candidates <- c("/project/R/common/utils.R", "R/common/utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

local({
  candidates <- c("/project/R/common/constants.R", "R/common/constants.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/constants.R not found")
  source(hit[[1L]], chdir = TRUE)
})

.scalar_chr <- function(x, default = "") {
  if (is.null(x)) return(default)
  x <- unlist(x, use.names = FALSE)
  if (!length(x)) return(default)
  as.character(x[[1L]])
}

.scalar_num <- function(x, default = NA_real_) {
  if (is.null(x)) return(default)
  x <- unlist(x, use.names = FALSE)
  if (!length(x)) return(default)
  suppressWarnings(as.numeric(x[[1L]]))
}

.scalar_int <- function(x, default = NA_integer_) {
  if (is.null(x)) return(default)
  x <- unlist(x, use.names = FALSE)
  if (!length(x)) return(default)
  suppressWarnings(as.integer(x[[1L]]))
}

.is_named_map <- function(x) {
  is.list(x) && length(x) > 0L && !is.null(names(x)) && all(nzchar(names(x)))
}

.check_named_numeric <- function(x, label, required = NULL, allow_null = FALSE, allow_infinite = FALSE) {
  if (is.null(x)) {
    if (isTRUE(allow_null)) return(invisible(TRUE))
    stop(label, " must be provided")
  }
  vec <- if (is.list(x)) unlist(x, use.names = TRUE) else x
  if (!is.numeric(vec)) stop(label, " must be numeric")
  if (is.null(names(vec)) || any(!nzchar(names(vec)))) {
    stop(label, " must be a named numeric vector")
  }
  if (!isTRUE(allow_infinite) && any(!is.finite(vec) & !is.na(vec))) {
    stop(label, " contains invalid non-finite values")
  }
  if (!is.null(required)) {
    miss <- setdiff(required, names(vec))
    if (length(miss)) stop(label, " missing names: ", paste(miss, collapse = ", "))
  }
  invisible(TRUE)
}

.normalize_bound_map <- function(x, required_names, default_value) {
  out <- setNames(rep(default_value, length(required_names)), required_names)
  if (is.null(x)) return(out)
  if (is.list(x)) {
    keys <- intersect(names(x), required_names)
    for (nm in keys) {
      val <- .scalar_num(x[[nm]], default_value)
      if (is.na(val)) val <- default_value
      out[[nm]] <- val
    }
    return(out)
  }
  if (is.numeric(x) && !is.null(names(x))) {
    keys <- intersect(names(x), required_names)
    out[keys] <- as.numeric(x[keys])
    out[is.na(out)] <- default_value
    return(out)
  }
  stop("bounds must be an object (named numeric or nullable map)")
}

.check_positive <- function(value, label, integer = FALSE, allow_null = TRUE) {
  if (is.null(value)) {
    if (isTRUE(allow_null)) return(invisible(TRUE))
    stop(label, " must be set")
  }
  v <- if (isTRUE(integer)) .scalar_int(value, NA_integer_) else .scalar_num(value, NA_real_)
  if (!is.finite(v) || v <= 0) stop(label, " must be > 0")
  invisible(TRUE)
}

.check_nonnegative <- function(value, label, integer = FALSE, allow_null = TRUE) {
  if (is.null(value)) {
    if (isTRUE(allow_null)) return(invisible(TRUE))
    stop(label, " must be set")
  }
  v <- if (isTRUE(integer)) .scalar_int(value, NA_integer_) else .scalar_num(value, NA_real_)
  if (!is.finite(v) || v < 0) stop(label, " must be >= 0")
  invisible(TRUE)
}

.validate_clients <- function(cfg) {
  clients <- cfg$clients
  if (!.is_named_map(clients)) {
    stop("clients must be a non-empty object of URL -> dataPath")
  }
  urls <- names(clients)
  if (any(!grepl("^https?://", urls))) {
    stop("all client keys must be HTTP(S) URLs")
  }
  paths <- vapply(clients, function(v) .scalar_chr(v, ""), character(1))
  if (any(!nzchar(paths))) stop("all clients values must be non-empty dataPath strings")
  invisible(TRUE)
}

.validate_model <- function(cfg) {
  model <- cfg$modelInfo
  if (!is.list(model) || !length(model)) stop("modelInfo must be a non-empty object")

  model_name <- .scalar_chr(model$modelName, "")
  if (!nzchar(model_name)) stop("modelInfo.modelName must be non-empty")

  admin <- tolower(.scalar_chr(model$administration %||% "iv", "iv"))
  if (!admin %in% c("iv", "po")) stop("modelInfo.administration must be 'iv' or 'po'")

  res <- tolower(.scalar_chr(model$res %||% "mix", "mix"))
  if (!res %in% c("mix", "add", "prop")) stop("modelInfo.res must be one of: mix, add, prop")
  invisible(TRUE)
}

.validate_parameter_block <- function(cfg) {
  params <- cfg$parameters %||% cfg$paramSpec
  if (!is.null(params)) {
    if (!is.list(params) || !length(params)) stop("parameters/paramSpec must be a non-empty list")
    names_seen <- character()
    for (i in seq_along(params)) {
      row <- params[[i]]
      nm <- .scalar_chr(row$name, "")
      if (!nzchar(nm)) stop("parameter entry ", i, " is missing name")
      if (nm %in% names_seen) stop("duplicated parameter name: ", nm)
      names_seen <- c(names_seen, nm)
      init <- .scalar_num(row$init, NA_real_)
      if (!is.finite(init)) stop("parameter ", nm, " must define finite init")
      lo <- .scalar_num(row$lower, -Inf)
      up <- .scalar_num(row$upper, Inf)
      if (is.finite(lo) && is.finite(up) && lo >= up) {
        stop("parameter ", nm, " has lower >= upper")
      }
    }
    return(invisible(TRUE))
  }

  init <- cfg$initPar %||% cfg$init_par
  .check_named_numeric(init, "initPar", allow_null = FALSE)
  init_vec <- if (is.list(init)) unlist(init, use.names = TRUE) else init
  req <- names(init_vec)
  lo <- .normalize_bound_map(cfg$lower, req, default_value = -Inf)
  up <- .normalize_bound_map(cfg$upper, req, default_value = Inf)
  bad <- req[is.finite(lo) & is.finite(up) & (lo >= up)]
  if (length(bad)) stop("lower >= upper for parameters: ", paste(bad, collapse = ", "))

  invisible(TRUE)
}

.validate_optim <- function(cfg) {
  oc <- cfg$optimControl %||% list()
  method <- .scalar_chr(oc$method %||% "L-BFGS-B", "L-BFGS-B")
  if (!nzchar(method)) stop("optimControl.method must be non-empty")

  .check_positive(oc$maxit %||% fedpoppk_const("optim.maxit", 100L), "optimControl.maxit", integer = TRUE, allow_null = FALSE)
  .check_nonnegative(oc$pgtol %||% fedpoppk_const("optim.pgtol", 5e-3), "optimControl.pgtol", allow_null = FALSE)
  .check_positive(oc$factr %||% fedpoppk_const("optim.factr", 1e8), "optimControl.factr", allow_null = FALSE)

  if (!is.null(oc$grad_cores)) {
    .check_positive(oc$grad_cores, "optimControl.grad_cores", integer = TRUE, allow_null = FALSE)
  }
  if (!is.null(oc$stepmax)) {
    .check_positive(oc$stepmax, "optimControl.stepmax", allow_null = FALSE)
  }

  invisible(TRUE)
}

.validate_comm <- function(cfg) {
  cm <- cfg$comm %||% list()
  .check_positive(cm$init_max_tries %||% fedpoppk_const("comm.init_max_tries", 20L), "comm.init_max_tries", integer = TRUE, allow_null = FALSE)
  .check_positive(cm$init_pause %||% fedpoppk_const("comm.init_pause", 5), "comm.init_pause", allow_null = FALSE)
  .check_positive(cm$init_timeout %||% fedpoppk_const("comm.init_timeout", 300), "comm.init_timeout", allow_null = FALSE)
  .check_positive(cm$run_max_tries %||% fedpoppk_const("comm.run_max_tries", 3L), "comm.run_max_tries", integer = TRUE, allow_null = FALSE)
  .check_positive(cm$run_pause %||% fedpoppk_const("comm.run_pause", 1), "comm.run_pause", allow_null = FALSE)
  .check_positive(cm$run_timeout %||% fedpoppk_const("comm.run_timeout", 300), "comm.run_timeout", allow_null = FALSE)
  invisible(TRUE)
}

.validate_logging <- function(cfg) {
  lg <- cfg$logging %||% list()
  base_dir <- .scalar_chr(lg$base_dir %||% "/project/result/logs", "/project/result/logs")
  if (!nzchar(base_dir)) stop("logging.base_dir must be non-empty")
  scenario <- .scalar_chr(lg$scenario %||% "scenario1", "scenario1")
  if (!nzchar(scenario)) stop("logging.scenario must be non-empty")
  invisible(TRUE)
}

.validate_bootstrap <- function(cfg) {
  b <- cfg$bootstrap %||% list()
  .check_positive(b$B_per_client %||% fedpoppk_const("bootstrap.B_per_client", 500L),
                  "bootstrap.B_per_client", integer = TRUE, allow_null = FALSE)
  seed <- .scalar_int(b$seed_base %||% fedpoppk_const("bootstrap.seed_base", 4869L), NA_integer_)
  if (!is.finite(seed)) stop("bootstrap.seed_base must be an integer")
  if (!is.null(b$pre_generated_root)) {
    p <- .scalar_chr(b$pre_generated_root, "")
    if (!nzchar(p)) stop("bootstrap.pre_generated_root must be a non-empty string when supplied")
  }
  invisible(TRUE)
}

fedpoppk_detect_config_kind <- function(cfg, path = "") {
  if (!is.null(cfg$bootstrap)) return("bootstrap")
  if (grepl("bootstrap", basename(path), ignore.case = TRUE)) return("bootstrap")
  "standard"
}

fedpoppk_validate_config <- function(cfg, kind = c("standard", "bootstrap", "auto"), path = "") {
  kind <- match.arg(kind)
  if (identical(kind, "auto")) {
    kind <- fedpoppk_detect_config_kind(cfg, path = path)
  }

  .validate_clients(cfg)
  .validate_model(cfg)
  .validate_parameter_block(cfg)
  .validate_optim(cfg)
  .validate_comm(cfg)
  .validate_logging(cfg)
  if (identical(kind, "bootstrap")) {
    .validate_bootstrap(cfg)
  }

  invisible(TRUE)
}
