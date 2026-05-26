# server_optimize.R
# Thin wrappers around optim() used by server entry points.
suppressPackageStartupMessages({
  library(stats)
})

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

.normalize_lbfgsb_control <- function(control) {
  ctrl_in <- control %||% list()

  # Match analysis convention: print=0 suppresses optimizer logs.
  # optim() uses trace, so map print -> trace when trace is not explicitly set.
  if (is.null(ctrl_in$trace) && !is.null(ctrl_in$print)) {
    ctrl_in$trace <- as.integer(ctrl_in$print)
  }

  ctrl <- list(
    maxit = as.integer(ctrl_in$maxit %||% fedpoppk_const("optim.maxit", 100L)),
    factr = as.numeric(ctrl_in$factr %||% fedpoppk_const("optim.factr", 1e8)),
    pgtol = as.numeric(ctrl_in$pgtol %||% fedpoppk_const("optim.pgtol", 5e-3)),
    trace = as.integer(ctrl_in$trace %||% 0L)
  )

  # Optional L-BFGS-B controls supported by optim().
  if (!is.null(ctrl_in$REPORT)) ctrl$REPORT <- as.integer(ctrl_in$REPORT)
  if (!is.null(ctrl_in$lmm)) ctrl$lmm <- as.integer(ctrl_in$lmm)

  ctrl
}

# Legacy optimizer wrapper kept for bootstrap compatibility.
server_optimize <- function(objective, init_z, control, run_id, save_dir) {
  stopifnot(is.list(objective), is.function(objective$fn))
  if (is.null(names(init_z)) || any(!nzchar(names(init_z)))) {
    stop("init_z must be a **named** numeric vector")
  }

  ctrl <- control %||% list()
  if (is.null(ctrl$method)) ctrl$method <- "BFGS"
  if (is.null(ctrl$trace) && !is.null(ctrl$print)) ctrl$trace <- as.integer(ctrl$print)
  if (is.null(ctrl$trace)) ctrl$trace <- 0

  method_to_use <- ctrl$method
  ctrl$method <- NULL

  par_names <- names(init_z)
  fn_wrap <- function(z) {
    if (is.null(names(z)) || any(!nzchar(names(z)))) names(z) <- par_names
    objective$fn(z)
  }
  gr_wrap <- NULL
  if (is.function(objective$gr)) {
    gr_wrap <- function(z) {
      if (is.null(names(z)) || any(!nzchar(names(z)))) names(z) <- par_names
      objective$gr(z)
    }
  }

  op <- optim(
    par     = init_z,
    fn      = fn_wrap,
    gr      = gr_wrap,
    method  = method_to_use,
    control = ctrl
  )

  list(
    par         = op$par,
    value       = op$value,
    counts      = op$counts,
    convergence = op$convergence,
    message     = op$message %||% "",
    control     = c(ctrl, list(method = method_to_use)),
    log         = NULL,
    meta        = list(
      run_id  = run_id,
      log_csv = objective$log_path %||% NA_character_,
      client_log_csv = objective$client_log_path %||% NA_character_
    )
  )
}

# L-BFGS-B optimization in natural parameter space.
server_optimize_lbfgsb <- function(objective,
                                   init_par,
                                   lower,
                                   upper,
                                   control,
                                   run_id,
                                   save_dir) {
  stopifnot(is.list(objective), is.function(objective$fn))
  if (is.null(names(init_par)) || any(!nzchar(names(init_par)))) {
    stop("init_par must be a **named** numeric vector")
  }
  par_names <- names(init_par)
  lower <- lower[par_names]
  upper <- upper[par_names]

  ctrl_base <- control %||% list()
  ctrl_used <- .normalize_lbfgsb_control(ctrl_base)

  fn_wrap <- function(p) {
    if (is.null(names(p)) || any(!nzchar(names(p)))) names(p) <- par_names
    objective$fn(p)
  }
  gr_wrap <- NULL
  if (is.function(objective$gr)) {
    gr_wrap <- function(p) {
      if (is.null(names(p)) || any(!nzchar(names(p)))) names(p) <- par_names
      objective$gr(p)
    }
  }

  op <- optim(
    par     = init_par,
    fn      = fn_wrap,
    gr      = gr_wrap,
    method  = "L-BFGS-B",
    lower   = lower,
    upper   = upper,
    control = ctrl_used
  )
  msg <- op$message %||% ""

  list(
    par         = op$par,
    value       = op$value,
    counts      = op$counts,
    convergence = op$convergence,
    message     = msg,
    control     = c(ctrl_used, list(method = "L-BFGS-B")),
    log         = NULL,
    meta        = list(
      run_id  = run_id,
      log_csv = objective$log_path %||% NA_character_,
      client_log_csv = objective$client_log_path %||% NA_character_
    )
  )
}
