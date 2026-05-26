# compute_obj_grad.R
# Evaluates client-side objective values and optional numerical gradients.
# Uses Richardson finite differences with optional parallel acceleration via pnd.
suppressPackageStartupMessages({
  library(nlmixr2)
  library(numDeriv)
  library(rxode2)
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
local({
  candidates <- c("/project/R/common/param_transform.R", "R/common/param_transform.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/param_transform.R not found")
  source(hit[[1L]], chdir = TRUE)
})

# Override nlmixr2 iniDf estimates using natural-scale parameters.
# This is name-driven (not hard-coded), so alternative models/parameter sets are supported.
apply_ini_overrides <- function(model_ui, th_nat) {
  ui2   <- model_ui
  iniDf <- ui2$iniDf
  if (is.null(iniDf) || is.null(iniDf$name) || is.null(iniDf$est)) {
    stop("model_ui$iniDf is missing required fields")
  }
  th_vec <- unlist(th_nat, use.names = TRUE)
  storage.mode(th_vec) <- "double"
  if (is.null(names(th_vec)) || any(!nzchar(names(th_vec)))) {
    stop("th_nat must be a named numeric vector")
  }

  shared <- intersect(names(th_vec), iniDf$name)
  if (length(shared)) {
    idx <- match(shared, iniDf$name)
    iniDf$est[idx] <- th_vec[shared]
  }

  ui2$iniDf <- iniDf
  ui2
}

# Objective function evaluation helpers.
.as_df <- function(x) {
  if (inherits(x, "data.frame")) return(as.data.frame(x))
  stop("data must be a data.frame (or a list of them)")
}

# Evaluate a single dataset and return FOCEi objective value.
.eval_one <- function(ui2, dt) {
  dt2 <- .as_df(dt)
  fit <- suppressMessages(
    nlmixr2(
      object    = ui2,
      data      = dt2,
      est       = "focei",
      # Align local OBJF evaluation settings with analysis-side FOCEi setup.
      control   = foceiControl(maxOuterIterations = 0,
                               outerOpt = "L-BFGS-B",
                               calcTables = FALSE, covMethod = "", print = 0),
      rxControl = rxode2::rxControl(nCores = 1)
    )
  )
  on.exit({
    # Explicitly release large fit objects in long bootstrap runs.
    if (exists("fit", inherits = FALSE)) rm(fit)
    if (exists("objDf", inherits = FALSE)) rm(objDf)
    if (exists("dt2", inherits = FALSE)) rm(dt2)
  }, add = TRUE)
  objDf <- fit$objDf
  if (!is.null(objDf)) {
    objDf <- as.data.frame(objDf)
  }
  if (is.null(objDf) || !"OBJF" %in% colnames(objDf) || nrow(objDf) == 0L) {
    stop("nlmixr2 fit did not return a usable OBJF table")
  }
  if ("FOCEi" %in% rownames(objDf)) {
    obj_val <- objDf["FOCEi", "OBJF"]
  } else {
    obj_val <- objDf[[1, "OBJF"]]
  }
  if (!is.finite(obj_val)) {
    stop("nlmixr2 returned non-finite OBJF")
  }
  as.numeric(obj_val)
}

# Evaluate objective at z for state_env (single dataset or list of datasets).
eval_objf_z <- function(z, env) {
  get2 <- function(nm, default = NULL) {
    if (is.environment(env)) {
      if (exists(nm, envir = env, inherits = FALSE)) get(nm, envir = env, inherits = FALSE) else default
    } else {
      env[[nm]] %||% default
    }
  }
  lower <- get2("lower", NULL)
  upper <- get2("upper", NULL)
  param_spec <- param_resolve_spec(get2("param_spec", NULL), names(z), lower = lower, upper = upper)
  th  <- param_int_to_ini(z, param_spec = param_spec)

  ui_base <- get2("model_ui")
  if (is.null(ui_base)) stop("env$model_ui is missing")

  ui2 <- apply_ini_overrides(ui_base, th)
  dt  <- get2("dt")
  if (is.null(dt)) stop("env$dt is missing")

  if (is.list(dt) && !inherits(dt, "data.frame")) {
    vals <- vapply(dt, function(d) .eval_one(ui2, d), numeric(1))
    if (any(!is.finite(vals))) Inf else sum(vals)
  } else {
    .eval_one(ui2, dt)
  }
}

.PENALTY_OBJF <- fedpoppk_const("penalty_objf", 1e15)
# Safe objective wrapper that returns penalty on failure.
safe_eval_F <- function(z, env) {
  f <- try(eval_objf_z(z, env), silent = TRUE)
  if (inherits(f, "try-error")) {
    msg <- conditionMessage(attr(f, "condition"))
    message("[compute_obj_grad] evaluation failed: ", msg)
    return(list(ok = FALSE, f = .PENALTY_OBJF, error = msg))
  }
  if (!is.finite(f)) {
    message("[compute_obj_grad] evaluation produced non-finite OBJF; applying penalty")
    return(list(ok = FALSE, f = .PENALTY_OBJF, error = "non-finite OBJF"))
  }
  list(ok = TRUE, f = f)
}

.safe_int <- function(x, default = 1L) {
  val <- suppressWarnings(as.integer(x))
  if (!length(val) || is.na(val) || !is.finite(val)) return(as.integer(default))
  as.integer(val[[1L]])
}

.with_quiet_pnd_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      # pnd emits this advisory when using numDeriv-compatible arguments.
      # Keep behavior unchanged and silence only this known warning.
      if (grepl("You are using numDeriv-like syntax", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

.compute_grad <- function(Fz, p_z, eps_use, grad_cores = 1L) {
  grad_cores <- max(1L, .safe_int(grad_cores, default = 1L))

  # Keep numDeriv behavior as default; switch to pnd when multi-core is requested.
  if (grad_cores <= 1L) {
    return(numDeriv::grad(
      func = Fz,
      x = p_z,
      method = "Richardson",
      method.args = list(eps = eps_use)
    ))
  }

  if (!requireNamespace("pnd", quietly = TRUE)) {
    warning("pnd is not installed; falling back to numDeriv::grad on a single core")
    return(numDeriv::grad(
      func = Fz,
      x = p_z,
      method = "Richardson",
      method.args = list(eps = eps_use)
    ))
  }

  .with_quiet_pnd_warning(
    pnd::Grad(
      FUN = Fz,
      x = p_z,
      method = "Richardson",
      method.args = list(eps = eps_use),
      cores = grad_cores,
      preschedule = TRUE
    )
  )
}

# Public API: compute objective and optional gradient for a client request.
compute_obj_grad <- function(p_z,
                             state_env,
                             return_grad    = TRUE,
                             richardson_eps = fedpoppk_const("objective.richardson_eps", 1e-5)) {
  stopifnot(is.numeric(p_z))
  get_state <- function(name, default = NULL) {
    if (is.environment(state_env)) {
      if (exists(name, envir = state_env, inherits = FALSE)) get(name, envir = state_env, inherits = FALSE) else default
    } else {
      state_env[[name]] %||% default
    }
  }

  set_state <- function(name, value) {
    if (is.environment(state_env)) assign(name, value, envir = state_env)
  }

  param_spec <- get_state("param_spec", NULL)
  param_names <- param_spec$names %||% names(p_z)
  if (is.null(param_names) || any(!nzchar(param_names))) {
    stop("parameter names are required (missing in both param_spec and request vector)")
  }
  if (is.null(names(p_z)) || any(!nzchar(names(p_z)))) {
    if (length(p_z) != length(param_names)) {
      stop("unnamed parameter vector length does not match param_spec names")
    }
    p_z <- setNames(as.numeric(p_z), param_names[seq_along(p_z)])
  }
  nms <- names(p_z)
  param_spec <- param_resolve_spec(param_spec, nms,
                                    lower = get_state("lower", NULL),
                                    upper = get_state("upper", NULL))
  if (is.null(get_state("param_spec", NULL))) {
    set_state("param_spec", param_spec)
  }

  if (isTRUE(getOption("fednlmixr2.debug_theta", FALSE))) {
    th_dbg <- param_int_to_ini(p_z, param_spec = param_spec)
    message("theta@run = ", paste(sprintf("%s=%.6g", names(th_dbg), unlist(th_dbg)), collapse=", "))
  }

  eps_use <- richardson_eps
  grad_cores <- max(1L, .safe_int(get_state("grad_cores", 1L), default = 1L))

  Fz <- function(z) {
    if (is.null(names(z)) || any(!nzchar(names(z)))) names(z) <- nms
    eval_objf_z(z, state_env)
  }

  base <- safe_eval_F(p_z, state_env)
  if (!base$ok) return(list(objf = base$f, grad = NULL))

  if (!isTRUE(return_grad)) return(list(objf = base$f, grad = NULL))

  grad_try <- try(
    .compute_grad(Fz = Fz, p_z = p_z, eps_use = eps_use, grad_cores = grad_cores),
    silent = TRUE
  )
  if (inherits(grad_try, "try-error") || any(!is.finite(grad_try))) {
    return(list(objf = base$f, grad = NULL))
  }
  grad_vec <- as.numeric(grad_try)

  grad_vec <- as.numeric(grad_vec)
  if (length(grad_vec) != length(nms)) {
    grad_vec <- grad_vec[seq_along(nms)]
  }
  grad_vec[!is.finite(grad_vec)] <- 0
  names(grad_vec) <- nms
  list(objf = base$f, grad = grad_vec)
}
# ---------------------------------------------------------------------------
