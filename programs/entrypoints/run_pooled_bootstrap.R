#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(nlmixr2)
  library(parallel)
  library(rxode2)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
SCRIPT_ROOT <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
REPO_ROOT <- normalizePath(dirname(dirname(SCRIPT_ROOT)), winslash = "/", mustWork = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a

source_first <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (!length(hit)) stop("missing required source file; checked: ", paste(paths, collapse = ", "))
  source(hit[[1L]], chdir = TRUE)
}

source_first(c(
  file.path(REPO_ROOT, "programs", "code", "R", "config", "config_loader.R"),
  file.path(REPO_ROOT, "R", "config", "config_loader.R")
))

arg_value <- function(args, key, default = NULL) {
  prefix <- paste0("--", key, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[[1L]], fixed = TRUE)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

to_num <- function(x, default = NA_real_) {
  if (is.null(x)) return(default)
  val <- suppressWarnings(as.numeric(x))
  if (!length(val) || is.na(val[[1L]])) return(default)
  val[[1L]]
}

to_int <- function(x, default = 0L) {
  val <- suppressWarnings(as.integer(x))
  if (!length(val) || is.na(val[[1L]])) return(as.integer(default))
  val[[1L]]
}

pick <- function(x, nm, default) {
  to_num(x[[nm]], default)
}

bound_expr <- function(est, lower = NA_real_, upper = NA_real_) {
  lo_ok <- is.finite(lower)
  hi_ok <- is.finite(upper)
  if (lo_ok && hi_ok) return(sprintf("c(%0.16g, %0.16g, %0.16g)", lower, est, upper))
  if (lo_ok) return(sprintf("c(%0.16g, %0.16g)", lower, est))
  if (hi_ok) return(sprintf("c(%0.16g, %0.16g)", est, upper))
  sprintf("%0.16g", est)
}

build_deferiprone_base_model <- function(cfg) {
  init <- cfg$initPar %||% list()
  lower <- cfg$lower %||% list()
  upper <- cfg$upper %||% list()
  est_lka <- pick(init, "lka", log(9.13))
  est_lcl <- pick(init, "lcl", log(8.3))
  est_lvc <- pick(init, "lvc", log(18.7))
  est_eta_cl <- pick(init, "etaLcl", 0.086)
  est_eta_v <- pick(init, "etaLvc", 0.086)
  est_eta_cov <- pick(init, "(etaLcl,etaLvc)", pick(init, "etaLclEtaLvc", 0.030))
  est_prop <- pick(init, "CcPropSd", 0.3)

  lo <- function(nm) to_num(lower[[nm]], NA_real_)
  up <- function(nm) to_num(upper[[nm]], NA_real_)

  omega_line <- if (is.finite(est_eta_cov)) {
    sprintf("etaLcl + etaLvc ~ c(%0.16g, %0.16g, %0.16g)", est_eta_cl, est_eta_cov, est_eta_v)
  } else {
    sprintf("etaLcl ~ %0.16g\n        etaLvc ~ %0.16g", est_eta_cl, est_eta_v)
  }

  model_txt <- sprintf(
    "function() {
      ini({
        lka <- %s
        lcl <- %s
        lvc <- %s
        CcPropSd <- %s
        %s
      })
      model({
        ka <- exp(lka)
        cl <- exp(lcl + etaLcl)
        vc <- exp(lvc + etaLvc)
        Cc <- linCmt() * 1000 / 139.15
        Cc ~ prop(CcPropSd)
      })
    }",
    bound_expr(est_lka, lo("lka"), up("lka")),
    bound_expr(est_lcl, lo("lcl"), up("lcl")),
    bound_expr(est_lvc, lo("lvc"), up("lvc")),
    bound_expr(est_prop, lo("CcPropSd"), up("CcPropSd")),
    omega_line
  )
  eval(parse(text = model_txt))
}

resolve_focei_control <- function(cfg, args) {
  optim_cfg <- cfg$optimControl %||% list()
  focei_cfg <- cfg$foceiControl %||% list()
  focei_args <- list(
    outerOpt = "L-BFGS-B",
    print = to_int(arg_value(args, "trace", optim_cfg$trace %||% 0L), 0L),
    maxOuterIterations = to_int(arg_value(args, "maxit", optim_cfg$maxit %||% 100L), 100L),
    lbfgsPgtol = to_num(arg_value(args, "pgtol", optim_cfg$pgtol %||% 0), 0),
    lbfgsFactr = to_num(arg_value(args, "factr", optim_cfg$factr %||% 1e10), 1e10),
    covMethod = ""
  )
  if (!is.null(focei_cfg$sdLowerFact)) focei_args$sdLowerFact <- to_num(focei_cfg$sdLowerFact, 0.001)
  if (!is.null(focei_cfg$diagOmegaBoundLower)) focei_args$diagOmegaBoundLower <- to_num(focei_cfg$diagOmegaBoundLower, 1)
  if (!is.null(focei_cfg$diagOmegaBoundUpper)) focei_args$diagOmegaBoundUpper <- to_num(focei_cfg$diagOmegaBoundUpper, 1)
  list(ctrl = do.call(foceiControl, focei_args), settings = focei_args)
}

prepare_nlmixr_data <- function(dt_raw) {
  req <- c("id", "time", "cmt", "amt", "evid", "dv")
  miss <- setdiff(req, names(dt_raw))
  if (length(miss)) stop("input data is missing columns: ", paste(miss, collapse = ", "))
  dt <- copy(dt_raw)
  dt[, ID := as.integer(id)]
  dt[, TIME := as.numeric(time)]
  dt[, CMT := as.integer(cmt)]
  dt[, EVID := as.integer(evid)]
  dt[, AMT := fifelse(EVID == 1L, as.numeric(fifelse(is.na(amt), 0, amt)), NA_real_)]
  dt[, DV := fifelse(EVID == 0L, as.numeric(dv), NA_real_)]
  dt[, MDV := as.integer(EVID != 0L)]
  dt[, .(ID, TIME, CMT, AMT, EVID, DV, MDV)]
}

shift_subject_ids <- function(dt_list, block = 100000L) {
  lapply(seq_along(dt_list), function(idx) {
    dt <- copy(as.data.table(dt_list[[idx]]))
    dt[, id := as.integer(id) + as.integer((idx - 1L) * block)]
    dt
  })
}

run_fit <- function(dat, focei_ctrl, fit_model) {
  tryCatch(
    suppressMessages(nlmixr2(
      fit_model,
      dat,
      est = "focei",
      control = focei_ctrl,
      rxControl = rxode2::rxControl(nCores = 1)
    )),
    error = function(e) e
  )
}

safe_pf <- function(fit, row_name) {
  pf <- tryCatch(fit$parFixedDf, error = function(e) NULL)
  if (is.null(pf) || !(row_name %in% rownames(pf))) return(NA_real_)
  as.numeric(pf[row_name, "Estimate"])
}

safe_omega <- function(fit, r, c) {
  om <- tryCatch(as.matrix(fit$omega), error = function(e) NULL)
  if (is.null(om) || !(r %in% rownames(om)) || !(c %in% colnames(om))) return(NA_real_)
  as.numeric(om[r, c])
}

safe_shrink <- function(fit, col_name) {
  sh <- tryCatch(as.data.frame(fit$shrink), error = function(e) NULL)
  if (is.null(sh) || !("sd shrinkage (%)" %in% rownames(sh)) || !(col_name %in% colnames(sh))) return(NA_real_)
  as.numeric(sh["sd shrinkage (%)", col_name])
}

safe_objf <- function(fit) {
  obj <- tryCatch(as.data.frame(fit$objDf), error = function(e) NULL)
  if (!is.null(obj) && "OBJF" %in% names(obj) && nrow(obj)) return(as.numeric(obj$OBJF[[1L]]))
  ll <- tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_)
  if (is.finite(ll)) -2 * ll else NA_real_
}

safe_aic <- function(fit) as.numeric(tryCatch(AIC(fit), error = function(e) NA_real_))

extract_info <- function(fit_obj, n_patients) {
  if (inherits(fit_obj, "error")) {
    return(data.table(
      fit_ok = FALSE,
      lka = NA_real_, lcl = NA_real_, lvc = NA_real_,
      KA = NA_real_, CL = NA_real_, V = NA_real_,
      omega2_cl = NA_real_, omega_cl_v = NA_real_, omega2_v = NA_real_, rho_cl_v = NA_real_,
      sigma_prop = NA_real_, sigma2_prop = NA_real_,
      objf = NA_real_, aic = NA_real_,
      sh_eta_cl_pct = NA_real_, sh_eta_v_pct = NA_real_, sh_eps_pct = NA_real_,
      convergence = conditionMessage(fit_obj)
    ))
  }
  lka <- safe_pf(fit_obj, "lka")
  lcl <- safe_pf(fit_obj, "lcl")
  lvc <- safe_pf(fit_obj, "lvc")
  sigma_prop <- safe_pf(fit_obj, "CcPropSd")
  omega2_cl <- safe_omega(fit_obj, "etaLcl", "etaLcl")
  omega_cl_v <- safe_omega(fit_obj, "etaLcl", "etaLvc")
  omega2_v <- safe_omega(fit_obj, "etaLvc", "etaLvc")
  rho_cl_v <- if (is.finite(omega_cl_v) && is.finite(omega2_cl) && is.finite(omega2_v) && omega2_cl > 0 && omega2_v > 0) {
    omega_cl_v / sqrt(omega2_cl * omega2_v)
  } else {
    NA_real_
  }
  data.table(
    fit_ok = TRUE,
    lka = lka,
    lcl = lcl,
    lvc = lvc,
    KA = exp(lka),
    CL = exp(lcl),
    V = exp(lvc),
    omega2_cl = omega2_cl,
    omega_cl_v = omega_cl_v,
    omega2_v = omega2_v,
    rho_cl_v = rho_cl_v,
    sigma_prop = sigma_prop,
    sigma2_prop = sigma_prop^2,
    objf = safe_objf(fit_obj),
    aic = safe_aic(fit_obj),
    sh_eta_cl_pct = safe_shrink(fit_obj, "etaLcl"),
    sh_eta_v_pct = safe_shrink(fit_obj, "etaLvc"),
    sh_eps_pct = safe_shrink(fit_obj, "IWRES"),
    convergence = as.character(tryCatch(fit_obj$message, error = function(e) NA_character_))
  )
}

is_retryable_compile_failure <- function(fit_obj, est_row) {
  msg <- ""
  if (inherits(fit_obj, "error")) {
    msg <- conditionMessage(fit_obj)
  } else if (!is.null(est_row$convergence) && length(est_row$convergence)) {
    msg <- as.character(est_row$convergence[[1L]] %||% "")
  }
  grepl("something went wrong in compilation", msg, fixed = TRUE)
}

run_one_trial <- function(trial_id, scenario, trial_root, focei_ctrl, fit_model, output_dir) {
  trial_dir <- file.path(trial_root, sprintf("trial_%04d", trial_id))
  split_paths <- file.path(trial_dir, sprintf("data1_%d.rds", 1:3))
  missing <- split_paths[!file.exists(split_paths)]
  if (length(missing)) {
    result <- list(
      scenario = scenario,
      trial = as.integer(trial_id),
      fit_ok = FALSE,
      converged = FALSE,
      error = sprintf("missing split files: %s", paste(missing, collapse = ", "))
    )
    write_json(result, file.path(output_dir, sprintf("pooled_bootstrap_trial_%04d.json", trial_id)), auto_unbox = TRUE, pretty = TRUE, digits = 10)
    return(result)
  }

  raw_list <- lapply(split_paths, readRDS)
  raw_shifted <- shift_subject_ids(raw_list)
  dat_all <- rbindlist(lapply(raw_shifted, prepare_nlmixr_data), use.names = TRUE, fill = TRUE)
  setorderv(dat_all, c("ID", "TIME", "EVID", "CMT"))

  max_attempts <- 3L
  row <- NULL
  fit <- NULL
  for (attempt in seq_len(max_attempts)) {
    fit <- run_fit(dat_all, focei_ctrl = focei_ctrl, fit_model = fit_model)
    est <- extract_info(fit, uniqueN(dat_all$ID))
    row <- as.list(est[1])
    if (!is_retryable_compile_failure(fit, row) || attempt == max_attempts) break
    Sys.sleep(1)
  }

  conv_msg <- as.character(row$convergence %||% "")
  converged <- isTRUE(row$fit_ok) && startsWith(conv_msg, "CONVERGENCE:")
  result <- c(
    list(
      scenario = scenario,
      trial = as.integer(trial_id),
      n_patients = as.integer(uniqueN(dat_all$ID)),
      fit_ok = isTRUE(row$fit_ok),
      converged = converged
    ),
    row[c(
      "lka", "lcl", "lvc", "KA", "CL", "V",
      "omega2_cl", "omega_cl_v", "omega2_v", "rho_cl_v",
      "sigma_prop", "sigma2_prop", "objf", "aic",
      "sh_eta_cl_pct", "sh_eta_v_pct", "sh_eps_pct", "convergence"
    )]
  )
  write_json(result, file.path(output_dir, sprintf("pooled_bootstrap_trial_%04d.json", trial_id)), auto_unbox = TRUE, pretty = TRUE, digits = 10)
  result
}

summarize_trials <- function(results_dt) {
  conv <- results_dt[converged == TRUE]
  metrics <- list(
    KA = conv$KA,
    CL = conv$CL,
    V = conv$V,
    omega2_cl = conv$omega2_cl,
    omega_cl_v = conv$omega_cl_v,
    omega2_v = conv$omega2_v,
    rho_cl_v = conv$rho_cl_v,
    sigma2_prop = conv$sigma2_prop
  )
  rbindlist(lapply(names(metrics), function(metric) {
    x <- metrics[[metric]]
    data.table(
      parameter = metric,
      n_converged = length(x),
      median = if (length(x)) as.numeric(stats::median(x, na.rm = TRUE)) else NA_real_,
      ci_lower = if (length(x)) as.numeric(stats::quantile(x, probs = 0.025, names = FALSE, na.rm = TRUE)) else NA_real_,
      ci_upper = if (length(x)) as.numeric(stats::quantile(x, probs = 0.975, names = FALSE, na.rm = TRUE)) else NA_real_
    )
  }), fill = TRUE)
}

write_summary_md <- function(summary_dt, path, scenario, total_trials, converged_trials) {
  fmt <- function(x) {
    if (!is.finite(x)) return("NA")
    format(signif(x, 6), scientific = FALSE, trim = TRUE)
  }
  lines <- c(
    sprintf("# Centralized Bootstrap Summary: %s", scenario),
    "",
    sprintf("- attempted trials: %d", total_trials),
    sprintf("- converged trials: %d", converged_trials),
    "",
    "|parameter|n_converged|median|ci_lower|ci_upper|",
    "|---|---:|---:|---:|---:|"
  )
  for (i in seq_len(nrow(summary_dt))) {
    r <- summary_dt[i]
    lines <- c(lines, sprintf("|%s|%d|%s|%s|%s|", r$parameter, as.integer(r$n_converged), fmt(r$median), fmt(r$ci_lower), fmt(r$ci_upper)))
  }
  writeLines(lines, path)
}

run_pooled_bootstrap <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  scenario <- as.character(arg_value(args, "scenario", NA_character_))
  if (!nzchar(scenario) || is.na(scenario)) stop("--scenario is required")
  if (!scenario %in% c("scenario1", "scenario2")) stop("--scenario must be scenario1 or scenario2")

  cfg_path <- normalizePath(as.character(arg_value(args, "config", NA_character_)), winslash = "/", mustWork = TRUE)
  trial_root <- normalizePath(as.character(arg_value(args, "trial-root", NA_character_)), winslash = "/", mustWork = TRUE)
  output_root <- ensure_dir(as.character(arg_value(args, "output-root", NA_character_)))
  start_id <- to_int(arg_value(args, "trial-start", "1"), 1L)
  end_id <- to_int(arg_value(args, "trial-end", "500"), 500L)
  workers <- to_int(arg_value(args, "workers", "1"), 1L)
  if (!is.finite(start_id) || !is.finite(end_id) || start_id < 1L || end_id < start_id) stop("invalid trial range")

  cfg <- cfg_load_json_with_extends(cfg_path)
  fit_model <- build_deferiprone_base_model(cfg)
  ctrl_info <- resolve_focei_control(cfg, args)

  raw_dir <- ensure_dir(file.path(output_root, "raw"))
  aggregate_dir <- ensure_dir(file.path(output_root, "aggregate"))
  reporting_dir <- ensure_dir(file.path(output_root, "reporting"))
  trial_ids <- seq.int(start_id, end_id)

  worker_fn <- function(trial_id) {
    run_one_trial(
      trial_id = trial_id,
      scenario = scenario,
      trial_root = trial_root,
      focei_ctrl = ctrl_info$ctrl,
      fit_model = fit_model,
      output_dir = raw_dir
    )
  }

  results <- if (workers > 1L) {
    mclapply(trial_ids, worker_fn, mc.cores = workers, mc.preschedule = TRUE)
  } else {
    lapply(trial_ids, worker_fn)
  }

  results_dt <- rbindlist(lapply(results, as.data.table), fill = TRUE)
  setorder(results_dt, trial)
  fwrite(results_dt, file.path(aggregate_dir, "trial_results.csv"))

  summary_dt <- summarize_trials(results_dt)
  fwrite(summary_dt, file.path(aggregate_dir, "summary_converged_only.csv"))
  write_summary_md(
    summary_dt,
    path = file.path(reporting_dir, "summary_converged_only.md"),
    scenario = scenario,
    total_trials = length(trial_ids),
    converged_trials = results_dt[converged == TRUE, .N]
  )

  write_json(list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    scenario = scenario,
    config = cfg_path,
    trial_root = trial_root,
    output_root = output_root,
    trial_start = start_id,
    trial_end = end_id,
    workers = workers,
    model = "deferiprone oral 1-compartment base model; centralized pooled bootstrap",
    control = ctrl_info$settings
  ), file.path(output_root, "run_meta.json"), auto_unbox = TRUE, pretty = TRUE, digits = 10)

  cat(toJSON(list(
    scenario = scenario,
    attempted = length(trial_ids),
    converged = results_dt[converged == TRUE, .N],
    raw_dir = raw_dir,
    aggregate_dir = aggregate_dir,
    reporting_dir = reporting_dir
  ), auto_unbox = TRUE, pretty = TRUE, digits = 10))
}

if (sys.nframe() == 0L) run_pooled_bootstrap()
