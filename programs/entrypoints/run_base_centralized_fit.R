#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(nlmixr2)
  library(rxode2)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
ROOT <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
REPO_ROOT <- normalizePath(dirname(dirname(ROOT)), winslash = "/", mustWork = TRUE)

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
  val <- x[[nm]]
  to_num(val, default)
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
  est_eta_cl <- pick(init, "etaLcl", 0.0644)
  est_eta_v <- pick(init, "etaLvc", 0.0392)
  est_eta_cov <- pick(init, "(etaLcl,etaLvc)", pick(init, "etaLclEtaLvc", NA_real_))
  est_prop <- pick(init, "CcPropSd", sqrt(0.0953))

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
    print = to_int(args$trace %||% optim_cfg$trace, 0L),
    maxOuterIterations = to_int(args$maxit %||% optim_cfg$maxit, 5000L),
    lbfgsPgtol = to_num(args$pgtol %||% optim_cfg$pgtol, 0),
    lbfgsFactr = to_num(args$factr %||% optim_cfg$factr, 1e8),
    covMethod = ""
  )
  if (!is.null(focei_cfg$sdLowerFact)) focei_args$sdLowerFact <- to_num(focei_cfg$sdLowerFact, 0.001)
  if (!is.null(focei_cfg$diagOmegaBoundLower)) focei_args$diagOmegaBoundLower <- to_num(focei_cfg$diagOmegaBoundLower, 1)
  if (!is.null(focei_cfg$diagOmegaBoundUpper)) focei_args$diagOmegaBoundUpper <- to_num(focei_cfg$diagOmegaBoundUpper, 1)
  list(
    ctrl = do.call(foceiControl, focei_args),
    settings = focei_args
  )
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

extract_info <- function(fit_obj, dataset_label, n_patients) {
  if (inherits(fit_obj, "error")) {
    return(data.table(
      dataset = dataset_label,
      n_patients = as.integer(n_patients),
      fit_ok = FALSE,
      lka = NA_real_, lcl = NA_real_, lvc = NA_real_,
      theta_ka = NA_real_, theta_cl = NA_real_, theta_v = NA_real_,
      omega2_cl = NA_real_, omega_cl_v = NA_real_, omega2_v = NA_real_, rho_cl_v = NA_real_,
      sigma_prop = NA_real_, sigma2_prop = NA_real_,
      objf = NA_real_, aic = NA_real_,
      eps_shrink = NA_real_, omega2_cl_shrink = NA_real_, omega2_v_shrink = NA_real_,
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
  if (!is.finite(omega_cl_v) && is.finite(omega2_cl) && is.finite(omega2_v)) {
    omega_cl_v <- 0
  }
  rho_cl_v <- if (is.finite(omega_cl_v) && is.finite(omega2_cl) && is.finite(omega2_v) && omega2_cl > 0 && omega2_v > 0) {
    omega_cl_v / sqrt(omega2_cl * omega2_v)
  } else {
    NA_real_
  }
  data.table(
    dataset = dataset_label,
    n_patients = as.integer(n_patients),
    fit_ok = TRUE,
    lka = lka,
    lcl = lcl,
    lvc = lvc,
    theta_ka = exp(lka),
    theta_cl = exp(lcl),
    theta_v = exp(lvc),
    omega2_cl = omega2_cl,
    omega_cl_v = omega_cl_v,
    omega2_v = omega2_v,
    rho_cl_v = rho_cl_v,
    sigma_prop = sigma_prop,
    sigma2_prop = sigma_prop^2,
    objf = safe_objf(fit_obj),
    aic = safe_aic(fit_obj),
    eps_shrink = safe_shrink(fit_obj, "IWRES"),
    omega2_cl_shrink = safe_shrink(fit_obj, "etaLcl"),
    omega2_v_shrink = safe_shrink(fit_obj, "etaLvc"),
    convergence = as.character(tryCatch(fit_obj$message, error = function(e) NA_character_))
  )
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

weighted_between_var <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  mu <- weighted_mean_safe(x[ok], w[ok])
  sum(w[ok] * (x[ok] - mu)^2) / sum(w[ok])
}

pool_from_sites <- function(est_sites) {
  n <- est_sites$n_patients
  total_var_cl <- weighted_between_var(est_sites$lcl, n) + weighted_mean_safe(est_sites$omega2_cl, n)
  total_var_v <- weighted_between_var(est_sites$lvc, n) + weighted_mean_safe(est_sites$omega2_v, n)
  between_cov_cl_v <- {
    ok <- is.finite(est_sites$lcl) & is.finite(est_sites$lvc) & is.finite(n)
    if (!any(ok)) {
      NA_real_
    } else {
      mu_cl <- weighted_mean_safe(est_sites$lcl[ok], n[ok])
      mu_v <- weighted_mean_safe(est_sites$lvc[ok], n[ok])
      sum(n[ok] * (est_sites$lcl[ok] - mu_cl) * (est_sites$lvc[ok] - mu_v)) / sum(n[ok])
    }
  }
  total_cov_cl_v <- between_cov_cl_v + weighted_mean_safe(est_sites$omega_cl_v, n)
  total_rho_cl_v <- if (is.finite(total_cov_cl_v) && is.finite(total_var_cl) && is.finite(total_var_v) && total_var_cl > 0 && total_var_v > 0) {
    total_cov_cl_v / sqrt(total_var_cl * total_var_v)
  } else {
    NA_real_
  }
  rbindlist(list(
    data.table(parameter = "theta_ka", pooled_mean = exp(weighted_mean_safe(est_sites$lka, n)), between_var = weighted_between_var(est_sites$lka, n), within_var = NA_real_, total_var = weighted_between_var(est_sites$lka, n), definition = "between-site variance of KA fixed effect on log scale"),
    data.table(parameter = "theta_cl", pooled_mean = exp(weighted_mean_safe(est_sites$lcl, n)), between_var = weighted_between_var(est_sites$lcl, n), within_var = NA_real_, total_var = weighted_between_var(est_sites$lcl, n), definition = "between-site variance of CL fixed effect on log scale"),
    data.table(parameter = "theta_v", pooled_mean = exp(weighted_mean_safe(est_sites$lvc, n)), between_var = weighted_between_var(est_sites$lvc, n), within_var = NA_real_, total_var = weighted_between_var(est_sites$lvc, n), definition = "between-site variance of V fixed effect on log scale"),
    data.table(parameter = "omega2_cl", pooled_mean = NA_real_, between_var = NA_real_, within_var = weighted_mean_safe(est_sites$omega2_cl, n), total_var = NA_real_, definition = "within-site variance component for CL IIV"),
    data.table(parameter = "omega_cl_v", pooled_mean = NA_real_, between_var = NA_real_, within_var = weighted_mean_safe(est_sites$omega_cl_v, n), total_var = NA_real_, definition = "within-site covariance component for CL/V IIV"),
    data.table(parameter = "omega2_v", pooled_mean = NA_real_, between_var = NA_real_, within_var = weighted_mean_safe(est_sites$omega2_v, n), total_var = NA_real_, definition = "within-site variance component for V IIV"),
    data.table(parameter = "var_total_cl", pooled_mean = NA_real_, between_var = weighted_between_var(est_sites$lcl, n), within_var = weighted_mean_safe(est_sites$omega2_cl, n), total_var = total_var_cl, definition = "within omega2_cl plus between theta_cl"),
    data.table(parameter = "cov_total_cl_v", pooled_mean = NA_real_, between_var = between_cov_cl_v, within_var = weighted_mean_safe(est_sites$omega_cl_v, n), total_var = total_cov_cl_v, definition = "within omega_cl_v plus between theta_cl/theta_v covariance"),
    data.table(parameter = "var_total_v", pooled_mean = NA_real_, between_var = weighted_between_var(est_sites$lvc, n), within_var = weighted_mean_safe(est_sites$omega2_v, n), total_var = total_var_v, definition = "within omega2_v plus between theta_v"),
    data.table(parameter = "rho_total_cl_v", pooled_mean = total_rho_cl_v, between_var = NA_real_, within_var = NA_real_, total_var = NA_real_, definition = "correlation implied by cov_total_cl_v, var_total_cl, and var_total_v"),
    data.table(parameter = "sigma_prop", pooled_mean = sqrt(weighted_mean_safe(est_sites$sigma2_prop, n)), between_var = NA_real_, within_var = weighted_mean_safe(est_sites$sigma2_prop, n), total_var = NA_real_, definition = "pooled proportional residual SD from within-site variances")
  ), fill = TRUE)[, weight_basis := "patients"][]
}

write_estimates_markdown <- function(estimates, path) {
  fmt <- function(x) {
    x <- as.numeric(x)
    if (!length(x) || is.na(x)) return("NA")
    if (x == 0) return("0")
    if (abs(x) < 1e-4) return(sprintf("%.2e", x))
    formatC(signif(x, 4), format = "fg", digits = 4, flag = "#")
  }
  lines <- c(
    "# Deferiprone Base Model Fit Summary",
    "",
    "|Dataset|fit_ok|n|KA|CL|V|omega2_CL|omega_CL,V|omega2_V|rho_CL,V|sigma2_prop (sd)|OBJF|sh_eta_CL|sh_eta_V|message|",
    "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
  )
  for (i in seq_len(nrow(estimates))) {
    r <- estimates[i]
    lines <- c(lines, sprintf(
      "|%s|%s|%d|%s|%s|%s|%s|%s|%s|%s|%s (%s)|%s|%s|%s|%s|",
      r$dataset,
      ifelse(isTRUE(r$fit_ok), "TRUE", "FALSE"),
      as.integer(r$n_patients),
      fmt(r$theta_ka), fmt(r$theta_cl), fmt(r$theta_v),
      fmt(r$omega2_cl), fmt(r$omega_cl_v), fmt(r$omega2_v), fmt(r$rho_cl_v),
      fmt(r$sigma2_prop), fmt(r$sigma_prop),
      fmt(r$objf), fmt(r$omega2_cl_shrink), fmt(r$omega2_v_shrink),
      gsub("\\|", "/", as.character(r$convergence %||% ""))
    ))
  }
  writeLines(lines, path)
}

main <- function() {
  args <- parse_cli_args()
  input_dir <- normalizePath(args$`input-dir` %||% file.path(ROOT, "data", "base"), winslash = "/", mustWork = TRUE)
  output_dir <- ensure_dir(args$`output-dir` %||% file.path(ROOT, "runs", "scenario1", "standard", "centralized", "raw"))
  cfg_path <- normalizePath(args$`federated-config` %||% file.path(ROOT, "configs", "resolved", "scenario1_standard.json"), winslash = "/", mustWork = TRUE)
  cfg <- cfg_load_json_with_extends(cfg_path)
  fit_model <- build_deferiprone_base_model(cfg)
  ctrl_info <- resolve_focei_control(cfg, args)

  split_paths <- file.path(input_dir, sprintf("data1_%d.csv", 1:3))
  missing <- split_paths[!file.exists(split_paths)]
  if (length(missing)) stop("missing split files: ", paste(missing, collapse = ", "))

  raw_list <- lapply(split_paths, fread)
  dt_list <- lapply(raw_list, prepare_nlmixr_data)
  for (i in seq_along(dt_list)) setorderv(dt_list[[i]], c("ID", "TIME", "EVID", "CMT"))
  dat_all <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  setorderv(dat_all, c("ID", "TIME", "EVID", "CMT"))

  site_labels <- c("Site1", "Site2", "Site3")
  n_site <- vapply(dt_list, function(dt) uniqueN(dt$ID), integer(1))
  n_all <- uniqueN(dat_all$ID)

  cat("[run_base_centralized_fit] running nlmixr2 FOCEi fits\n")
  fit_all <- run_fit(dat_all, ctrl_info$ctrl, fit_model)
  fit_sites <- lapply(dt_list, function(dt) run_fit(dt, ctrl_info$ctrl, fit_model))

  estimates <- rbindlist(c(
    list(extract_info(fit_all, "ALL", n_all)),
    lapply(seq_along(fit_sites), function(i) extract_info(fit_sites[[i]], site_labels[[i]], n_site[[i]]))
  ), fill = TRUE)

  site_est <- estimates[dataset %in% site_labels & fit_ok == TRUE]
  pooled <- if (nrow(site_est) == 3L) pool_from_sites(site_est) else data.table(
    parameter = "pooling_skipped",
    weight_basis = "patients",
    pooled_mean = NA_real_,
    between_var = NA_real_,
    within_var = NA_real_,
    total_var = NA_real_,
    definition = "one or more site fits failed"
  )

  fit_dir <- ensure_dir(file.path(output_dir, "fit_objects"))
  saveRDS(fit_all, file.path(fit_dir, "fit_all.rds"))
  for (i in seq_along(fit_sites)) saveRDS(fit_sites[[i]], file.path(fit_dir, sprintf("fit_site%d.rds", i)))

  fwrite(estimates, file.path(output_dir, "fit_estimates.csv"))
  write_estimates_markdown(estimates, file.path(output_dir, "fit_estimates.md"))
  fwrite(pooled, file.path(output_dir, "pooled_summary.csv"))
  write_json(list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    script = file.path(ROOT, "run_base_centralized_fit.R"),
    input_dir = input_dir,
    output_dir = output_dir,
    config_path = cfg_path,
    split_files = split_paths,
    n_patients = c(list(ALL = n_all), as.list(setNames(as.integer(n_site), site_labels))),
    model = "deferiprone oral 1-compartment base model; no covariate model; DV in umol/L",
    control = ctrl_info$settings,
    outputs = list(
      fit_objects = fit_dir,
      estimates_csv = file.path(output_dir, "fit_estimates.csv"),
      pooled_csv = file.path(output_dir, "pooled_summary.csv")
    )
  ), file.path(output_dir, "fit_meta.json"), auto_unbox = TRUE, pretty = TRUE, digits = 10)
  cat("[run_base_centralized_fit] completed\n")
}

main()
