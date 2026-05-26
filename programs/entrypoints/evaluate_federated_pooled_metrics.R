#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(nlmixr2)
  library(rxode2)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
ROOT <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", unset = dirname(script_path))
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)
REPO_HOST <- normalizePath("/project", winslash = "/", mustWork = FALSE)
REPO_CONTAINER <- "/project"

`%||%` <- function(x, y) if (is.null(x)) y else x

map_path <- function(path) {
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  if (startsWith(path, REPO_HOST)) {
    mapped <- sub(paste0("^", REPO_HOST), REPO_CONTAINER, path)
    if (file.exists(mapped)) return(normalizePath(mapped, winslash = "/", mustWork = FALSE))
  }
  if (startsWith(path, REPO_CONTAINER)) {
    mapped <- sub(paste0("^", REPO_CONTAINER), REPO_HOST, path)
    if (file.exists(mapped)) return(normalizePath(mapped, winslash = "/", mustWork = FALSE))
  }
  path
}

prepare_nlmixr_data <- function(dt_raw) {
  dt <- copy(as.data.table(dt_raw))
  setorderv(dt, c("id", "time", "evid", "cmt"))
  dt[, ID := as.integer(id)]
  dt[, TIME := as.numeric(time)]
  dt[, CMT := as.integer(cmt)]
  dt[, EVID := as.integer(evid)]
  dt[, AMT := fifelse(EVID == 1L, as.numeric(fifelse(is.na(amt), 0, amt)), NA_real_)]
  dt[, DV := fifelse(EVID == 0L, as.numeric(dv), NA_real_)]
  dt[, MDV := as.integer(EVID != 0L)]
  dt[, .(ID, TIME, CMT, AMT, EVID, DV, MDV)]
}

build_model <- function(par_nat) {
  omega_line <- if (!is.null(par_nat[["(etaLcl,etaLvc)"]])) {
    sprintf(
      "etaLcl + etaLvc ~ c(%0.16g, %0.16g, %0.16g)",
      as.numeric(par_nat$etaLcl),
      as.numeric(par_nat[["(etaLcl,etaLvc)"]]),
      as.numeric(par_nat$etaLvc)
    )
  } else {
    sprintf("etaLcl ~ %0.16g\n        etaLvc ~ %0.16g", as.numeric(par_nat$etaLcl), as.numeric(par_nat$etaLvc))
  }
  model_txt <- sprintf(
    "function() {
      ini({
        lka <- %0.16g
        lcl <- %0.16g
        lvc <- %0.16g
        CcPropSd <- c(0.0003, %0.16g)
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
    as.numeric(par_nat$lka),
    as.numeric(par_nat$lcl),
    as.numeric(par_nat$lvc),
    as.numeric(par_nat$CcPropSd),
    omega_line
  )
  eval(parse(text = model_txt))
}

safe_objf <- function(fit) {
  obj <- tryCatch(as.data.frame(fit$objDf), error = function(e) NULL)
  if (!is.null(obj) && "OBJF" %in% names(obj) && nrow(obj)) return(as.numeric(obj$OBJF[[1L]]))
  ll <- tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_)
  if (is.finite(ll)) -2 * ll else NA_real_
}

safe_shrink <- function(fit, col_name) {
  sh <- tryCatch(as.data.frame(fit$shrink), error = function(e) NULL)
  if (is.null(sh) || !("sd shrinkage (%)" %in% rownames(sh)) || !(col_name %in% colnames(sh))) return(NA_real_)
  as.numeric(sh["sd shrinkage (%)", col_name])
}

evaluate_one <- function(scenario) {
  latest_path <- file.path(ROOT, "runs", scenario, "standard", "federated", "latest_run.txt")
  if (!file.exists(latest_path)) stop("missing latest run pointer: ", latest_path)
  run_dir <- map_path(trimws(readLines(latest_path, warn = FALSE)[1L]))
  result <- fromJSON(file.path(run_dir, "result.json"), simplifyVector = FALSE)
  nat <- result$natPar
  fit_model <- build_model(nat)

  split_paths <- file.path(ROOT, "data", scenario, "base", sprintf("data1_%d.csv", 1:3))
  if (!all(file.exists(split_paths))) {
    split_paths <- file.path(ROOT, "data", "base", sprintf("data1_%d.csv", 1:3))
  }
  dat_all <- rbindlist(lapply(lapply(split_paths, fread), prepare_nlmixr_data), use.names = TRUE, fill = TRUE)
  setorderv(dat_all, c("ID", "TIME", "EVID", "CMT"))

  fit <- suppressMessages(nlmixr2(
    fit_model,
    dat_all,
    est = "focei",
    control = foceiControl(
      outerOpt = "L-BFGS-B",
      print = 0L,
      maxOuterIterations = 0L,
      lbfgsPgtol = as.numeric(result$control$pgtol %||% 0),
      lbfgsFactr = as.numeric(result$control$factr %||% 1e8),
      covMethod = ""
    ),
    rxControl = rxode2::rxControl(nCores = 1)
  ))

  out_dir <- file.path(ROOT, "runs", scenario, "standard", "federated", "posthoc_eval")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sigma_prop <- as.numeric(nat$CcPropSd)
  omega2_cl <- as.numeric(nat$etaLcl)
  omega2_v <- as.numeric(nat$etaLvc)
  omega_cl_v <- if (!is.null(nat[["(etaLcl,etaLvc)"]])) {
    as.numeric(nat[["(etaLcl,etaLvc)"]])
  } else if (is.finite(omega2_cl) && is.finite(omega2_v)) {
    0
  } else {
    NA_real_
  }
  rho_cl_v <- if (is.finite(omega_cl_v) && is.finite(omega2_cl) && is.finite(omega2_v) && omega2_cl > 0 && omega2_v > 0) {
    omega_cl_v / sqrt(omega2_cl * omega2_v)
  } else {
    NA_real_
  }
  row <- data.frame(
    scenario = scenario,
    run_dir = run_dir,
    run_id = result$meta$run_id %||% basename(run_dir),
    objective_saved = as.numeric(result$value %||% NA_real_),
    objective_pooled_eval = safe_objf(fit),
    objective_delta = safe_objf(fit) - as.numeric(result$value %||% NA_real_),
    KA = exp(as.numeric(nat$lka)),
    CL = exp(as.numeric(nat$lcl)),
    V = exp(as.numeric(nat$lvc)),
    omega2_cl = omega2_cl,
    omega_cl_v = omega_cl_v,
    omega2_v = omega2_v,
    rho_cl_v = rho_cl_v,
    sigma_prop = sigma_prop,
    sigma2_prop = sigma_prop^2,
    sigma2_prop_x1e3 = sigma_prop^2 * 1000,
    sh_eta_cl_pct = safe_shrink(fit, "etaLcl"),
    sh_eta_v_pct = safe_shrink(fit, "etaLvc"),
    sh_eps_pct = safe_shrink(fit, "IWRES"),
    n_subjects = uniqueN(dat_all$ID),
    stringsAsFactors = FALSE
  )
  write.csv(row, file.path(out_dir, "federated_pooled_eval.csv"), row.names = FALSE, na = "")
  saveRDS(fit, file.path(out_dir, "fit_fixed_federated.rds"))
  writeLines(c(
    "# Federated Pooled Evaluation",
    "",
    paste0("- `scenario = ", scenario, "`"),
    paste0("- `objective_saved = ", format(row$objective_saved, digits = 10), "`"),
    paste0("- `objective_pooled_eval = ", format(row$objective_pooled_eval, digits = 10), "`"),
    paste0("- `objective_delta = ", format(row$objective_delta, digits = 10), "`"),
    paste0("- `sh_eta_cl_pct = ", format(row$sh_eta_cl_pct, digits = 10), "`"),
    paste0("- `sh_eta_v_pct = ", format(row$sh_eta_v_pct, digits = 10), "`")
  ), file.path(out_dir, "federated_pooled_eval.md"))
  cat(toJSON(row, auto_unbox = TRUE, pretty = TRUE), "\n")
}

args <- commandArgs(trailingOnly = TRUE)
scenarios <- if (length(args)) args else "scenario1"
invisible(lapply(scenarios, evaluate_one))
