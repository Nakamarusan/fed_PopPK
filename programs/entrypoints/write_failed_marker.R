#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop("usage: write_failed_marker.R <trial> <status> <timeout_sec> <dst>")
}

scenario <- Sys.getenv("SCENARIO")
trial <- as.integer(args[[1]])
status <- as.integer(args[[2]])
timeout_sec <- as.integer(as.numeric(args[[3]]))
dst <- args[[4]]

reason <- if (status %in% c(124L, 137L)) "timeout" else "rerun exited without result"
result <- list(
  scenario = scenario,
  trial = trial,
  n_patients = NA,
  fit_ok = FALSE,
  converged = FALSE,
  lka = NA,
  lcl = NA,
  lvc = NA,
  KA = NA,
  CL = NA,
  V = NA,
  omega2_cl = NA,
  omega_cl_v = NA,
  omega2_v = NA,
  rho_cl_v = NA,
  sigma_prop = NA,
  sigma2_prop = NA,
  objf = NA,
  aic = NA,
  sh_eta_cl_pct = NA,
  sh_eta_v_pct = NA,
  sh_eps_pct = NA,
  convergence = sprintf("%s; exit_status=%d; timeout_sec=%d", reason, status, timeout_sec)
)

dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
write_json(result, dst, auto_unbox = TRUE, pretty = TRUE, na = "null")
