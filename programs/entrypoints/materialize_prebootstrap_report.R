#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && !length(a))) b else a

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
ROOT <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", unset = dirname(script_path))
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

fmt <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1L]])) return("NA")
  if (x == 0) return("0")
  if (abs(x) < 1e-4) return(sprintf("%.3e", x))
  format(signif(x, 6), scientific = FALSE, trim = TRUE)
}

read_optional <- function(path) {
  if (!file.exists(path)) return(NULL)
  fread(path)
}
parse_scenarios <- function(default = c("scenario1", "scenario2")) {
  raw <- trimws(Sys.getenv("SCENARIOS", ""))
  if (!nzchar(raw)) return(default)
  vals <- strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]
  vals[nzchar(vals)]
}

table_dir <- ensure_dir(file.path(ROOT, "reports", "tables"))
main_dir <- ensure_dir(file.path(ROOT, "reports", "main"))

rows <- list()
for (scenario in parse_scenarios()) {
  cent <- read_optional(file.path(ROOT, "runs", scenario, "standard", "centralized", "raw", "fit_estimates.csv"))
  fed <- read_optional(file.path(ROOT, "runs", scenario, "standard", "federated", "reporting", "federated_summary_natural_sigma2.csv"))
  n_all <- NA_integer_
  if (!is.null(cent) && nrow(cent) && any(cent$dataset == "ALL")) {
    n_all <- as.integer(cent[dataset == "ALL", n_patients][[1L]])
  }

  if (!is.null(cent) && nrow(cent)) {
    rows[[length(rows) + 1L]] <- cent[, .(
    scenario,
    method = fifelse(dataset == "ALL", "Centralized", paste0("Local-", dataset)),
    dataset,
    fit_ok,
    n_patients,
    KA = theta_ka,
    CL = theta_cl,
    V = theta_v,
    omega2_CL = omega2_cl,
    omega_CL_V = omega_cl_v,
    omega2_V = omega2_v,
    rho_CL_V = rho_cl_v,
    sigma2_prop,
    sigma_prop,
    objective_value = objf,
    message = convergence
  )]
  }
  if (!is.null(fed) && nrow(fed)) {
    rows[[length(rows) + 1L]] <- fed[, .(
    scenario,
    method = "Federated",
    dataset = "ALL",
    fit_ok = convergence == 0,
    n_patients = n_all,
    KA,
    CL,
    V,
    omega2_CL = omega2_cl,
    omega_CL_V = omega_cl_v,
    omega2_V = omega2_v,
    rho_CL_V = rho_cl_v,
    sigma2_prop,
    sigma_prop,
    objective_value,
    message
  )]
  }
}

summary <- if (length(rows)) rbindlist(rows, fill = TRUE) else data.table()
csv_path <- file.path(table_dir, "prebootstrap_base_model_summary.csv")
fwrite(summary, csv_path)

lines <- c(
  "# Prebootstrap Base Model Summary",
  "",
  "The deferiprone base model is a one-compartment oral model with first-order absorption and first-order elimination. No covariate model is fitted. DV is modeled in umol/L by multiplying `linCmt()` by `1000 / 139.15`.",
  "",
  "|Scenario|Method|Dataset|fit_ok|n|KA|CL|V|omega2_CL|omega_CL,V|omega2_V|rho_CL,V|sigma2_prop (sd)|OBJF|message|",
  "|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
)
if (nrow(summary)) {
  for (i in seq_len(nrow(summary))) {
    r <- summary[i]
    lines <- c(lines, sprintf(
      "|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s (%s)|%s|%s|",
      r$scenario,
      r$method,
      r$dataset,
      ifelse(isTRUE(r$fit_ok), "TRUE", "FALSE"),
      ifelse(is.na(r$n_patients), "", as.character(r$n_patients)),
      fmt(r$KA), fmt(r$CL), fmt(r$V),
      fmt(r$omega2_CL), fmt(r$omega_CL_V), fmt(r$omega2_V), fmt(r$rho_CL_V),
      fmt(r$sigma2_prop), fmt(r$sigma_prop),
      fmt(r$objective_value),
      gsub("\\|", "/", as.character(r$message %||% ""))
    ))
  }
}
md_path <- file.path(table_dir, "prebootstrap_base_model_summary.md")
writeLines(lines, md_path)

message("Wrote: ", csv_path)
message("Wrote: ", md_path)
