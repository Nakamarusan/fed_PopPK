#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && !length(a))) b else a

repo_host <- normalizePath("/project", winslash = "/", mustWork = FALSE)
repo_container <- "/project"

map_run_path <- function(path) {
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  if (startsWith(path, repo_host)) {
    mapped <- sub(paste0("^", repo_host), repo_container, path)
    if (file.exists(mapped)) return(normalizePath(mapped, winslash = "/", mustWork = FALSE))
  }
  if (startsWith(path, repo_container)) {
    mapped <- sub(paste0("^", repo_container), repo_host, path)
    if (file.exists(mapped)) return(normalizePath(mapped, winslash = "/", mustWork = FALSE))
  }
  path
}

fmt <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1L]])) return("NA")
  if (x == 0) return("0")
  if (abs(x) < 1e-4) return(sprintf("%.3e", x))
  format(signif(x, 6), scientific = FALSE, trim = TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("usage: format_federated_outputs.R <run_dir> <reporting_dir>", call. = FALSE)
}

run_dir <- map_run_path(args[[1L]])
reporting_dir <- map_run_path(args[[2L]])
dir.create(reporting_dir, recursive = TRUE, showWarnings = FALSE)

result <- fromJSON(file.path(run_dir, "result.json"), simplifyVector = TRUE)
nat <- result$natPar
meta <- result$meta
control <- result$control

sigma_prop <- suppressWarnings(as.numeric(nat$CcPropSd))
omega2_cl <- if (!is.null(nat$etaLcl)) as.numeric(nat$etaLcl) else NaN
omega2_v <- if (!is.null(nat$etaLvc)) as.numeric(nat$etaLvc) else NaN
omega_cl_v <- if (!is.null(nat[["(etaLcl,etaLvc)"]])) {
  as.numeric(nat[["(etaLcl,etaLvc)"]])
} else if (is.finite(omega2_cl) && is.finite(omega2_v)) {
  0
} else {
  NaN
}
rho_cl_v <- if (!is.null(nat[["rho_(etaLcl,etaLvc)"]])) {
  as.numeric(nat[["rho_(etaLcl,etaLvc)"]])
} else if (is.finite(omega_cl_v) && is.finite(omega2_cl) && is.finite(omega2_v) && omega2_cl > 0 && omega2_v > 0) {
  omega_cl_v / sqrt(omega2_cl * omega2_v)
} else {
  NaN
}
row <- data.frame(
  run_id = meta$run_id %||% "",
  convergence = result$convergence %||% "",
  message = result$message %||% "",
  objective_value = result$value %||% "",
  KA = if (!is.null(nat$lka)) exp(as.numeric(nat$lka)) else NaN,
  CL = if (!is.null(nat$lcl)) exp(as.numeric(nat$lcl)) else NaN,
  V = if (!is.null(nat$lvc)) exp(as.numeric(nat$lvc)) else NaN,
  omega2_cl = omega2_cl,
  omega_cl_v = omega_cl_v,
  omega2_v = omega2_v,
  rho_cl_v = rho_cl_v,
  sigma_prop = sigma_prop,
  sigma2_prop = if (is.finite(sigma_prop)) sigma_prop^2 else NaN,
  sigma2_prop_x1e3 = if (is.finite(sigma_prop)) sigma_prop^2 * 1000 else NaN,
  maxit = control$maxit %||% "",
  pgtol = control$pgtol %||% "",
  factr = control$factr %||% "",
  trace = control$trace %||% "",
  grad_cores = meta$grad_cores %||% "",
  stringsAsFactors = FALSE
)

write.csv(row, file.path(reporting_dir, "federated_summary_natural_sigma2.csv"), row.names = FALSE, na = "")

lines <- c(
  "# Federated Deferiprone Base Model Summary",
  "",
  "KA, CL, and V are reported on the natural scale. The model does not include a covariate term.",
  "",
  "|run_id|conv|KA|CL|V|omega2_CL|omega_CL,V|omega2_V|rho_CL,V|sigma2_prop (sd)|OBJF|message|",
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|",
  paste0(
    "|", row$run_id,
    "|", row$convergence,
    "|", fmt(row$KA),
    "|", fmt(row$CL),
    "|", fmt(row$V),
    "|", fmt(row$omega2_cl),
    "|", fmt(row$omega_cl_v),
    "|", fmt(row$omega2_v),
    "|", fmt(row$rho_cl_v),
    "|", fmt(row$sigma2_prop), " (", fmt(row$sigma_prop), ")",
    "|", fmt(row$objective_value),
    "|", gsub("\\|", "/", as.character(row$message)),
    "|"
  ),
  "",
  "Optimizer control:",
  "",
  paste0("- `maxit = ", row$maxit, "`"),
  paste0("- `pgtol = ", row$pgtol, "`"),
  paste0("- `factr = ", row$factr, "`"),
  paste0("- `trace = ", row$trace, "`"),
  paste0("- `grad_cores = ", row$grad_cores, "`"),
  ""
)
writeLines(lines, file.path(reporting_dir, "federated_summary_natural_sigma2.md"))
