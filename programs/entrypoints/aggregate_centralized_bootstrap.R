#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

scenario <- Sys.getenv("SCENARIO")
canon <- Sys.getenv("CANON_OUT")
raw <- Sys.getenv("RAW_DIR")
start <- as.integer(Sys.getenv("TRIAL_START"))
end <- as.integer(Sys.getenv("TRIAL_END"))
maxit <- as.integer(as.numeric(Sys.getenv("MAXIT")))
pgtol <- as.numeric(Sys.getenv("PGTOL"))
factr <- as.numeric(Sys.getenv("FACTR"))

if (!nzchar(scenario) || !nzchar(canon) || !nzchar(raw)) {
  stop("SCENARIO, CANON_OUT, and RAW_DIR must be set")
}

as_bool <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  tolower(trimws(as.character(x))) == "true"
}

as_num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(out), out, NA_real_)
}

q7 <- function(values, p) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (!length(values)) return(NA_real_)
  as.numeric(stats::quantile(values, p, type = 7, names = FALSE, na.rm = TRUE))
}

rows <- vector("list", end - start + 1L)
idx <- 0L
for (trial in seq.int(start, end)) {
  idx <- idx + 1L
  path <- file.path(raw, sprintf("pooled_bootstrap_trial_%04d.json", trial))
  if (!file.exists(path)) {
    rows[[idx]] <- list(
      scenario = scenario,
      trial = trial,
      fit_ok = FALSE,
      converged = FALSE,
      convergence = "missing after rerun"
    )
  } else {
    rows[[idx]] <- fromJSON(path, simplifyVector = FALSE)
  }
}

columns <- c(
  "scenario", "trial", "n_patients", "fit_ok", "converged",
  "lka", "lcl", "lvc", "KA", "CL", "V",
  "omega2_cl", "omega_cl_v", "omega2_v", "rho_cl_v",
  "sigma_prop", "sigma2_prop", "objf", "aic",
  "sh_eta_cl_pct", "sh_eta_v_pct", "sh_eps_pct", "convergence"
)

to_row <- function(x) {
  out <- setNames(as.list(rep(NA, length(columns))), columns)
  for (nm in intersect(names(x), columns)) out[[nm]] <- x[[nm]]
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

trial_results <- do.call(rbind, lapply(rows, to_row))

agg_dir <- file.path(canon, "aggregate")
rep_dir <- file.path(canon, "reporting")
dir.create(agg_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rep_dir, recursive = TRUE, showWarnings = FALSE)
write.table(
  trial_results,
  file.path(agg_dir, "trial_results.csv"),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

conv <- rows[vapply(rows, function(row) as_bool(row$converged), logical(1))]
metrics <- c("KA", "CL", "V", "omega2_cl", "omega_cl_v", "omega2_v", "rho_cl_v", "sigma2_prop")
summary_rows <- lapply(metrics, function(metric) {
  vals <- vapply(conv, function(row) as_num(row[[metric]]), numeric(1))
  vals <- vals[is.finite(vals)]
  data.frame(
    parameter = metric,
    n_converged = length(vals),
    median = q7(vals, 0.5),
    ci_lower = q7(vals, 0.025),
    ci_upper = q7(vals, 0.975),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
})
summary_df <- do.call(rbind, summary_rows)
write.table(
  summary_df,
  file.path(agg_dir, "summary_converged_only.csv"),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

fmt <- function(x) {
  x <- as.numeric(x)
  if (!is.finite(x)) return("NA")
  sprintf("%.6g", x)
}

lines <- c(
  sprintf("# Centralized Bootstrap Summary: %s", scenario),
  "",
  sprintf("- attempted trials: %d", length(rows)),
  sprintf("- converged trials: %d", length(conv)),
  "",
  "|parameter|n_converged|median|ci_lower|ci_upper|",
  "|---|---:|---:|---:|---:|"
)
for (i in seq_len(nrow(summary_df))) {
  row <- summary_df[i, ]
  lines <- c(
    lines,
    sprintf(
      "|%s|%d|%s|%s|%s|",
      row$parameter,
      row$n_converged,
      fmt(row$median),
      fmt(row$ci_lower),
      fmt(row$ci_upper)
    )
  )
}
writeLines(lines, file.path(rep_dir, "summary_converged_only.md"), useBytes = TRUE)

meta <- list(
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  scenario = scenario,
  trial_start = start,
  trial_end = end,
  attempted = length(rows),
  converged = length(conv),
  model = "deferiprone oral 1-compartment base model; centralized pooled bootstrap",
  control = list(
    outerOpt = "L-BFGS-B",
    maxOuterIterations = maxit,
    lbfgsPgtol = pgtol,
    lbfgsFactr = factr,
    covMethod = ""
  )
)
write_json(meta, file.path(canon, "run_meta.json"), auto_unbox = TRUE, pretty = TRUE)
cat(paste(lines, collapse = "\n"), "\n")
