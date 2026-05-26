#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
root <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", file.path(repo_root, "results"))
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
scenarios <- strsplit(Sys.getenv("SCENARIOS", "scenario1 scenario2"), "\\s+")[[1]]
scenarios <- scenarios[nzchar(scenarios)]
out_dir <- Sys.getenv(
  "TABLE3_OUT_DIR",
  file.path(root, "reports", "main", "table3")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

parameters <- data.frame(
  source_name = c("KA", "CL", "V", "omega2_cl", "omega_cl_v", "omega2_v", "rho_cl_v", "sigma2_prop"),
  label = c(
    "$\\theta_{KA}$",
    "$\\theta_{CL}$",
    "$\\theta_V$",
    "$\\omega_{CL}^2$",
    "$\\omega_{CL,V}$",
    "$\\omega_V^2$",
    "$\\rho_{CL,V}$",
    "$\\sigma_{\\mathrm{prop}}^2\\times 10^3$"
  ),
  source_parameter = c("KA", "CL", "V", "omega2_cl", "omega_cl_v", "omega2_v", "rho_cl_v", "sigma2_prop"),
  scale_multiplier = c(1, 1, 1, 1, 1, 1, 1, 1000),
  stringsAsFactors = FALSE
)

as_bool <- function(x) {
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes")
}

summarize_values <- function(values) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  c(
    median = as.numeric(stats::quantile(values, 0.5, type = 7, names = FALSE, na.rm = TRUE)),
    `p2.5` = as.numeric(stats::quantile(values, 0.025, type = 7, names = FALSE, na.rm = TRUE)),
    `p97.5` = as.numeric(stats::quantile(values, 0.975, type = 7, names = FALSE, na.rm = TRUE))
  )
}

fmt_number <- function(x) {
  x <- as.numeric(x)
  if (!is.finite(x)) return("NA")
  if (identical(x, 0)) return("0")
  sprintf("%.3g", x)
}

read_federated_values <- function(scenario) {
  base <- file.path(root, "runs", scenario, "bootstrap", "federated", scenario, "raw_runs")
  estimate_paths <- sort(list.files(base, pattern = "^trial_estimates_wide\\.csv$", recursive = TRUE, full.names = TRUE))
  if (!length(estimate_paths)) {
    stop("Missing federated bootstrap files for ", scenario, ": ", base)
  }

  rows <- do.call(
    rbind,
    lapply(estimate_paths, function(path) read.csv(path, check.names = FALSE, stringsAsFactors = FALSE))
  )
  attempted <- nrow(rows)
  rows <- rows[as_bool(rows[["converged"]]), , drop = FALSE]

  values <- list(
    KA = exp(as.numeric(rows[["lka"]])),
    CL = exp(as.numeric(rows[["lcl"]])),
    V = exp(as.numeric(rows[["lvc"]])),
    omega2_cl = as.numeric(rows[["etaLcl"]]),
    omega_cl_v = as.numeric(rows[["(etaLcl,etaLvc)"]]),
    omega2_v = as.numeric(rows[["etaLvc"]]),
    rho_cl_v = as.numeric(rows[["rho_(etaLcl,etaLvc)"]]),
    sigma2_prop = as.numeric(rows[["CcPropSd"]])^2
  )
  list(attempted = attempted, successful = nrow(rows), values = values)
}

read_centralized_summary <- function(scenario) {
  path <- file.path(root, "runs", scenario, "bootstrap", "centralized", "aggregate", "summary_converged_only.csv")
  if (!file.exists(path)) stop("Missing centralized summary for ", scenario, ": ", path)
  rows <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  split(rows, rows[["parameter"]])
}

rows <- list()
sources <- list(successful_trials = list())

for (scenario in scenarios) {
  fed <- read_federated_values(scenario)
  sources[[paste0(scenario, "_federated_raw_runs")]] <-
    file.path(root, "runs", scenario, "bootstrap", "federated", scenario, "raw_runs")
  sources$successful_trials[[paste0(scenario, "_Federated")]] <- fed$successful

  for (i in seq_len(nrow(parameters))) {
    prm <- parameters[i, ]
    stats <- summarize_values(fed$values[[prm$source_name]] * prm$scale_multiplier)
    rows[[length(rows) + 1L]] <- data.frame(
      scenario = scenario,
      method = "Federated",
      successful = fed$successful,
      attempted = fed$attempted,
      parameter = prm$label,
      source_parameter = prm$source_parameter,
      scale_multiplier = prm$scale_multiplier,
      median = stats[["median"]],
      `p2.5` = stats[["p2.5"]],
      `p97.5` = stats[["p97.5"]],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  cent <- read_centralized_summary(scenario)
  sources[[paste0(scenario, "_centralized_summary")]] <-
    file.path(root, "runs", scenario, "bootstrap", "centralized", "aggregate", "summary_converged_only.csv")
  cent_success <- as.integer(cent[[1L]][["n_converged"]][1L])
  sources$successful_trials[[paste0(scenario, "_Centralized")]] <- cent_success

  for (i in seq_len(nrow(parameters))) {
    prm <- parameters[i, ]
    stats <- cent[[prm$source_name]]
    rows[[length(rows) + 1L]] <- data.frame(
      scenario = scenario,
      method = "Centralized",
      successful = cent_success,
      attempted = 500L,
      parameter = prm$label,
      source_parameter = prm$source_parameter,
      scale_multiplier = prm$scale_multiplier,
      median = as.numeric(stats[["median"]]) * prm$scale_multiplier,
      `p2.5` = as.numeric(stats[["ci_lower"]]) * prm$scale_multiplier,
      `p97.5` = as.numeric(stats[["ci_upper"]]) * prm$scale_multiplier,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
}

out <- do.call(rbind, rows)
csv_path <- file.path(out_dir, "table3.csv")
write.csv(out, csv_path, row.names = FALSE, quote = TRUE, na = "NA")

row_for <- function(scenario, method, parameter_label) {
  hit <- out[out$scenario == scenario & out$method == method & out$parameter == parameter_label, , drop = FALSE]
  if (!nrow(hit)) stop("missing row: ", scenario, " ", method, " ", parameter_label)
  hit[1L, , drop = FALSE]
}

headers <- c("Parameter")
for (scenario in scenarios) {
  for (method in c("Federated", "Centralized")) {
    key <- paste0(scenario, "_", method)
    headers <- c(headers, sprintf("%s<br>%s<br>(%d/500)", tools::toTitleCase(scenario), method, sources$successful_trials[[key]]))
  }
}

md_lines <- c(
  "# Table 3. Bootstrap median estimates and percentile intervals",
  "",
  "Values are median (2.5th, 97.5th percentiles) from successful bootstrap trials only.",
  "",
  paste0("| ", paste(headers, collapse = " | "), " |"),
  paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |")
)

for (label in parameters$label) {
  cells <- c(label)
  for (scenario in scenarios) {
    for (method in c("Federated", "Centralized")) {
      row <- row_for(scenario, method, label)
      cells <- c(cells, sprintf("%s<br>(%s, %s)", fmt_number(row$median), fmt_number(row[["p2.5"]]), fmt_number(row[["p97.5"]])))
    }
  }
  md_lines <- c(md_lines, paste0("| ", paste(cells, collapse = " | "), " |"))
}

md_lines <- c(
  md_lines,
  "",
  "KA, CL, and V are reported on the natural scale. Omega rows are variance/covariance-scale estimates. The sigma row is multiplied by 1000.",
  ""
)
md_path <- file.path(out_dir, "table3.md")
writeLines(md_lines, md_path, useBytes = TRUE)

sources$note <- "Successful converged bootstrap trials only; nonconverged trials are excluded from percentile summaries."
write_json(sources, file.path(out_dir, "sources.json"), auto_unbox = TRUE, pretty = TRUE)

pending <- file.path(out_dir, "table3_pending.md")
if (file.exists(pending)) unlink(pending)

cat(md_path, "\n")
