#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
ROOT <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", unset = dirname(script_path))
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)
REPORTS <- file.path(ROOT, "reports")
REPORTS_MAIN <- file.path(REPORTS, "main")
REPORTS_TABLES <- file.path(REPORTS, "tables")
REPORTS_FIGURES <- file.path(REPORTS, "figures")
SITE_ORDER <- c("Site1", "Site2", "Site3")
parse_scenarios <- function(default = c("scenario1", "scenario2")) {
  raw <- trimws(Sys.getenv("SCENARIOS", ""))
  if (!nzchar(raw)) return(default)
  vals <- strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]
  vals[nzchar(vals)]
}
SCENARIOS <- parse_scenarios()
read_design_labels <- function() {
  meta_path <- file.path(ROOT, "manifests", "generation_meta.json")
  fallback <- c(
    D1 = "D1 (0.25, 0.5, 1.0) h",
    D2 = "D2 (0.5, 2.0, 5.0) h",
    D3 = "D3 (2.0, 4.0, 6.0) h"
  )
  if (!file.exists(meta_path)) return(fallback)

  meta <- tryCatch(fromJSON(meta_path, simplifyVector = FALSE), error = function(e) NULL)
  times <- meta$design$sample_times_h %||% NULL
  if (is.null(times)) return(fallback)

  labels <- vapply(names(times), function(design) {
    vals <- unlist(times[[design]], use.names = FALSE)
    vals <- as.numeric(vals)
    paste0(design, " (", paste(format(vals, trim = TRUE, scientific = FALSE), collapse = ", "), ") h")
  }, character(1))
  labels[intersect(c("D1", "D2", "D3"), names(labels))]
}
DESIGN_LABELS <- read_design_labels()

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}
write_csv_rows <- function(path, rows) {
  ensure_dir(dirname(path))
  write.csv(rows, path, row.names = FALSE, na = "")
}
write_text <- function(path, text) {
  ensure_dir(dirname(path))
  writeLines(text, path, useBytes = TRUE)
}
write_json_file <- function(path, payload) {
  write_text(path, toJSON(payload, auto_unbox = TRUE, pretty = TRUE))
}
fmt_sig <- function(value, sig = 3) {
  x <- suppressWarnings(as.numeric(value))
  if (!length(x) || !is.finite(x)) return("NA")
  if (x == 0) return("0")
  format(signif(x, sig), scientific = FALSE, trim = TRUE)
}
markdown_table <- function(df) {
  header <- paste0("|", paste(names(df), collapse = "|"), "|")
  sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  body <- apply(df, 1, function(row) paste0("|", paste(row, collapse = "|"), "|"))
  paste(c(header, sep, body), collapse = "\n")
}
copy_file <- function(src, dst) {
  ensure_dir(dirname(dst))
  if (file.exists(src)) file.copy(src, dst, overwrite = TRUE) else warning("missing artifact: ", src)
}

build_table1 <- function() {
  scenario_ids_path <- function(scenario) {
    p <- file.path(ROOT, "data", scenario, "meta", "id_params.csv")
    if (file.exists(p)) p else file.path(ROOT, "data", "meta", "id_params.csv")
  }
  site_schedule <- function(counts, site) {
    site_rows <- counts[Site == site]
    total <- sum(site_rows$n_subjects)
    design_counts <- setNames(site_rows$n_subjects, site_rows$design)
    parts <- names(design_counts)[match(c("D1", "D2", "D3"), names(design_counts), nomatch = 0L)]
    sched <- paste(sprintf("%s (%s)", parts, design_counts[parts]), collapse = ", ")
    list(total = as.character(total), sched = sched)
  }
  rows <- data.frame(Site = SITE_ORDER, stringsAsFactors = FALSE, check.names = FALSE)
  for (scenario in SCENARIOS) {
    ids <- fread(scenario_ids_path(scenario))
    counts <- ids[, .(n_subjects = .N), by = .(Site, design)]
    col_prefix <- sub("^scenario", "Scenario ", scenario)
    vals <- lapply(SITE_ORDER, function(site) site_schedule(counts, site))
    rows[[paste0(col_prefix, " Subjects")]] <- vapply(vals, `[[`, character(1), "total")
    rows[[paste0(col_prefix, " Sampling Schedules")]] <- vapply(vals, `[[`, character(1), "sched")
  }
  csv_path <- file.path(REPORTS_TABLES, "table1_site_allocation_sampling_design_distribution.csv")
  md_path <- file.path(REPORTS_TABLES, "table1_site_allocation_sampling_design_distribution.md")
  write_csv_rows(csv_path, rows)
  write_text(md_path, c(
    "# Table 1. Site-wise subject allocation and sampling design distribution under the simulation scenario.",
    "",
    markdown_table(rows),
    "",
    "Values in parentheses indicate the number of subjects assigned to each sampling schedule (D1-D3) within each site.",
    paste0("The sampling schedules were: ", paste(unname(DESIGN_LABELS), collapse = "; "), "."),
    ""
  ))
  list(csv = csv_path, md = md_path)
}

read_fed <- function(scenario) {
  posthoc_path <- file.path(ROOT, "runs", scenario, "standard", "federated", "posthoc_eval", "federated_pooled_eval.csv")
  if (file.exists(posthoc_path)) return(fread(posthoc_path)[1])
  fed_path <- file.path(ROOT, "runs", scenario, "standard", "federated", "reporting", "federated_summary_natural_sigma2.csv")
  if (!file.exists(fed_path)) return(NULL)
  x <- fread(fed_path)[1]
  data.table(
    KA = x$KA,
    CL = x$CL,
    V = x$V,
    omega2_cl = x$omega2_cl,
    omega_cl_v = if ("omega_cl_v" %in% names(x)) x$omega_cl_v else NA_real_,
    omega2_v = x$omega2_v,
    rho_cl_v = if ("rho_cl_v" %in% names(x)) x$rho_cl_v else NA_real_,
    sigma2_prop = x$sigma2_prop,
    sh_eta_cl_pct = NA_real_,
    sh_eta_v_pct = NA_real_
  )
}

build_table2 <- function() {
  fed_available <- vapply(SCENARIOS, function(scenario) !is.null(read_fed(scenario)), logical(1))
  methods <- c(if (any(fed_available)) "Federated", "Site1", "Site2", "Site3", "Weighted", "Centralized")
  approach_label <- if (any(fed_available)) "Federated, Weighted, and Centralized" else "Site-wise, Weighted, and Centralized"
  scenario_values <- list()

  for (scenario in SCENARIOS) {
    fit <- fread(file.path(ROOT, "runs", scenario, "standard", "centralized", "reporting", "fit_estimates.csv"))
    pooled <- fread(file.path(ROOT, "runs", scenario, "standard", "centralized", "reporting", "pooled_summary.csv"))
    fit_map <- split(fit, fit$dataset)
    pool_map <- split(pooled, pooled$parameter)
    fed <- read_fed(scenario)

    get_fit <- function(dataset, field) {
      if (is.null(fit_map[[dataset]]) || !(field %in% names(fit_map[[dataset]]))) return(NA_real_)
      suppressWarnings(as.numeric(fit_map[[dataset]][[field]][1]))
    }
    get_pool <- function(parameter, field) {
      if (is.null(pool_map[[parameter]]) || !(field %in% names(pool_map[[parameter]]))) return(NA_real_)
      suppressWarnings(as.numeric(pool_map[[parameter]][[field]][1]))
    }
    fed_val <- function(field) {
      if (is.null(fed) || !(field %in% names(fed))) return(NA_real_)
      suppressWarnings(as.numeric(fed[[field]][1]))
    }
    scenario_values[[scenario]] <- list(
    KA = list(
      Federated = fed_val("KA"),
      Site1 = get_fit("Site1", "theta_ka"),
      Site2 = get_fit("Site2", "theta_ka"),
      Site3 = get_fit("Site3", "theta_ka"),
      Weighted = get_pool("theta_ka", "pooled_mean"),
      Centralized = get_fit("ALL", "theta_ka")
    ),
    CL = list(
      Federated = fed_val("CL"),
      Site1 = get_fit("Site1", "theta_cl"),
      Site2 = get_fit("Site2", "theta_cl"),
      Site3 = get_fit("Site3", "theta_cl"),
      Weighted = get_pool("theta_cl", "pooled_mean"),
      Centralized = get_fit("ALL", "theta_cl")
    ),
    V = list(
      Federated = fed_val("V"),
      Site1 = get_fit("Site1", "theta_v"),
      Site2 = get_fit("Site2", "theta_v"),
      Site3 = get_fit("Site3", "theta_v"),
      Weighted = get_pool("theta_v", "pooled_mean"),
      Centralized = get_fit("ALL", "theta_v")
    ),
    omega2_cl = list(
      Federated = fed_val("omega2_cl"),
      Site1 = get_fit("Site1", "omega2_cl"),
      Site2 = get_fit("Site2", "omega2_cl"),
      Site3 = get_fit("Site3", "omega2_cl"),
      Weighted = get_pool("var_total_cl", "total_var"),
      Centralized = get_fit("ALL", "omega2_cl")
    ),
    omega_cl_v = list(
      Federated = fed_val("omega_cl_v"),
      Site1 = get_fit("Site1", "omega_cl_v"),
      Site2 = get_fit("Site2", "omega_cl_v"),
      Site3 = get_fit("Site3", "omega_cl_v"),
      Weighted = get_pool("cov_total_cl_v", "total_var"),
      Centralized = get_fit("ALL", "omega_cl_v")
    ),
    omega2_v = list(
      Federated = fed_val("omega2_v"),
      Site1 = get_fit("Site1", "omega2_v"),
      Site2 = get_fit("Site2", "omega2_v"),
      Site3 = get_fit("Site3", "omega2_v"),
      Weighted = get_pool("var_total_v", "total_var"),
      Centralized = get_fit("ALL", "omega2_v")
    ),
    rho_cl_v = list(
      Federated = fed_val("rho_cl_v"),
      Site1 = get_fit("Site1", "rho_cl_v"),
      Site2 = get_fit("Site2", "rho_cl_v"),
      Site3 = get_fit("Site3", "rho_cl_v"),
      Weighted = get_pool("rho_total_cl_v", "pooled_mean"),
      Centralized = get_fit("ALL", "rho_cl_v")
    ),
    sigma2_prop = list(
      Federated = fed_val("sigma2_prop"),
      Site1 = get_fit("Site1", "sigma2_prop"),
      Site2 = get_fit("Site2", "sigma2_prop"),
      Site3 = get_fit("Site3", "sigma2_prop"),
      Weighted = get_pool("sigma_prop", "within_var"),
      Centralized = get_fit("ALL", "sigma2_prop")
    ),
    sh_eta_cl_pct = list(
      Federated = fed_val("sh_eta_cl_pct"),
      Site1 = get_fit("Site1", "omega2_cl_shrink"),
      Site2 = get_fit("Site2", "omega2_cl_shrink"),
      Site3 = get_fit("Site3", "omega2_cl_shrink"),
      Weighted = NA_real_,
      Centralized = get_fit("ALL", "omega2_cl_shrink")
    ),
    sh_eta_v_pct = list(
      Federated = fed_val("sh_eta_v_pct"),
      Site1 = get_fit("Site1", "omega2_v_shrink"),
      Site2 = get_fit("Site2", "omega2_v_shrink"),
      Site3 = get_fit("Site3", "omega2_v_shrink"),
      Weighted = NA_real_,
      Centralized = get_fit("ALL", "omega2_v_shrink")
    )
  )
  }

  parameter_order <- c("KA", "CL", "V", "omega2_cl", "omega_cl_v", "omega2_v", "rho_cl_v", "sigma2_prop", "sh_eta_cl_pct", "sh_eta_v_pct")
  raw_rows <- list()
  md_rows <- list()
  for (param_key in parameter_order) {
    raw_row <- list(Parameter = param_key)
    md_row <- list(Parameter = param_key)
    for (scenario in SCENARIOS) {
      scenario_label <- sub("^scenario", "Scenario", scenario)
      vals <- scenario_values[[scenario]]
      for (method in methods) {
        col <- paste(scenario_label, method)
        value <- vals[[param_key]][[method]]
        raw_row[[col]] <- if (is.finite(value)) value else ""
        md_row[[col]] <- fmt_sig(value)
      }
    }
    raw_rows[[length(raw_rows) + 1L]] <- as.data.frame(raw_row, check.names = FALSE, stringsAsFactors = FALSE)
    md_rows[[length(md_rows) + 1L]] <- as.data.frame(md_row, check.names = FALSE, stringsAsFactors = FALSE)
  }
  raw_df <- do.call(rbind, raw_rows)
  md_df <- do.call(rbind, md_rows)

  stems <- c(
    "table2_parameter_estimates_eta_shrinkage",
    paste0("table2_", SCENARIOS, "_parameter_estimates_eta_shrinkage")
  )
  outputs <- list()
  for (stem in stems) {
    raw_use <- raw_df
    md_use <- md_df
    if (grepl("scenario1", stem)) {
      keep <- c("Parameter", grep("^Scenario1 ", names(raw_df), value = TRUE))
      raw_use <- raw_df[, keep, drop = FALSE]
      md_use <- md_df[, keep, drop = FALSE]
    } else if (grepl("scenario2", stem)) {
      keep <- c("Parameter", grep("^Scenario2 ", names(raw_df), value = TRUE))
      raw_use <- raw_df[, keep, drop = FALSE]
      md_use <- md_df[, keep, drop = FALSE]
    }
    raw_csv <- file.path(REPORTS_TABLES, paste0(stem, "_raw.csv"))
    csv_path <- file.path(REPORTS_TABLES, paste0(stem, ".csv"))
    md_path <- file.path(REPORTS_TABLES, paste0(stem, ".md"))
    raw_md <- file.path(REPORTS_TABLES, paste0(stem, "_raw.md"))
    write_csv_rows(raw_csv, raw_use)
    write_csv_rows(csv_path, md_use)
    title <- if (grepl("scenario1", stem)) {
      paste0("# Table 2. Parameter estimates and eta-shrinkage under ", approach_label, " approaches in Scenario 1.")
    } else if (grepl("scenario2", stem)) {
      paste0("# Table 2. Parameter estimates and eta-shrinkage under ", approach_label, " approaches in Scenario 2.")
    } else {
      paste0("# Table 2. Parameter estimates and eta-shrinkage under ", approach_label, " approaches.")
    }
    write_text(raw_md, c("# Table 2 Raw", "", markdown_table(raw_use), ""))
    write_text(md_path, c(
      title,
      "",
      markdown_table(md_use),
      "",
      "KA, CL, and V are reported on the natural scale. omega and sigma rows are reported as variance/covariance unless rho is explicitly indicated.",
      ""
    ))
    outputs[[stem]] <- list(csv = csv_path, md = md_path, raw = raw_csv)
  }
  outputs
}

build_table3_pending <- function() {
  out_dir <- file.path(REPORTS_MAIN, "table3")
  ensure_dir(out_dir)
  pending <- file.path(out_dir, "table3_pending.md")
  write_text(pending, c("# Table 3 Pending", "", "Bootstrap was not run in this workspace.", ""))
}

bundle_output <- function(name, title, caption, artifacts, source_files, artifact_names = NULL) {
  out_dir <- file.path(REPORTS_MAIN, name)
  ensure_dir(out_dir)
  if (is.null(artifact_names)) artifact_names <- basename(artifacts)
  if (length(artifact_names) != length(artifacts)) stop("artifact_names must match artifacts length")
  copied <- character()
  for (idx in seq_along(artifacts)) {
    src <- artifacts[[idx]]
    dst <- file.path(out_dir, artifact_names[[idx]])
    copy_file(src, dst)
    copied <- c(copied, basename(dst))
  }
  write_json_file(file.path(out_dir, "sources.json"), list(title = title, artifacts = unname(copied), sources = unname(source_files)))
}

write_main_index <- function() {
  invisible(NULL)
}

main <- function() {
  ensure_dir(REPORTS_MAIN)
  ensure_dir(REPORTS_TABLES)
  ensure_dir(REPORTS_FIGURES)
  table1 <- build_table1()
  table2 <- build_table2()
  build_table3_pending()
  has_fed_output <- any(vapply(SCENARIOS, function(scenario) !is.null(read_fed(scenario)), logical(1)))

  bundle_output(
    "figure1",
    "Figure 1. Sequence diagram of the federated framework",
    "Static sequence diagram reused for the current deferiprone analysis workspace.",
    artifacts = file.path(REPORTS_FIGURES, c(
      "figure1_federated_framework_sequence_diagram.pdf",
      "figure1_federated_framework_sequence_diagram.svg",
      "figure1_federated_framework_sequence_diagram_notes.md"
    )),
    source_files = c("reports/figures/figure1_federated_framework_sequence_diagram.pdf"),
    artifact_names = c("figure1.pdf", "figure1.svg", "notes.md")
  )
  bundle_output(
    "figure2",
    "Figure 2. Site-wise distributions of simulated individual CL, V, and body weight.",
    "Boxplots summarize the generated individual CL, V, and body weight by site and scenario in the deferiprone single-dose analysis.",
    artifacts = file.path(REPORTS_FIGURES, c(
      "figure2_sitewise_cl_v_wt_distributions.png",
      "figure2_sitewise_cl_v_wt_distributions.pdf",
      "figure2_sitewise_cl_v_wt_distributions.svg"
    )),
    source_files = c(
      "data/scenario1/meta/id_params.csv",
      "data/scenario2/meta/id_params.csv"
    ),
    artifact_names = c("figure2.png", "figure2.pdf", "figure2.svg")
  )
  if (has_fed_output && file.exists(file.path(REPORTS_FIGURES, "figure3_gradient_trajectories.png"))) {
    bundle_output(
      "figure3",
      "Figure 3. Site-specific and aggregated gradient trajectories during optimization in the Federated approach.",
      "Gradients from the standard federated base-model runs for Scenario 1 and Scenario 2. Fixed-effect gradients are shown on the log fixed-effect scale; BSV gradients are transformed from Cholesky parameters to omega variance/covariance parameters; RUV gradients are transformed to sigma_prop^2. The plotting axis uses a signed log10 display transform.",
      artifacts = file.path(REPORTS_FIGURES, c(
        "figure3_gradient_trajectories.png",
        "figure3_gradient_trajectories.pdf",
        "figure3_gradient_trajectories.svg"
      )),
      source_files = file.path("runs", SCENARIOS, "standard", "federated", "latest_run.txt"),
      artifact_names = c("figure3.png", "figure3.pdf", "figure3.svg")
    )
  }
  if (file.exists(file.path(REPORTS_FIGURES, "supplement1_dv_time_all_designs.svg"))) {
    bundle_output(
      "supplement1",
      "Supplement 1. DV-time profiles across sampling designs.",
      "Observed concentrations and design-level median profiles are colored by sampling design.",
      artifacts = file.path(REPORTS_FIGURES, c(
        "supplement1_dv_time_all_designs.png",
        "supplement1_dv_time_all_designs.pdf",
        "supplement1_dv_time_all_designs.svg"
      )),
      source_files = c(
        "data/scenario1/base/data1_1.csv",
        "data/scenario1/base/data1_2.csv",
        "data/scenario1/base/data1_3.csv",
        "reports/figures/dv_time_all_designs.svg"
      ),
      artifact_names = c("supplement1.png", "supplement1.pdf", "supplement1.svg")
    )
  }
  if (file.exists(file.path(REPORTS_FIGURES, "supplement2_scenario1_dv_time_by_design.svg"))) {
    bundle_output(
      "supplement2",
      "Supplement 2. Scenario 1 DV-time profiles by sampling design.",
      "Observed concentrations are faceted by site and colored by sampling design.",
      artifacts = file.path(REPORTS_FIGURES, c(
        "supplement2_scenario1_dv_time_by_design.png",
        "supplement2_scenario1_dv_time_by_design.pdf",
        "supplement2_scenario1_dv_time_by_design.svg"
      )),
      source_files = c(
        "data/scenario1/base/data1_1.csv",
        "data/scenario1/base/data1_2.csv",
        "data/scenario1/base/data1_3.csv",
        "reports/figures/scenario1/dv_time_by_design.svg"
      ),
      artifact_names = c("supplement2.png", "supplement2.pdf", "supplement2.svg")
    )
  }
  if (file.exists(file.path(REPORTS_FIGURES, "supplement3_scenario2_dv_time_by_design.svg"))) {
    bundle_output(
      "supplement3",
      "Supplement 3. Scenario 2 DV-time profiles by sampling design.",
      "Observed concentrations are faceted by site and colored by sampling design.",
      artifacts = file.path(REPORTS_FIGURES, c(
        "supplement3_scenario2_dv_time_by_design.png",
        "supplement3_scenario2_dv_time_by_design.pdf",
        "supplement3_scenario2_dv_time_by_design.svg"
      )),
      source_files = c(
        "data/scenario2/base/data1_1.csv",
        "data/scenario2/base/data1_2.csv",
        "data/scenario2/base/data1_3.csv",
        "reports/figures/scenario2/dv_time_by_design.svg"
      ),
      artifact_names = c("supplement3.png", "supplement3.pdf", "supplement3.svg")
    )
  }
  bundle_output(
    "table1",
    "Table 1. Site-wise subject allocation and sampling design distribution.",
    "Derived from the generated deferiprone meta data.",
    artifacts = c(table1$md, table1$csv),
    source_files = c("data/meta/id_params.csv"),
    artifact_names = c("table1.md", "table1.csv")
  )
  bundle_output(
    "table2",
    if (has_fed_output) "Table 2. Parameter estimates and eta-shrinkage under Federated, Weighted, and Centralized approaches." else "Table 2. Parameter estimates and eta-shrinkage under Site-wise, Weighted, and Centralized approaches.",
    "KA/CL/V are on the natural scale. Omega variance/covariance, rho, and sigma variance are included. No covariate model was fitted.",
    artifacts = c(table2$table2_parameter_estimates_eta_shrinkage$md, table2$table2_parameter_estimates_eta_shrinkage$csv, table2$table2_parameter_estimates_eta_shrinkage$raw),
    source_files = c(
      "runs/scenario1/standard/centralized/reporting/fit_estimates.csv",
      "runs/scenario1/standard/centralized/reporting/pooled_summary.csv",
      if (has_fed_output) "runs/scenario1/standard/federated/reporting/federated_summary_natural_sigma2.csv" else character(),
      if (has_fed_output) "runs/scenario1/standard/federated/posthoc_eval/federated_pooled_eval.csv" else character(),
      "runs/scenario2/standard/centralized/reporting/fit_estimates.csv",
      "runs/scenario2/standard/centralized/reporting/pooled_summary.csv",
      if (has_fed_output) "runs/scenario2/standard/federated/reporting/federated_summary_natural_sigma2.csv" else character(),
      if (has_fed_output) "runs/scenario2/standard/federated/posthoc_eval/federated_pooled_eval.csv" else character()
    ),
    artifact_names = c("table2.md", "table2.csv", "raw.csv")
  )
  write_main_index()
}

main()
