#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
ROOT <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", unset = dirname(script_path))
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)
REPORTS <- file.path(ROOT, "reports")
REPORTS_FIGURES <- file.path(REPORTS, "figures")
REPORTS_SUPP <- file.path(REPORTS, "supplementary")
dir.create(REPORTS_SUPP, recursive = TRUE, showWarnings = FALSE)

copy_file <- function(src, dst) {
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(src)) file.copy(src, dst, overwrite = TRUE) else warning("missing artifact: ", src)
}
write_text <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, path, useBytes = TRUE)
}
write_json_file <- function(path, payload) {
  write_text(path, toJSON(payload, auto_unbox = TRUE, pretty = TRUE))
}

bundle_output <- function(name, title, caption, artifacts, source_files) {
  out_dir <- file.path(REPORTS_SUPP, name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  copied <- character()
  for (src in artifacts) {
    dst <- file.path(out_dir, basename(src))
    copy_file(src, dst)
    copied <- c(copied, basename(dst))
  }
  write_json_file(file.path(out_dir, "sources.json"), list(title = title, artifacts = unname(copied), sources = unname(source_files)))
}

bundle_output(
  "supplement1_sitewise_concentration_profiles",
  "Supplement 1. Site-wise concentration-time profiles used in the current analysis.",
  "Observed deferiprone concentration data from the base datasets, shown against time after dose and colored by sampling design.",
  artifacts = file.path(REPORTS_FIGURES, c("supplement1.png", "supplement1.pdf", "supplement1.svg")),
  source_files = c("data/base/data1_1.csv", "data/base/data1_2.csv", "data/base/data1_3.csv")
)

supp_lines <- c("- Supplement 1: `supplement1_sitewise_concentration_profiles`")

if (file.exists(file.path(REPORTS_FIGURES, "figure3_theta_v_gradient_detail.png"))) {
  bundle_output(
    "supplement2_theta_v_gradient_detail",
    "Supplement 2. Detailed gradient trajectories for theta_V in the Federated approach.",
    "A theta_V-only view of the reporting-scale gradients from the standard federated base-model run.",
    artifacts = file.path(REPORTS_FIGURES, c("figure3_theta_v_gradient_detail.png", "figure3_theta_v_gradient_detail.pdf", "figure3_theta_v_gradient_detail.svg")),
    source_files = c("runs/scenario1/standard/federated/latest_run.txt")
  )
  supp_lines <- c(supp_lines, "- Supplement 2: `supplement2_theta_v_gradient_detail`")
}

if (file.exists(file.path(REPORTS_FIGURES, "supplement3_optimization_scale_gradient_trajectories.png"))) {
  bundle_output(
    "supplement3_optimization_scale_gradient_trajectories",
    "Supplement 3. Optimization-scale gradient trajectories in the Federated approach.",
    "Gradient trajectories with respect to the optimization parameters for Scenario 1 and Scenario 2. Panel labels indicate the corresponding reporting parameters; the actual gradients are on the optimization scale, including log fixed effects, Cholesky BSV parameters, and the transformed residual-error parameter. The plotting axis uses a signed-log scale for visualization while preserving the sign of each gradient.",
    artifacts = file.path(REPORTS_FIGURES, c("supplement3_optimization_scale_gradient_trajectories.png", "supplement3_optimization_scale_gradient_trajectories.pdf", "supplement3_optimization_scale_gradient_trajectories.svg")),
    source_files = c("runs/scenario1/standard/federated/latest_run.txt", "runs/scenario2/standard/federated/latest_run.txt")
  )
  supp_lines <- c(supp_lines, "- Supplement 3: `supplement3_optimization_scale_gradient_trajectories`")
}

bundle_output(
  "supplement4_time_concentration_profiles",
  "Supplement 4. Site-wise observed concentration-time profiles.",
  "Observed deferiprone concentration data from the base datasets, faceted by site and colored by sampling design.",
  artifacts = file.path(REPORTS_FIGURES, c("time_concentration_profiles.png", "time_concentration_profiles.pdf", "time_concentration_profiles.svg")),
  source_files = c("data/base/data1_1.csv", "data/base/data1_2.csv", "data/base/data1_3.csv")
)
supp_lines <- c(supp_lines, "- Supplement 4: `supplement4_time_concentration_profiles`")
