#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
SCRIPT_ROOT <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
ROOT <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", unset = SCRIPT_ROOT)
dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})
fedpoppk_source("R/client/bootstrap_sampling.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("usage: materialize_bootstrap_trials.R <scenario> <out_root> [b] [seed_base]", call. = FALSE)
}

scenario <- args[[1L]]
out_root <- normalizePath(args[[2L]], winslash = "/", mustWork = FALSE)
b <- if (length(args) >= 3L) as.integer(args[[3L]]) else 500L
seed_base <- if (length(args) >= 4L) as.integer(args[[4L]]) else 4869L

if (!scenario %in% c("scenario1", "scenario2")) stop("scenario must be scenario1 or scenario2")
if (!is.finite(b) || b < 1L) stop("b must be >= 1")
if (!is.finite(seed_base)) stop("seed_base must be finite")

input_root <- file.path(ROOT, "data", scenario, "base")
split_files <- file.path(input_root, sprintf("data1_%d.csv", 1:3))
missing <- split_files[!file.exists(split_files)]
if (length(missing)) stop("missing base files: ", paste(missing, collapse = ", "))

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

base_list <- lapply(split_files, fread)
names(base_list) <- sprintf("data1_%d", seq_along(base_list))

materialize_one <- function(trial_id) {
  trial_dir <- file.path(out_root, sprintf("trial_%04d", trial_id))
  dir.create(trial_dir, recursive = TRUE, showWarnings = FALSE)

  trial_seed_master <- seed_base + trial_id * 10000L
  trial_meta <- list(
    scenario = scenario,
    trial_id = trial_id,
    seed_base = seed_base,
    trial_seed_master = trial_seed_master,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  for (idx in seq_along(base_list)) {
    stem <- names(base_list)[[idx]]
    dt <- base_list[[idx]]
    site_seed <- trial_seed_master + idx * 1000L
    set.seed(site_seed)
    boot_info <- bootstrap_build_spec(dt, facility_col = bootstrap_detect_facility_column(dt))
    saveRDS(boot_info$data, file = file.path(trial_dir, sprintf("%s.rds", stem)), compress = TRUE)
    write_json(
      boot_info$spec,
      path = file.path(trial_dir, sprintf("%s_spec.json", stem)),
      auto_unbox = TRUE,
      pretty = TRUE,
      digits = 10
    )
    trial_meta[[stem]] <- list(
      site_seed = site_seed,
      n_rows = nrow(boot_info$data),
      n_subjects = uniqueN(boot_info$data$id)
    )
  }

  write_json(
    trial_meta,
    path = file.path(trial_dir, "meta.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = 10
  )
}

for (trial_id in seq_len(b)) materialize_one(trial_id)

manifest <- data.table(
  trial = seq_len(b),
  trial_dir = file.path(out_root, sprintf("trial_%04d", seq_len(b))),
  seed_base = seed_base,
  trial_seed_master = seed_base + seq_len(b) * 10000L
)
fwrite(manifest, file.path(out_root, "manifest.csv"))

cat(toJSON(list(
  scenario = scenario,
  root = ROOT,
  out_root = out_root,
  trials = b,
  seed_base = seed_base,
  manifest = file.path(out_root, "manifest.csv")
), auto_unbox = TRUE, pretty = TRUE, digits = 10))
