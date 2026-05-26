# bootstrap_sampling.R
# Subject-level bootstrap sampling helpers used by client bootstrap mode.

suppressPackageStartupMessages({
  library(data.table)
})

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})
fedpoppk_source("R/common/utils.R")

bootstrap_detect_facility_column <- function(dt) {
  candidates <- c("Facility", "facility", "FACILITY")
  hit <- candidates[candidates %in% names(dt)]
  if (length(hit)) hit[[1L]] else NULL
}

bootstrap_build_spec <- function(dt, facility_col = NULL) {
  dt <- as.data.table(dt)
  if (!"id" %in% names(dt)) {
    stop("data must contain column 'id' for subject-level bootstrap")
  }

  facility_col <- facility_col %||% bootstrap_detect_facility_column(dt)

  original_nrow <- nrow(dt)
  new_id_counter <- 0L
  groups_out <- list()
  pieces <- list()

  if (!is.null(facility_col)) {
    facilities <- dt[, unique(get(facility_col))]
    for (fac in facilities) {
      dt_fac <- dt[get(facility_col) == fac]
      subj_ids <- unique(dt_fac$id)
      resampled_ids <- sample(subj_ids, length(subj_ids), replace = TRUE)
      subj_rows <- vector("list", length(resampled_ids))
      for (j in seq_along(resampled_ids)) {
        dt_sub <- copy(dt_fac[id == resampled_ids[j]])
        new_id_counter <- new_id_counter + 1L
        dt_sub[, id := new_id_counter]
        subj_rows[[j]] <- dt_sub
      }
      pieces[[length(pieces) + 1L]] <- rbindlist(subj_rows, use.names = TRUE, fill = TRUE)
      groups_out[[length(groups_out) + 1L]] <- list(
        facility = fac,
        resampled_ids = resampled_ids
      )
    }
  } else {
    subj_ids <- unique(dt$id)
    resampled_ids <- sample(subj_ids, length(subj_ids), replace = TRUE)
    pieces <- lapply(seq_along(resampled_ids), function(j) {
      dt_sub <- copy(dt[id == resampled_ids[j]])
      dt_sub[, id := j]
      dt_sub
    })
    groups_out[[1L]] <- list(facility = NULL, resampled_ids = resampled_ids)
  }

  boot_dt <- rbindlist(pieces, use.names = TRUE, fill = TRUE)

  boot_nrow <- nrow(boot_dt)

  if (!is.null(facility_col)) {
    orig_counts_dt <- dt[, .(n_subjects = uniqueN(id)), by = facility_col]
    boot_counts_dt <- boot_dt[, .(n_subjects = uniqueN(id)), by = facility_col]
    setnames(orig_counts_dt, facility_col, "facility")
    setnames(boot_counts_dt, facility_col, "facility")
    setorder(orig_counts_dt, facility)
    setorder(boot_counts_dt, facility)
    if (!identical(orig_counts_dt$n_subjects, boot_counts_dt$n_subjects)) {
      stop("bootstrap subject counts per facility do not match original")
    }
    subject_summary <- orig_counts_dt
  } else {
    subject_summary <- data.table(n_subjects = uniqueN(dt$id))
    if (uniqueN(boot_dt$id) != subject_summary$n_subjects) {
      stop("bootstrap subject count mismatch")
    }
  }

  spec <- list(
    facility_col = facility_col,
    groups = groups_out,
    original_nrow = original_nrow,
    boot_nrow = boot_nrow,
    original_subjects = subject_summary
  )

  list(data = boot_dt, spec = spec)
}

bootstrap_reconstruct <- function(base_dt, spec) {
  dt <- as.data.table(base_dt)
  facility_col <- spec$facility_col
  new_id_counter <- 0L
  pieces <- list()

  if (!is.null(facility_col)) {
    for (grp in spec$groups) {
      fac <- grp$facility
      dt_fac <- dt[get(facility_col) == fac]
      subj_rows <- vector("list", length(grp$resampled_ids))
      for (j in seq_along(grp$resampled_ids)) {
        dt_sub <- copy(dt_fac[id == grp$resampled_ids[j]])
        new_id_counter <- new_id_counter + 1L
        dt_sub[, id := new_id_counter]
        subj_rows[[j]] <- dt_sub
      }
      pieces[[length(pieces) + 1L]] <- rbindlist(subj_rows, use.names = TRUE, fill = TRUE)
    }
  } else {
    resampled_ids <- spec$groups[[1L]]$resampled_ids
    pieces <- lapply(seq_along(resampled_ids), function(j) {
      dt_sub <- copy(dt[id == resampled_ids[j]])
      dt_sub[, id := j]
      dt_sub
    })
  }

  boot_dt <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
  expected_nrow <- spec$boot_nrow
  if (is.null(expected_nrow) || !length(expected_nrow) || !is.finite(as.numeric(expected_nrow[[1L]]))) {
    expected_nrow <- spec$original_nrow
  }
  expected_nrow <- as.integer(expected_nrow[[1L]])
  if (nrow(boot_dt) != expected_nrow) {
    stop(sprintf("reconstructed bootstrap sample row count mismatch (expected %d, got %d)", expected_nrow, nrow(boot_dt)))
  }
  if (!is.null(facility_col)) {
    boot_counts_dt <- boot_dt[, .(n_subjects = uniqueN(id)), by = facility_col]
    setnames(boot_counts_dt, facility_col, "facility")
    orig_counts_dt <- as.data.table(spec$original_subjects)
    setorder(boot_counts_dt, facility)
    setorder(orig_counts_dt, facility)
    if (!identical(orig_counts_dt$n_subjects, boot_counts_dt$n_subjects)) {
      stop("reconstructed bootstrap subject counts per facility mismatch")
    }
  } else {
    orig_count <- as.integer(spec$original_subjects$n_subjects[[1L]])
    if (uniqueN(boot_dt$id) != orig_count) {
      stop("reconstructed bootstrap subject count mismatch")
    }
  }
  boot_dt[]
}
