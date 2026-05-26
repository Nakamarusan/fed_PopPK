# summary_writer.R
# Summary and output writers for bootstrap runs.

local({
  candidates <- c("/project/R/common/utils.R", "R/common/utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

bootstrap_write_summary <- function(out_root,
                                    trials,
                                    trial_records,
                                    par_storage,
                                    run_info,
                                    schema_meta = list(),
                                    run_meta = list()) {
  .execution_state <- function(x) {
    if (is.null(x) || !nzchar(x)) return("unknown")
    if (identical(x, "ok")) return("completed")
    as.character(x)
  }

  final_records <- lapply(seq_along(trials), function(i) {
    attempts <- trial_records[[i]]
    if (!length(attempts)) {
      return(list(
        status = "missing",
        trial = as.integer(trials[[i]]),
        attempts = 0L
      ))
    }
    rec <- tail(attempts, 1)[[1]]
    rec$trial <- as.integer(trials[[i]])
    rec$attempts <- as.integer(length(attempts))
    rec
  })

  # Build a compact status table to make downstream QC/reporting deterministic.
  trial_status_df <- do.call(rbind, lapply(final_records, function(rec) {
    counts <- rec$counts %||% c(NA_integer_, NA_integer_)
    if (is.null(names(counts))) names(counts) <- c("fn", "gr")[seq_along(counts)]
    execution_state <- .execution_state(rec$status %||% "unknown")
    conv_code <- as.integer(rec$convergence %||% NA_integer_)
    converged <- isTRUE(execution_state == "completed") && is.finite(conv_code) && (conv_code == 0L)
    data.frame(
      trial = as.integer(rec$trial %||% NA_integer_),
      execution_state = as.character(execution_state),
      converged = as.logical(converged),
      attempts = as.integer(rec$attempts %||% 0L),
      convergence = conv_code,
      value = as.numeric(rec$value %||% NA_real_),
      eval_fn = as.integer(counts[[1]] %||% NA_integer_),
      eval_gr = as.integer(counts[[2]] %||% NA_integer_),
      log_n = as.integer(rec$log_n %||% NA_integer_),
      failed_clients_n = as.integer(length(rec$failed_clients %||% list())),
      message = as.character(rec$message %||% ""),
      result_file = as.character(rec$files$result %||% NA_character_),
      trace_file = as.character(rec$files$trace %||% NA_character_),
      final_file = as.character(rec$files$final %||% NA_character_),
      stringsAsFactors = FALSE
    )
  }))

  completed_idx <- which(trial_status_df$execution_state == "completed")
  par_cols <- sort(unique(unlist(lapply(final_records[completed_idx], function(rec) names(rec$nat_par %||% numeric())))))
  par_matrix <- matrix(
    NA_real_,
    nrow = length(final_records),
    ncol = length(par_cols),
    dimnames = list(NULL, par_cols)
  )
  for (i in seq_along(final_records)) {
    vec <- final_records[[i]]$nat_par %||% numeric()
    if (length(vec)) par_matrix[i, names(vec)] <- as.numeric(vec)
  }
  par_df <- as.data.frame(par_matrix, stringsAsFactors = FALSE)

  estimates_wide_df <- data.frame(
    trial = as.integer(trials),
    execution_state = as.character(trial_status_df$execution_state),
    converged = as.logical(trial_status_df$converged),
    par_df,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!length(par_cols) || !length(completed_idx)) {
    summary_df_out <- data.frame(parameter = character(), median = numeric(), p2.5 = numeric(), p97.5 = numeric())
  } else {
    summary_df <- t(apply(par_df[completed_idx, , drop = FALSE], 2, function(x) {
      x <- x[is.finite(x)]
      if (!length(x)) return(c(median = NA_real_, p2.5 = NA_real_, p97.5 = NA_real_))
      qs <- quantile(x, c(0.025, 0.975), names = FALSE)
      c(median = median(x), p2.5 = qs[[1]], p97.5 = qs[[2]])
    }))
    summary_df_out <- data.frame(parameter = rownames(summary_df), summary_df, row.names = NULL)
  }

  summary_path_csv <- file.path(out_root, "bootstrap_summary.csv")
  summary_path_json <- file.path(out_root, "bootstrap_summary.json")
  result_path_json <- file.path(out_root, "result.json")
  status_path_csv <- file.path(out_root, "trial_status.csv")
  failures_path_csv <- file.path(out_root, "failed_trials.csv")
  estimates_wide_path_csv <- file.path(out_root, "trial_estimates_wide.csv")
  estimates_long_path_csv <- file.path(out_root, "trial_estimates_long.csv")

  write.csv(summary_df_out, summary_path_csv, row.names = FALSE)
  write.csv(trial_status_df, status_path_csv, row.names = FALSE)
  write.csv(estimates_wide_df, estimates_wide_path_csv, row.names = FALSE)

  failed_trials_df <- trial_status_df[
    trial_status_df$execution_state != "completed",
    c("trial", "execution_state", "attempts", "message", "failed_clients_n"),
    drop = FALSE
  ]
  write.csv(failed_trials_df, failures_path_csv, row.names = FALSE)

  if (nrow(estimates_wide_df) > 0L && length(par_cols) > 0L) {
    estimates_long_df <- stats::reshape(
      estimates_wide_df,
      varying = par_cols,
      v.names = "estimate",
      timevar = "parameter",
      times = par_cols,
      idvar = c("trial", "execution_state", "converged"),
      direction = "long"
    )
    rownames(estimates_long_df) <- NULL
    estimates_long_df <- estimates_long_df[, c("trial", "execution_state", "converged", "parameter", "estimate"), drop = FALSE]
  } else {
    estimates_long_df <- data.frame(
      trial = integer(),
      execution_state = character(),
      converged = logical(),
      parameter = character(),
      estimate = numeric(),
      stringsAsFactors = FALSE
    )
  }
  write.csv(estimates_long_df, estimates_long_path_csv, row.names = FALSE)

  runs_output <- lapply(seq_along(trials), function(i) {
    history_fmt <- lapply(trial_records[[i]], function(rec) {
      rec2 <- rec
      rec2$log <- NULL
      if (!is.null(rec2$nat_par)) rec2$nat_par <- as.list(rec2$nat_par)
      rec2
    })
    if (!length(history_fmt)) {
      final <- list(
        execution_state = "missing",
        trial = as.integer(trials[[i]]),
        attempts = 0L
      )
    } else {
      final <- history_fmt[[length(history_fmt)]]
      final$execution_state <- .execution_state(final$status %||% "unknown")
      final$status <- NULL
      final$trial <- as.integer(trials[[i]])
      final$attempts <- length(history_fmt)
      if (length(history_fmt) > 1) final$history <- history_fmt
    }
    final
  })

  summary_list <- split(summary_df_out[, c("median", "p2.5", "p97.5")], summary_df_out$parameter)
  summary_json <- lapply(names(summary_list), function(nm) {
    vals <- summary_list[[nm]]
    list(
      parameter = nm,
      median = as.numeric(vals[["median"]]),
      p2.5 = as.numeric(vals[["p2.5"]]),
      p97.5 = as.numeric(vals[["p97.5"]])
    )
  })

  success_count <- sum(trial_status_df$execution_state == "completed")
  failure_count <- length(final_records) - success_count
  conv_codes <- suppressWarnings(as.integer(trial_status_df$convergence))
  completed_mask <- trial_status_df$execution_state == "completed"
  nonconverged_mask <- completed_mask & is.finite(conv_codes) & (conv_codes != 0L)
  nonconverged_missing_mask <- completed_mask & !is.finite(conv_codes)
  nonconverged_trials <- as.integer(trial_status_df$trial[nonconverged_mask])

  json_output <- list(
    schema = schema_meta,
    aggregation = list(
      ci_method = "percentile",
      ci_level = 0.95,
      quantile_type = 7,
      estimates_scale = "natural"
    ),
    summary = summary_json,
    runs = runs_output,
    trial_status = trial_status_df,
    info = list(
      run_id = run_info$run_id,
      log_base_dir = run_info$log_base,
      scenario = run_info$log_scenario,
      total_trials = length(trials),
      successes = success_count,
      failures = failure_count,
      nonconverged_count = length(nonconverged_trials),
      nonconverged_trials = as.list(nonconverged_trials),
      convergence_missing_count = sum(nonconverged_missing_mask),
      seed_base = run_info$seed_base,
      B_per_client = run_info$B_per_client,
      duration_sec = run_meta$duration_sec %||% NA_real_,
      host = run_meta$host %||% (Sys.info()[["nodename"]] %||% NA_character_),
      image_tag = run_meta$image_tag %||% Sys.getenv("FEDPOPPK_IMAGE_TAG", unset = NA_character_),
      communication = run_meta$communication %||% list(),
      communication_files = run_meta$communication_files %||% list(),
      output_files = list(
        bootstrap_summary_csv = summary_path_csv,
        bootstrap_summary_json = summary_path_json,
        trial_status_csv = status_path_csv,
        failed_trials_csv = failures_path_csv,
        trial_estimates_wide_csv = estimates_wide_path_csv,
        trial_estimates_long_csv = estimates_long_path_csv,
        result_json = result_path_json
      )
    )
  )

  writeLines(jsonlite::toJSON(json_output, auto_unbox = TRUE, digits = 10, pretty = TRUE), summary_path_json)
  writeLines(jsonlite::toJSON(json_output, auto_unbox = TRUE, digits = 10, pretty = TRUE), result_path_json)
  json_output
}
