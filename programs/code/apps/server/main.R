# server_main.R
# Main federated optimization entry point.
# Orchestrates config loading, client initialization, optimization, and outputs.

suppressPackageStartupMessages({
  library(jsonlite)
  library(fs)
  library(glue)
  library(withr)
})

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})
fedpoppk_source("R/config/server_config.R")     # load config
fedpoppk_source("R/server/runtime_common.R")    # shared runtime helpers
fedpoppk_source("R/comm/server_comm.R")         # client communication helpers (send_init, poll_clients)
fedpoppk_source("R/objective/server_objective.R")  # objective builder
fedpoppk_source("R/optimize/server_optimize.R") # optimization wrapper
fedpoppk_source("R/common/param_transform.R")

.save_results <- function(run_dir, res, par_names, schema_meta = list(), run_meta = list()) {
  dir_create(run_dir, recurse = TRUE)
  nat_hat <- unlist(res$par, use.names = TRUE)
  storage.mode(nat_hat) <- "double"
  if (is.null(names(nat_hat)) || any(!nzchar(names(nat_hat)))) {
    nat_hat <- setNames(as.numeric(nat_hat), par_names[seq_along(nat_hat)])
  }
  par_names_out <- names(nat_hat)
  out <- list(
    schema    = schema_meta,
    par       = unname(nat_hat),
    parNames  = unname(par_names_out),
    value     = unname(res$value),
    convergence = unname(res$convergence),
    message   = unname(res$message %||% ""),
    control   = res$control,
    natPar    = as.list(nat_hat),
    log       = res$log,
    meta      = c(res$meta %||% list(), run_meta)
  )
  writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE, digits = 10),
             file.path(run_dir, "result.json"))
  invisible(out)
}


.build_sequence_timing <- function(opt_elapsed_sec, eval_timing_path) {
  out <- list(
    n_eval_calls = 0L,
    n_eval_records = 0L,
    n_fn_calls = 0L,
    n_gr_calls = 0L,
    n_optimizer_iterations = 0L,
    n_rounds = 0L,
    step3_poll_total_sec = NA_real_,
    step3_round_wall_total_sec = NA_real_,
    step3_poll_overhead_total_sec = NA_real_,
    step3_broadcast_total_sec = NA_real_,
    step3_collect_barrier_total_sec = NA_real_,
    step3_exchange_compute_total_sec = NA_real_,
    step3_compute_critical_total_sec = NA_real_,
    step3_sync_wait_total_sec = NA_real_,
    step4_aggregate_total_sec = NA_real_,
    step5_optimizer_internal_sec = NA_real_,
    step3_5_total_sec = as.numeric(opt_elapsed_sec)
  )
  if (!nzchar(eval_timing_path %||% "") || !file.exists(eval_timing_path)) {
    return(out)
  }
  dt <- tryCatch(read.csv(eval_timing_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(dt) || !nrow(dt)) return(out)

  .sum_col <- function(name) {
    if (!(name %in% names(dt))) return(NA_real_)
    x <- suppressWarnings(as.numeric(dt[[name]]))
    if (!length(x) || all(!is.finite(x))) return(NA_real_)
    as.numeric(sum(x[is.finite(x)], na.rm = TRUE))
  }

  poll_total <- .sum_col("poll_elapsed_sec")
  round_wall_total <- .sum_col("round_wall_sec")
  if (!is.finite(round_wall_total) && is.finite(poll_total)) round_wall_total <- poll_total

  poll_overhead_total <- .sum_col("poll_overhead_sec")
  if (!is.finite(poll_overhead_total) && is.finite(poll_total) && is.finite(round_wall_total)) {
    poll_overhead_total <- pmax(as.numeric(poll_total) - as.numeric(round_wall_total), 0)
  }

  broadcast_total <- .sum_col("round_broadcast_window_sec")
  collect_total <- .sum_col("round_collect_barrier_sec")
  exchange_compute_total <- .sum_col("round_exchange_compute_sec")
  compute_critical_total <- .sum_col("round_compute_wall_sec")
  sync_wait_total <- .sum_col("round_sync_wait_sec")
  agg_total <- .sum_col("server_aggregate_sec")

  opt_internal_total <- .sum_col("optimizer_internal_eval_sec")
  if (!is.finite(opt_internal_total) && is.finite(opt_elapsed_sec) && is.finite(poll_total) && is.finite(agg_total)) {
    opt_internal_total <- pmax(as.numeric(opt_elapsed_sec) - as.numeric(poll_total) - as.numeric(agg_total), 0)
  }

  eval_type <- tolower(trimws(as.character(dt$eval_type %||% "")))
  fn_calls <- sum(eval_type == "fn", na.rm = TRUE)
  gr_calls <- sum(eval_type == "gr", na.rm = TRUE)

  iter_vals <- suppressWarnings(as.integer(dt$iter))
  iter_vals <- iter_vals[is.finite(iter_vals)]
  n_iter <- if (length(iter_vals)) as.integer(max(iter_vals, na.rm = TRUE)) else 0L
  if (!is.finite(n_iter) || n_iter < 0L) n_iter <- 0L
  if (n_iter == 0L && (fn_calls > 0L || gr_calls > 0L)) {
    n_iter <- as.integer(max(fn_calls, gr_calls))
  }

  round_ids <- suppressWarnings(as.integer(dt$round_id))
  round_ids <- unique(round_ids[is.finite(round_ids)])

  out$n_eval_calls <- as.integer(nrow(dt))
  out$n_eval_records <- as.integer(nrow(dt))
  out$n_fn_calls <- as.integer(fn_calls)
  out$n_gr_calls <- as.integer(gr_calls)
  out$n_optimizer_iterations <- as.integer(n_iter)
  out$n_rounds <- as.integer(length(round_ids))

  out$step3_poll_total_sec <- poll_total
  out$step3_round_wall_total_sec <- round_wall_total
  out$step3_poll_overhead_total_sec <- poll_overhead_total
  out$step3_broadcast_total_sec <- broadcast_total
  out$step3_collect_barrier_total_sec <- collect_total
  out$step3_exchange_compute_total_sec <- exchange_compute_total
  out$step3_compute_critical_total_sec <- compute_critical_total
  out$step3_sync_wait_total_sec <- sync_wait_total
  out$step4_aggregate_total_sec <- agg_total
  out$step5_optimizer_internal_sec <- opt_internal_total
  out
}

.write_timing_steps <- function(run_dir, steps_df, sequence_summary) {
  csv_path <- file.path(run_dir, "timing_steps.csv")
  json_path <- file.path(run_dir, "timing_sequence.json")
  write.csv(steps_df, csv_path, row.names = FALSE)
  writeLines(
    jsonlite::toJSON(sequence_summary, auto_unbox = TRUE, pretty = TRUE, digits = 10),
    json_path
  )
  list(csv = csv_path, json = json_path)
}

server_main <- function(
  json_path = "/project/configs/standard.json",
  seed = fedpoppk_const("bootstrap.seed_base", 4869L)
) {
  started_at <- Sys.time()
  with_seed(seed, {
    cfg <- runtime_prepare_cfg(server_config(json_path))
    schema_meta <- runtime_schema_meta(
      config_path = json_path,
      seed = seed,
      config_hash = cfg$config_hash_resolved %||% NA_character_
    )

    client_map <- cfg$clients
    model_info <- cfg$modelInfo
    param_spec <- cfg$param_spec
    init_par   <- cfg$init_par
    lower      <- cfg$lower
    upper      <- cfg$upper
    objective  <- cfg$objective
    comm       <- cfg$comm
    opt_space  <- param_optimizer_space(init_par, param_spec, lower = lower, upper = upper)

    par_names <- param_spec$names
    run <- runtime_make_run(
      logging = cfg$logging,
      default_base = "/project/result/logs",
      default_scenario = "scenario1"
    )
    run_id <- run$run_id
    run_dir <- run$run_dir
    comm_metrics_reset()

    message(sprintf("INFO [%s] initPar (natural): %s",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    paste(sprintf("%s=%s", names(init_par),
                                  format(init_par, digits = 6, trim = TRUE)),
                          collapse = ", ")))

    t_init_start <- Sys.time()
    init_res <- runtime_send_init(cfg, grad_cores = runtime_detect_grad_cores(objective))
    t_init_end <- Sys.time()

    obj <- make_server_objective(
      client_map     = client_map,
      param_spec     = param_spec,
      return_grad    = isTRUE(objective$return_grad),
      use_cor        = isTRUE(objective$use_cor),
      richardson_eps = objective$richardson_eps %||% fedpoppk_const("objective.richardson_eps", 1e-5),
      eps_pos        = objective$eps_pos %||% fedpoppk_const("objective.eps_pos", 1e-6),
      poll_timeout   = comm$run_timeout,
      poll_max_tries = comm$run_max_tries,
      poll_pause     = comm$run_pause,
      log_dir        = run_dir,
      lower          = lower
    )

    message(sprintf("[server_optimize] method=%s (client numerical gradient: %s)",
                    "L-BFGS-B", isTRUE(objective$return_grad)))

    ctrl <- runtime_strip_nonoptim_fields(cfg$optim_control)

    t_opt_start <- Sys.time()
    res <- server_optimize_lbfgsb(
      objective = obj,     # objective closure from make_server_objective()
      init_par  = opt_space$init_z,
      lower     = opt_space$lower_z,
      upper     = opt_space$upper_z,
      control   = ctrl,
      run_id    = run_id,  # reuse the same run_id
      save_dir  = run_dir  # output directory
    )
    t_opt_end <- Sys.time()
    par_z <- runtime_require_named_numeric(res$par, par_names, "optimized_par_internal")
    par_nat <- param_int_to_report(par_z, param_spec)
    res$par <- par_nat

    res$meta <- list(
      run_id   = run_id,
      eps_pos  = objective$eps_pos %||% fedpoppk_const("objective.eps_pos", 1e-6),
      use_cor  = isTRUE(objective$use_cor),
      clients  = as.list(client_map),
      model    = model_info,
      lower    = as.list(lower),
      upper    = as.list(upper),
      richardson_eps = obj$richardson_eps %||% (objective$richardson_eps %||% fedpoppk_const("objective.richardson_eps", 1e-5)),
      grad_cores = objective$grad_cores %||% 1L,
      seed = seed,
      par_internal = as.list(par_z)
    )
    t_finalize_start <- Sys.time()
    comm_out <- comm_metrics_write(run_dir)
    res$meta$communication <- comm_out$summary
    res$meta$communication_files <- list(
      csv = comm_out$csv,
      json = comm_out$json
    )
    setup_elapsed_sec <- as.numeric(difftime(t_init_start, started_at, units = "secs"))
    init_elapsed_sec <- as.numeric(difftime(t_init_end, t_init_start, units = "secs"))
    opt_elapsed_sec <- as.numeric(difftime(t_opt_end, t_opt_start, units = "secs"))

    seq_opt <- .build_sequence_timing(
      opt_elapsed_sec = opt_elapsed_sec,
      eval_timing_path = obj$eval_timing_path %||% ""
    )

    t_before_save <- Sys.time()
    finalize_elapsed_sec_pre <- as.numeric(difftime(t_before_save, t_finalize_start, units = "secs"))
    init_timing <- attr(init_res, "timing") %||% list()
    init_broadcast <- as.numeric(init_timing$round_broadcast_window_sec %||% NA_real_)
    init_collect <- as.numeric(init_timing$round_collect_barrier_sec %||% NA_real_)
    init_exchange <- if (is.finite(init_elapsed_sec)) {
      ib <- ifelse(is.finite(init_broadcast), init_broadcast, 0)
      ic <- ifelse(is.finite(init_collect), init_collect, 0)
      pmax(as.numeric(init_elapsed_sec) - ib - ic, 0)
    } else {
      NA_real_
    }

    sequence_summary <- list(
      step1_setup_sec = as.numeric(setup_elapsed_sec),
      step2_init_sec = as.numeric(init_elapsed_sec),
      step2_broadcast_window_sec = as.numeric(init_broadcast),
      step2_collect_barrier_sec = as.numeric(init_collect),
      step2_exchange_sec = as.numeric(init_exchange),
      step3_poll_total_sec = as.numeric(seq_opt$step3_poll_total_sec %||% NA_real_),
      step3_round_wall_total_sec = as.numeric(seq_opt$step3_round_wall_total_sec %||% NA_real_),
      step3_poll_overhead_total_sec = as.numeric(seq_opt$step3_poll_overhead_total_sec %||% NA_real_),
      step3_broadcast_total_sec = as.numeric(seq_opt$step3_broadcast_total_sec %||% NA_real_),
      step3_collect_barrier_total_sec = as.numeric(seq_opt$step3_collect_barrier_total_sec %||% NA_real_),
      step3_exchange_compute_total_sec = as.numeric(seq_opt$step3_exchange_compute_total_sec %||% NA_real_),
      step3_compute_critical_total_sec = as.numeric(seq_opt$step3_compute_critical_total_sec %||% NA_real_),
      step3_sync_wait_total_sec = as.numeric(seq_opt$step3_sync_wait_total_sec %||% NA_real_),
      step4_aggregate_total_sec = as.numeric(seq_opt$step4_aggregate_total_sec %||% NA_real_),
      step5_optimizer_internal_sec = as.numeric(seq_opt$step5_optimizer_internal_sec %||% NA_real_),
      step3_5_total_sec = as.numeric(opt_elapsed_sec),
      step6_finalize_sec = as.numeric(finalize_elapsed_sec_pre),
      total_runtime_sec = as.numeric(difftime(t_before_save, started_at, units = "secs")),
      n_eval_calls = as.integer(seq_opt$n_eval_calls %||% 0L),
      n_eval_records = as.integer(seq_opt$n_eval_records %||% 0L),
      n_fn_calls = as.integer(seq_opt$n_fn_calls %||% 0L),
      n_gr_calls = as.integer(seq_opt$n_gr_calls %||% 0L),
      n_optimizer_iterations = as.integer(seq_opt$n_optimizer_iterations %||% 0L),
      n_rounds = as.integer(seq_opt$n_rounds %||% 0L)
    )

    steps_df <- data.frame(
      step_id = c("1", "2", "3-5", "6", "1-6"),
      step_label = c("setup_before_init", "init_barrier", "optimization_loop", "finalize_outputs", "total_runtime"),
      elapsed_sec = c(
        as.numeric(setup_elapsed_sec),
        as.numeric(init_elapsed_sec),
        as.numeric(opt_elapsed_sec),
        as.numeric(finalize_elapsed_sec_pre),
        as.numeric(difftime(t_before_save, started_at, units = "secs"))
      ),
      stringsAsFactors = FALSE
    )
    timing_files <- .write_timing_steps(run_dir, steps_df, sequence_summary)

    res$meta$init_round <- list(
      round_id = suppressWarnings(as.integer(attr(init_res, "round_id") %||% NA_integer_)),
      timing = attr(init_res, "timing") %||% list()
    )
    res$meta$timing_sequence <- sequence_summary
    res$meta$timing_files <- c(
      list(
        eval_csv = obj$eval_timing_path %||% NA_character_,
        eval_site_csv = obj$eval_site_timing_path %||% NA_character_
      ),
      timing_files
    )

    out <- .save_results(
      run_dir,
      res,
      par_names,
      schema_meta = schema_meta,
      run_meta = list(
        duration_sec = as.numeric(difftime(t_before_save, started_at, units = "secs")),
        host = Sys.info()[["nodename"]] %||% NA_character_,
        image_tag = Sys.getenv("FEDPOPPK_IMAGE_TAG", unset = NA_character_),
        communication = comm_out$summary,
        communication_files = list(
          csv = comm_out$csv,
          json = comm_out$json
        ),
        timing_sequence = sequence_summary,
        timing_files = c(
          list(
            eval_csv = obj$eval_timing_path %||% NA_character_,
            eval_site_csv = obj$eval_site_timing_path %||% NA_character_
          ),
          timing_files
        )
      )
    )

    t_after_save <- Sys.time()
    save_elapsed_sec <- as.numeric(difftime(t_after_save, t_before_save, units = "secs"))
    sequence_summary$step6_finalize_sec <- as.numeric(sequence_summary$step6_finalize_sec %||% 0) + as.numeric(save_elapsed_sec)
    sequence_summary$total_runtime_sec <- as.numeric(difftime(t_after_save, started_at, units = "secs"))

    steps_df$elapsed_sec[steps_df$step_id == "6"] <- sequence_summary$step6_finalize_sec
    steps_df$elapsed_sec[steps_df$step_id == "1-6"] <- sequence_summary$total_runtime_sec
    timing_files <- .write_timing_steps(run_dir, steps_df, sequence_summary)

    out$meta$duration_sec <- as.numeric(difftime(t_after_save, started_at, units = "secs"))
    out$meta$timing_sequence <- sequence_summary
    out$meta$timing_files <- c(
      list(
        eval_csv = obj$eval_timing_path %||% NA_character_,
        eval_site_csv = obj$eval_site_timing_path %||% NA_character_
      ),
      timing_files
    )
    writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, digits = 10),
               file.path(run_dir, "result.json"))

    message(glue("Results saved under: {run_dir}"))
    invisible(out)
  })
}

if (identical(environment(), globalenv())) {
  cli <- runtime_parse_cli(
    args = commandArgs(trailingOnly = TRUE),
    default_config = "/project/configs/standard.json",
    default_seed = fedpoppk_const("bootstrap.seed_base", 4869L),
    allow_seed = TRUE
  )
  server_main(
    json_path = cli$config %||% "/project/configs/standard.json",
    seed      = cli$seed %||% fedpoppk_const("bootstrap.seed_base", 4869L)
  )
}
