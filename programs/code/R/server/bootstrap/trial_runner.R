# trial_runner.R
# Trial-level bootstrap execution helpers.

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

fedpoppk_source("R/common/utils.R")
fedpoppk_source("R/common/constants.R")

.bootstrap_build_payloads <- function(cfg, z, return_grad, richardson_eps, param_names, seed_base, trial_id, trial_stamp) {
  payloads <- lapply(cfg$clients, function(data_path) {
    list(
      modelInfo = cfg$modelInfo,
      dataPath = data_path,
      p = as.list(setNames(as.numeric(z), param_names)),
      return_grad = isTRUE(return_grad),
      richardson_eps = as.numeric(richardson_eps),
      resample = list(
        mode = "bootstrap",
        seed_base = seed_base,
        trial_id = trial_id,
        stamp = trial_stamp
      )
    )
  })
  names(payloads) <- names(cfg$clients)
  payloads
}

bootstrap_run_single_trial <- function(trial_id,
                                       attempt = 1L,
                                       cfg,
                                       init_par_opt,
                                       lower_opt,
                                       upper_opt,
                                       optim_ctrl,
                                       run_id,
                                       out_root,
                                       seed_base,
                                       comm_timeout,
                                       comm_max_tries,
                                       comm_pause,
                                       param_names,
                                       schema_meta = list()) {
  ok_run_dir <- dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(ok_run_dir) && !dir.exists(out_root)) {
    stop(sprintf("failed to create bootstrap run directory: %s", out_root))
  }

  trial_stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  build_boot_payloads <- function(z, return_grad, richardson_eps, param_names_local) {
    .bootstrap_build_payloads(
      cfg = cfg,
      z = z,
      return_grad = return_grad,
      richardson_eps = richardson_eps,
      param_names = param_names_local,
      seed_base = seed_base,
      trial_id = trial_id,
      trial_stamp = trial_stamp
    )
  }

  objective_spec <- make_server_objective(
    client_map = cfg$clients,
    param_spec = cfg$param_spec,
    return_grad = isTRUE(cfg$objective$return_grad %||% TRUE),
    use_cor = isTRUE(cfg$objective$use_cor %||% FALSE),
    richardson_eps = as.numeric(cfg$objective$richardson_eps %||% fedpoppk_const("objective.richardson_eps", 1e-5)),
    eps_pos = as.numeric(cfg$objective$eps_pos %||% fedpoppk_const("objective.eps_pos", 1e-6)),
    lower = lower_vec,
    poll_timeout = comm_timeout,
    poll_max_tries = comm_max_tries,
    poll_pause = comm_pause,
    log_dir = out_root,
    build_named_payloads = build_boot_payloads,
    poll_fn = poll_clients_bootstrap
  )

  last_eval_failures <- list()
  tag <- sprintf("trial_%03d_%s", as.integer(trial_id), trial_stamp)

  result <- tryCatch({
    res_t <- server_optimize_lbfgsb(
      objective = list(
        fn = objective_spec$fn,
        gr = objective_spec$gr,
        log_path = objective_spec$log_path
      ),
      init_par = init_par_opt,
      lower = lower_opt,
      upper = upper_opt,
      control = optim_ctrl,
      run_id = run_id,
      save_dir = out_root
    )

    last_eval_failures <<- objective_spec$get_failures() %||% list()
    history <- objective_spec$get_history()
    par_opt <- runtime_require_named_numeric(res_t$par, param_names, "optimized_par_internal")
    par_nat <- param_int_to_report(par_opt, cfg$param_spec)
    res_t$par <- par_nat
    res_t$par_internal <- as.list(par_opt)
    res_t$par_nat <- par_nat
    res_t$parNat <- as.list(par_nat)
    res_t$schema <- schema_meta

    result_path <- file.path(out_root, sprintf("optim_result_%s.json", tag))
    trace_path <- if (length(history)) file.path(out_root, sprintf("optim_trace_%s.csv", tag)) else NULL
    final_path <- file.path(out_root, sprintf("final_params_%s.csv", tag))

    jsonlite::write_json(res_t, result_path, auto_unbox = TRUE, pretty = TRUE, digits = 10)
    if (!is.null(trace_path)) {
      trace_df <- do.call(rbind, lapply(history, function(x) {
        data.frame(
          iter = x$iter,
          objf = x$objf,
          t(as.data.frame(as.list(x$par))),
          row.names = NULL,
          check.names = FALSE
        )
      }))
      write.csv(trace_df, trace_path, row.names = FALSE)
    }
    write.csv(data.frame(parameter = names(par_nat), value = as.numeric(par_nat)), final_path, row.names = FALSE)

    list(
      status = "ok",
      tag = tag,
      value = res_t$value,
      counts = res_t$counts %||% c(NA_integer_, NA_integer_),
      control = res_t$control %||% list(),
      convergence = res_t$convergence,
      message = res_t$message %||% "",
      nat_par = par_nat,
      failed_clients = last_eval_failures,
      attempt = attempt,
      files = list(result = result_path, trace = trace_path, final = final_path),
      log_n = length(history)
    )
  }, error = function(e) {
    fail_attr <- attr(e, "failed")
    if (!is.null(fail_attr)) {
      last_eval_failures <<- fail_attr
    } else {
      last_eval_failures <<- objective_spec$get_failures() %||% list()
    }
    failure_dir <- file.path(out_root, "failures")
    ok_failure_dir <- dir.create(failure_dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(ok_failure_dir) && !dir.exists(failure_dir)) {
      warning(sprintf("failed to create failures directory: %s", failure_dir))
      failure_dir <- out_root
    }
    history <- objective_spec$get_history()
    record <- list(
      schema = schema_meta,
      status = "failed",
      tag = tag,
      error = conditionMessage(e),
      failed_clients = last_eval_failures,
      attempt = attempt,
      history = history
    )
    failure_path <- file.path(failure_dir, sprintf("failure_%s.json", tag))
    write_ok <- tryCatch({
      writeLines(jsonlite::toJSON(record, auto_unbox = TRUE, pretty = TRUE, digits = 10), failure_path)
      TRUE
    }, error = function(write_err) {
      warning(sprintf("failed to write failure record: %s (%s)", failure_path, conditionMessage(write_err)))
      FALSE
    })
    record$history <- NULL
    record$file <- if (isTRUE(write_ok)) failure_path else NA_character_
    record
  })

  result
}

bootstrap_execute_trials <- function(trials,
                                     cfg,
                                     init_par_opt,
                                     lower_opt,
                                     upper_opt,
                                     optim_ctrl,
                                     run_id,
                                     out_root,
                                     seed_base,
                                     comm_timeout,
                                     comm_max_tries,
                                     comm_pause,
                                     param_names,
                                     schema_meta = list()) {
  trial_records <- vector("list", length(trials))
  par_storage <- vector("list", length(trials))
  gc_every <- {
    raw <- suppressWarnings(as.integer(Sys.getenv("SERVER_GC_EVERY", "1")))
    if (!length(raw) || is.na(raw) || raw < 1L) 1L else raw
  }

  run_trial <- function(idx, trial_id, attempt = 1L) {
    rec <- bootstrap_run_single_trial(
      trial_id = trial_id,
      attempt = attempt,
      cfg = cfg,
      init_par_opt = init_par_opt,
      lower_opt = lower_opt,
      upper_opt = upper_opt,
      optim_ctrl = optim_ctrl,
      run_id = run_id,
      out_root = out_root,
      seed_base = seed_base,
      comm_timeout = comm_timeout,
      comm_max_tries = comm_max_tries,
      comm_pause = comm_pause,
      param_names = param_names,
      schema_meta = schema_meta
    )
    trial_records[[idx]] <<- c(trial_records[[idx]], list(rec))
    if (identical(rec$status, "ok")) {
      par_storage[[idx]] <<- rec$nat_par
    }
    if ((idx %% gc_every) == 0L) {
      invisible(gc(verbose = FALSE, full = TRUE))
    }
    rec
  }

  initial_results <- vector("list", length(trials))
  for (i in seq_along(trials)) {
    initial_results[[i]] <- run_trial(i, trials[[i]], attempt = 1L)
  }

  failed_idx <- which(vapply(initial_results, function(rec) rec$status != "ok", logical(1)))
  if (length(failed_idx) > 0) {
    message(sprintf("[bootstrap] retrying %d failed trials", length(failed_idx)))
    for (idx in failed_idx) {
      run_trial(idx, trials[[idx]], attempt = 2L)
    }
  }

  list(
    trial_records = trial_records,
    par_storage = par_storage
  )
}
