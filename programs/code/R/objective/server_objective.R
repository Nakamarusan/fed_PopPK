# server_objective.R
# Builds server-side objective/gradient closures from per-client responses.
# Handles parameter transforms, client fan-out calls, and iteration logging.

suppressPackageStartupMessages({
  library(jsonlite)
  library(readr)
  library(fs)
})

local({
  candidates <- c("/project/R/common/utils.R", "R/common/utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})
local({
  candidates <- c("/project/R/common/constants.R", "R/common/constants.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/constants.R not found")
  source(hit[[1L]], chdir = TRUE)
})
local({
  candidates <- c("/project/R/common/param_transform.R", "R/common/param_transform.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/param_transform.R not found")
  source(hit[[1L]], chdir = TRUE)
})

.PENALTY_OBJF <- fedpoppk_const("penalty_objf", 1e15)

.ensure_names <- function(x, nms = NULL) {
  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    if (is.null(nms) || !length(nms)) {
      stop("parameter names are required")
    }
    x <- setNames(as.numeric(x), nms[seq_along(x)])
  }
  x
}

.append_log <- function(path, iter, par_z, objf_sum) {
  dir_create(dirname(path), recurse = TRUE)
  par_df <- as.data.frame(
    as.list(setNames(as.numeric(par_z), names(par_z))),
    check.names = FALSE
  )
  row <- data.frame(
    iter = as.integer(iter),
    par_df,
    objf = as.numeric(objf_sum),
    check.names = FALSE
  )
  write.table(
    row, file = path, sep = ",", row.names = FALSE, col.names = !file_exists(path),
    append = file_exists(path)
  )
}

.append_client_log <- function(path, iter, eval_type, res_list, param_names) {
  if (is.null(res_list) || !length(res_list)) return(invisible(NULL))
  dir_create(dirname(path), recurse = TRUE)

  rows <- lapply(names(res_list), function(client_name) {
    rr <- res_list[[client_name]]
    objf_val <- suppressWarnings(as.numeric(rr$objf))
    if (!length(objf_val) || !is.finite(objf_val[[1L]])) objf_val <- NA_real_

    grad_vals <- setNames(rep(NA_real_, length(param_names)), paste0("grad_", param_names))
    if (!is.null(rr$grad)) {
      g <- .ensure_names(rr$grad, param_names)
      g <- suppressWarnings(as.numeric(g[param_names]))
      if (length(g) == length(param_names)) {
        names(g) <- paste0("grad_", param_names)
        grad_vals[names(g)] <- g
      }
    }

    data.frame(
      iter = as.integer(iter),
      eval_type = as.character(eval_type),
      client = as.character(client_name),
      objf = as.numeric(objf_val[[1L]] %||% NA_real_),
      as.data.frame(as.list(grad_vals), check.names = FALSE),
      check.names = FALSE
    )
  })

  out <- do.call(rbind, rows)
  write.table(
    out, file = path, sep = ",", row.names = FALSE, col.names = !file_exists(path),
    append = file_exists(path)
  )
  invisible(NULL)
}


.append_eval_timing <- function(path, eval_idx, iter, eval_type, round_id,
                                poll_elapsed_sec, round_timing,
                                aggregate_sec, total_eval_sec,
                                objective_value = NA_real_) {
  dir_create(dirname(path), recurse = TRUE)
  rt <- round_timing %||% list()

  round_wall <- as.numeric(rt$round_wall_sec %||% NA_real_)
  poll_elapsed <- as.numeric(poll_elapsed_sec)
  aggregate_elapsed <- as.numeric(aggregate_sec)
  total_eval_elapsed <- as.numeric(total_eval_sec)

  poll_overhead <- if (is.finite(poll_elapsed) && is.finite(round_wall)) {
    pmax(poll_elapsed - round_wall, 0)
  } else {
    NA_real_
  }
  optimizer_internal_eval <- if (is.finite(total_eval_elapsed) && is.finite(poll_elapsed) && is.finite(aggregate_elapsed)) {
    pmax(total_eval_elapsed - poll_elapsed - aggregate_elapsed, 0)
  } else {
    NA_real_
  }

  row <- data.frame(
    eval_idx = as.integer(eval_idx),
    iter = as.integer(iter),
    eval_type = as.character(eval_type),
    round_id = as.integer(round_id %||% NA_integer_),
    round_start_epoch_sec = as.numeric(rt$round_start_epoch_sec %||% NA_real_),
    round_end_epoch_sec = as.numeric(rt$round_end_epoch_sec %||% NA_real_),
    poll_elapsed_sec = poll_elapsed,
    round_wall_sec = round_wall,
    round_broadcast_window_sec = as.numeric(rt$round_broadcast_window_sec %||% NA_real_),
    round_collect_barrier_sec = as.numeric(rt$round_collect_barrier_sec %||% NA_real_),
    round_first_response_sec = as.numeric(rt$round_first_response_sec %||% NA_real_),
    round_exchange_compute_sec = as.numeric(rt$round_exchange_compute_sec %||% NA_real_),
    round_compute_wall_sec = as.numeric(rt$round_compute_wall_sec %||% NA_real_),
    round_sync_wait_sec = as.numeric(rt$round_sync_wait_sec %||% NA_real_),
    poll_overhead_sec = as.numeric(poll_overhead),
    server_aggregate_sec = aggregate_elapsed,
    optimizer_internal_eval_sec = as.numeric(optimizer_internal_eval),
    total_eval_sec = total_eval_elapsed,
    objective_value = as.numeric(objective_value),
    check.names = FALSE
  )
  write.table(
    row, file = path, sep = ",", row.names = FALSE,
    col.names = !file_exists(path), append = file_exists(path)
  )
  invisible(NULL)
}


.append_eval_site_timing <- function(path, eval_idx, iter, eval_type, round_id, round_timing) {
  rt <- round_timing %||% list()
  per <- rt$per_client %||% data.frame()
  if (!is.data.frame(per) || nrow(per) == 0L) return(invisible(NULL))
  dir_create(dirname(path), recurse = TRUE)

  out <- data.frame(
    eval_idx = as.integer(eval_idx),
    iter = as.integer(iter),
    eval_type = as.character(eval_type),
    round_id = as.integer(round_id %||% NA_integer_),
    client = as.character(per$client %||% NA_character_),
    elapsed_sec = suppressWarnings(as.numeric(per$elapsed_sec %||% NA_real_)),
    compute_sec = suppressWarnings(as.numeric(per$compute_sec %||% NA_real_)),
    net_overhead_sec = suppressWarnings(as.numeric(per$net_overhead_sec %||% NA_real_)),
    site_wait_sec = suppressWarnings(as.numeric(per$site_wait_sec %||% NA_real_)),
    site_start_lag_sec = suppressWarnings(as.numeric(per$site_start_lag_sec %||% NA_real_)),
    request_start_ts_utc = as.character(per$request_start_ts_utc %||% NA_character_),
    request_end_ts_utc = as.character(per$request_end_ts_utc %||% NA_character_),
    request_start_epoch_sec = suppressWarnings(as.numeric(per$request_start_epoch_sec %||% NA_real_)),
    request_end_epoch_sec = suppressWarnings(as.numeric(per$request_end_epoch_sec %||% NA_real_)),
    curl_namelookup_sec = suppressWarnings(as.numeric(per$curl_namelookup_sec %||% NA_real_)),
    curl_connect_sec = suppressWarnings(as.numeric(per$curl_connect_sec %||% NA_real_)),
    curl_pretransfer_sec = suppressWarnings(as.numeric(per$curl_pretransfer_sec %||% NA_real_)),
    curl_starttransfer_sec = suppressWarnings(as.numeric(per$curl_starttransfer_sec %||% NA_real_)),
    curl_total_sec = suppressWarnings(as.numeric(per$curl_total_sec %||% NA_real_)),
    curl_ttfb_sec = suppressWarnings(as.numeric(per$curl_ttfb_sec %||% NA_real_)),
    curl_response_transfer_sec = suppressWarnings(as.numeric(per$curl_response_transfer_sec %||% NA_real_)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  write.table(
    out, file = path, sep = ",", row.names = FALSE,
    col.names = !file_exists(path), append = file_exists(path)
  )
  invisible(NULL)
}


.same_named_point <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  if (!is.numeric(a) || !is.numeric(b)) return(FALSE)
  if (length(a) != length(b)) return(FALSE)
  if (!identical(names(a), names(b))) return(FALSE)
  isTRUE(all(as.numeric(a) == as.numeric(b)))
}

.make_payloads <- function(z, return_grad, r_eps, param_names) {
  z <- .ensure_names(z, param_names)
  list(
    p = as.list(setNames(as.numeric(z), names(z))),
    return_grad = isTRUE(return_grad),
    richardson_eps = as.numeric(r_eps)
  )
}

make_server_objective <- function(
  client_map,
  param_spec,
  return_grad    = TRUE,
  use_cor        = FALSE,
  richardson_eps = fedpoppk_const("objective.richardson_eps", 1e-5),
  eps_pos        = fedpoppk_const("objective.eps_pos", 1e-6),
  lower,
  poll_timeout   = fedpoppk_const("comm.run_timeout", 300),
  poll_max_tries = fedpoppk_const("comm.run_max_tries", 3L),
  poll_pause     = fedpoppk_const("comm.run_pause", 1),
  log_dir        = "logs",
  build_named_payloads = NULL,
  poll_fn        = poll_clients
) {
  e <- new.env(parent = emptyenv())
  e$iter    <- 0L
  e$eval_idx <- 0L
  e$log_csv <- file.path(log_dir, "iter_log.csv")
  e$client_log_csv <- file.path(log_dir, "iter_client_log.csv")
  e$eval_timing_csv <- file.path(log_dir, "timing_eval.csv")
  e$eval_site_timing_csv <- file.path(log_dir, "timing_eval_site.csv")
  e$return_grad <- isTRUE(return_grad)
  e$richardson_eps <- as.numeric(richardson_eps)
  e$history <- list()
  e$last_failures <- list()
  if (is.null(param_spec)) stop("param_spec is required for make_server_objective")
  param_names <- param_spec$names
  e$param_spec <- param_spec
  e$cache <- list(
    valid = FALSE,
    z = NULL,
    objf = NULL,
    grad = NULL
  )

  .cache_get <- function(z, need_grad = FALSE) {
    if (!isTRUE(e$cache$valid)) return(NULL)
    if (!.same_named_point(e$cache$z, z)) return(NULL)
    if (isTRUE(need_grad) && is.null(e$cache$grad)) return(NULL)
    e$cache
  }

  .cache_set <- function(z, objf, grad = NULL) {
    e$cache$valid <- TRUE
    e$cache$z <- z
    e$cache$objf <- as.numeric(objf)
    e$cache$grad <- grad
    invisible(NULL)
  }

  .log_eval_timing <- function(eval_type, iter, res_list,
                               poll_start, poll_end,
                               agg_start, agg_end,
                               eval_start, eval_end,
                               objective_value = NA_real_) {
    round_id <- suppressWarnings(as.integer(attr(res_list, "round_id") %||% NA_integer_))
    round_timing <- attr(res_list, "timing") %||% list()
    e$eval_idx <- e$eval_idx + 1L
    .append_eval_timing(
      path = e$eval_timing_csv,
      eval_idx = e$eval_idx,
      iter = iter,
      eval_type = eval_type,
      round_id = round_id,
      poll_elapsed_sec = as.numeric(difftime(poll_end, poll_start, units = "secs")),
      round_timing = round_timing,
      aggregate_sec = as.numeric(difftime(agg_end, agg_start, units = "secs")),
      total_eval_sec = as.numeric(difftime(eval_end, eval_start, units = "secs")),
      objective_value = objective_value
    )
    .append_eval_site_timing(
      path = e$eval_site_timing_csv,
      eval_idx = e$eval_idx,
      iter = iter,
      eval_type = eval_type,
      round_id = round_id,
      round_timing = round_timing
    )
  }

  fn <- function(z) {
    z <- .ensure_names(z, param_names)
    e$iter <- e$iter + 1L
    p_nat <- param_int_to_report(z, e$param_spec)
    cached <- .cache_get(z, need_grad = FALSE)
    if (!is.null(cached)) {
      .append_log(e$log_csv, e$iter, z, cached$objf)
      return(as.numeric(cached$objf))
    }

    t_eval_start <- Sys.time()
    named_payloads <- if (is.function(build_named_payloads)) {
      build_named_payloads(
        z = z,
        return_grad = e$return_grad,
        richardson_eps = e$richardson_eps,
        param_names = param_names
      )
    } else {
      out <- lapply(
        names(client_map),
        function(u) .make_payloads(z, e$return_grad, e$richardson_eps, param_names)
      )
      names(out) <- names(client_map)
      out
    }

    t_poll_start <- Sys.time()
    res_list <- poll_fn(
      named_payloads = named_payloads,
      timeout   = poll_timeout,
      max_tries = poll_max_tries,
      pause     = poll_pause,
      param_names = param_names
    )
    t_poll_end <- Sys.time()

    .append_client_log(e$client_log_csv, e$iter, "fn", res_list, param_names)
    e$last_failures <- attr(res_list, "failed") %||% list()

    t_agg_start <- Sys.time()
    if (!length(res_list)) {
      obj_sum <- Inf
      .cache_set(z, obj_sum, grad = NULL)
      .append_log(e$log_csv, e$iter, z, obj_sum)
      e$history[[length(e$history) + 1L]] <- list(iter = e$iter, objf = obj_sum, par = as.list(p_nat))
      t_agg_end <- Sys.time()
      .log_eval_timing(
        eval_type = "fn", iter = e$iter, res_list = res_list,
        poll_start = t_poll_start, poll_end = t_poll_end,
        agg_start = t_agg_start, agg_end = t_agg_end,
        eval_start = t_eval_start, eval_end = t_agg_end,
        objective_value = obj_sum
      )
      return(as.numeric(obj_sum))
    }

    objf_vec <- vapply(res_list, function(r) as.numeric(r$objf), numeric(1))
    if (any(!is.finite(objf_vec)) || any(objf_vec >= .PENALTY_OBJF)) {
      obj_sum <- Inf
    } else {
      obj_sum <- sum(objf_vec)
    }
    .cache_set(z, obj_sum, grad = NULL)

    .append_log(e$log_csv, e$iter, z, obj_sum)
    e$history[[length(e$history) + 1L]] <- list(iter = e$iter, objf = obj_sum, par = as.list(p_nat))

    t_agg_end <- Sys.time()
    .log_eval_timing(
      eval_type = "fn", iter = e$iter, res_list = res_list,
      poll_start = t_poll_start, poll_end = t_poll_end,
      agg_start = t_agg_start, agg_end = t_agg_end,
      eval_start = t_eval_start, eval_end = t_agg_end,
      objective_value = obj_sum
    )

    as.numeric(obj_sum)
  }

  gr <- if (!isTRUE(return_grad)) {
    NULL
  } else {
    function(z) {
      z <- .ensure_names(z, param_names)
      cached <- .cache_get(z, need_grad = TRUE)
      if (!is.null(cached)) {
        return(setNames(as.numeric(cached$grad), param_names))
      }
      p_nat <- param_int_to_report(z, e$param_spec)

      t_eval_start <- Sys.time()
      named_payloads <- if (is.function(build_named_payloads)) {
        build_named_payloads(
          z = z,
          return_grad = TRUE,
          richardson_eps = e$richardson_eps,
          param_names = param_names
        )
      } else {
        out <- lapply(
          names(client_map),
          function(u) .make_payloads(z, TRUE, e$richardson_eps, param_names)
        )
        names(out) <- names(client_map)
        out
      }

      t_poll_start <- Sys.time()
      res_list <- poll_fn(
        named_payloads = named_payloads,
        timeout   = poll_timeout,
        max_tries = poll_max_tries,
        pause     = poll_pause,
        param_names = param_names
      )
      t_poll_end <- Sys.time()

      .append_client_log(e$client_log_csv, e$iter, "gr", res_list, param_names)
      e$last_failures <- attr(res_list, "failed") %||% list()

      t_agg_start <- Sys.time()
      if (!length(res_list)) {
        zero <- setNames(numeric(length(param_names)), param_names)
        .cache_set(z, Inf, grad = zero)
        t_agg_end <- Sys.time()
        .log_eval_timing(
          eval_type = "gr", iter = e$iter, res_list = res_list,
          poll_start = t_poll_start, poll_end = t_poll_end,
          agg_start = t_agg_start, agg_end = t_agg_end,
          eval_start = t_eval_start, eval_end = t_agg_end,
          objective_value = Inf
        )
        return(zero)
      }

      obj_vals <- vapply(res_list, function(r) as.numeric(r$objf), numeric(1))
      if (any(!is.finite(obj_vals)) || any(obj_vals >= .PENALTY_OBJF)) {
        zero <- setNames(numeric(length(param_names)), param_names)
        .cache_set(z, Inf, grad = zero)
        t_agg_end <- Sys.time()
        .log_eval_timing(
          eval_type = "gr", iter = e$iter, res_list = res_list,
          poll_start = t_poll_start, poll_end = t_poll_end,
          agg_start = t_agg_start, agg_end = t_agg_end,
          eval_start = t_eval_start, eval_end = t_agg_end,
          objective_value = Inf
        )
        return(zero)
      }

      gmat <- do.call(rbind, lapply(res_list, function(r) {
        g <- r$grad
        if (is.null(g) || any(!is.finite(r$objf)) || r$objf >= .PENALTY_OBJF) {
          out <- numeric(length(param_names))
          names(out) <- param_names
          return(out)
        }
        g <- .ensure_names(g, param_names)
        vals <- as.numeric(g[param_names])
        vals[!is.finite(vals)] <- 0
        vals
      }))
      gsum <- colSums(gmat)
      grad_z <- setNames(as.numeric(gsum), param_names)
      .cache_set(z, sum(obj_vals), grad = grad_z)

      t_agg_end <- Sys.time()
      .log_eval_timing(
        eval_type = "gr", iter = e$iter, res_list = res_list,
        poll_start = t_poll_start, poll_end = t_poll_end,
        agg_start = t_agg_start, agg_end = t_agg_end,
        eval_start = t_eval_start, eval_end = t_agg_end,
        objective_value = sum(obj_vals)
      )
      grad_z
    }
  }

  list(
    fn = fn,
    gr = gr,
    log_path = e$log_csv,
    client_log_path = e$client_log_csv,
    eval_timing_path = e$eval_timing_csv,
    eval_site_timing_path = e$eval_site_timing_csv,
    richardson_eps = e$richardson_eps,
    get_history = function() e$history,
    get_failures = function() e$last_failures
  )
}
