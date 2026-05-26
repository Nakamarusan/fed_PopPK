# server_comm.R
# Sends /init and /run requests to clients and normalizes responses.
# Includes retry/backoff behavior to tolerate transient communication failures.
suppressPackageStartupMessages({
  library(httr)
})
source("/project/R/comm/http_retry.R", chdir = TRUE)
local({
  candidates <- c("/project/R/common/constants.R", "R/common/constants.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/constants.R not found")
  source(hit[[1L]], chdir = TRUE)
})
.PENALTY_OBJF <- fedpoppk_const("penalty_objf", 1e15)

.new_comm_df <- function(n = 0L) {
  data.frame(
    timestamp = rep(NA_character_, n),
    phase = rep(NA_character_, n),
    round_id = rep(NA_integer_, n),
    client = rep(NA_character_, n),
    endpoint = rep(NA_character_, n),
    url = rep(NA_character_, n),
    attempt = rep(NA_integer_, n),
    ok = rep(NA, n),
    status = rep(NA_integer_, n),
    request_bytes = rep(NA_real_, n),
    response_bytes = rep(NA_real_, n),
    elapsed_sec = rep(NA_real_, n),
    request_start_ts_utc = rep(NA_character_, n),
    request_end_ts_utc = rep(NA_character_, n),
    request_start_epoch_sec = rep(NA_real_, n),
    request_end_epoch_sec = rep(NA_real_, n),
    curl_namelookup_sec = rep(NA_real_, n),
    curl_connect_sec = rep(NA_real_, n),
    curl_pretransfer_sec = rep(NA_real_, n),
    curl_starttransfer_sec = rep(NA_real_, n),
    curl_total_sec = rep(NA_real_, n),
    curl_ttfb_sec = rep(NA_real_, n),
    curl_response_transfer_sec = rep(NA_real_, n),
    client_compute_sec = rep(NA_real_, n),
    net_overhead_sec = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )
}


.comm_metrics_env <- local({
  e <- new.env(parent = emptyenv())
  e$records <- .new_comm_df(0L)
  e$rounds <- list(init = 0L, run = 0L, run_bootstrap = 0L)
  e
})

comm_metrics_reset <- function() {
  .comm_metrics_env$records <- .new_comm_df(0L)
  .comm_metrics_env$rounds <- list(init = 0L, run = 0L, run_bootstrap = 0L)
  invisible(TRUE)
}

comm_metrics_next_round <- function(phase = "run") {
  ph <- as.character(phase)
  cur <- as.integer(.comm_metrics_env$rounds[[ph]] %||% 0L)
  cur <- cur + 1L
  .comm_metrics_env$rounds[[ph]] <- cur
  cur
}

comm_metrics_add <- function(metrics, context = list()) {
  if (is.null(metrics)) return(invisible(FALSE))

  items <- if (is.data.frame(metrics)) {
    list(metrics)
  } else if (is.list(metrics)) {
    metrics
  } else {
    list()
  }
  if (!length(items)) return(invisible(FALSE))

  ts_now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  ph <- as.character(context$phase %||% NA_character_)
  rid <- suppressWarnings(as.integer(context$round_id %||% NA_integer_))
  ctx_client <- as.character(context$client %||% NA_character_)

  if (is.null(names(items))) names(items) <- rep("", length(items))
  for (i in seq_along(items)) {
    nm <- names(items)[[i]]
    df <- items[[i]]
    if (!is.data.frame(df) || nrow(df) == 0L) next
    out <- .new_comm_df(nrow(df))
    common <- intersect(names(out), names(df))
    for (col in common) out[[col]] <- df[[col]]
    out$timestamp <- ifelse(is.na(out$timestamp), ts_now, out$timestamp)
    out$phase <- ifelse(is.na(out$phase), ph, out$phase)
    out$round_id <- ifelse(is.na(out$round_id), rid, out$round_id)
    resolved_client <- ctx_client
    if (is.na(resolved_client) || !nzchar(resolved_client)) {
      resolved_client <- as.character(nm %||% NA_character_)
    }
    out$client <- ifelse(is.na(out$client), resolved_client, out$client)
    .comm_metrics_env$records <- rbind(.comm_metrics_env$records, out)
  }
  invisible(TRUE)
}

comm_metrics_snapshot <- function() {
  df <- .comm_metrics_env$records

  .phase_stats <- function(mask) {
    req <- sum(mask, na.rm = TRUE)
    list(
      requests = as.integer(req),
      request_bytes = as.numeric(sum(df$request_bytes[mask], na.rm = TRUE)),
      response_bytes = as.numeric(sum(df$response_bytes[mask], na.rm = TRUE)),
      total_bytes = as.numeric(sum(df$request_bytes[mask] + df$response_bytes[mask], na.rm = TRUE)),
      retries = as.integer(sum(df$attempt[mask] > 1L, na.rm = TRUE)),
      median_elapsed_sec = if (req > 0) as.numeric(stats::median(df$elapsed_sec[mask], na.rm = TRUE)) else NA_real_
    )
  }
  .round_wall_summary <- function(mask) {
    out <- list(n_rounds = 0L, median_round_wall_sec = NA_real_, p95_round_wall_sec = NA_real_)
    if (!(nrow(df) > 0L && any(mask) && any(!is.na(df$round_id[mask])))) return(out)
    keys <- paste(df$phase[mask], df$round_id[mask], sep = "::")
    split_idx <- split(which(mask), keys)

    round_wall <- vapply(split_idx, function(ix) {
      start_vals <- suppressWarnings(as.numeric(df$request_start_epoch_sec[ix]))
      end_vals <- suppressWarnings(as.numeric(df$request_end_epoch_sec[ix]))
      if (any(is.finite(start_vals)) && any(is.finite(end_vals))) {
        return(as.numeric(max(end_vals[is.finite(end_vals)], na.rm = TRUE) -
                          min(start_vals[is.finite(start_vals)], na.rm = TRUE)))
      }
      elapsed_vals <- suppressWarnings(as.numeric(df$elapsed_sec[ix]))
      if (any(is.finite(elapsed_vals))) {
        return(as.numeric(max(elapsed_vals[is.finite(elapsed_vals)], na.rm = TRUE)))
      }
      NA_real_
    }, numeric(1))

    round_wall <- round_wall[is.finite(round_wall)]
    if (!length(round_wall)) return(out)
    list(
      n_rounds = as.integer(length(round_wall)),
      median_round_wall_sec = as.numeric(stats::median(round_wall, na.rm = TRUE)),
      p95_round_wall_sec = as.numeric(stats::quantile(round_wall, probs = 0.95, names = FALSE, na.rm = TRUE))
    )
  }

  run_mask <- !is.na(df$phase) & df$phase %in% c("run", "run_bootstrap")
  init_mask <- !is.na(df$phase) & df$phase == "init"
  round_summary <- .round_wall_summary(run_mask)
  init_round_summary <- .round_wall_summary(init_mask)

  # Decompose run round wall-time into client compute and residual (network/serialization/queueing).
  timing_split <- list(
    n_rounds = 0L,
    rounds_with_compute = 0L,
    total_round_wall_sec = NA_real_,
    total_round_compute_wall_sec = NA_real_,
    total_round_comm_overhead_wall_sec = NA_real_,
    comm_overhead_ratio = NA_real_,
    compute_ratio = NA_real_
  )
  run_ok_mask <- run_mask & (df$ok %in% TRUE)
  if (nrow(df) > 0L && any(run_ok_mask) && any(!is.na(df$round_id[run_ok_mask]))) {
    keys <- paste(df$phase[run_ok_mask], df$round_id[run_ok_mask], sep = "::")
    split_idx <- split(which(run_ok_mask), keys)
    round_wall <- vapply(split_idx, function(ix) max(df$elapsed_sec[ix], na.rm = TRUE), numeric(1))
    round_compute <- vapply(split_idx, function(ix) {
      cc <- suppressWarnings(as.numeric(df$client_compute_sec[ix]))
      if (!length(cc) || all(!is.finite(cc))) return(NA_real_)
      max(cc[is.finite(cc)], na.rm = TRUE)
    }, numeric(1))
    round_overhead <- ifelse(
      is.finite(round_wall) & is.finite(round_compute),
      pmax(round_wall - round_compute, 0),
      NA_real_
    )

    total_wall <- as.numeric(sum(round_wall[is.finite(round_wall)], na.rm = TRUE))
    total_compute <- as.numeric(sum(round_compute[is.finite(round_compute)], na.rm = TRUE))
    total_overhead <- as.numeric(sum(round_overhead[is.finite(round_overhead)], na.rm = TRUE))
    timing_split <- list(
      n_rounds = as.integer(length(round_wall)),
      rounds_with_compute = as.integer(sum(is.finite(round_compute))),
      total_round_wall_sec = total_wall,
      total_round_compute_wall_sec = total_compute,
      total_round_comm_overhead_wall_sec = total_overhead,
      comm_overhead_ratio = if (is.finite(total_wall) && total_wall > 0) as.numeric(total_overhead / total_wall) else NA_real_,
      compute_ratio = if (is.finite(total_wall) && total_wall > 0) as.numeric(total_compute / total_wall) else NA_real_
    )
  }

  summary <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    totals = list(
      requests = as.integer(nrow(df)),
      request_bytes = as.numeric(sum(df$request_bytes, na.rm = TRUE)),
      response_bytes = as.numeric(sum(df$response_bytes, na.rm = TRUE)),
      total_bytes = as.numeric(sum(df$request_bytes + df$response_bytes, na.rm = TRUE)),
      retries = as.integer(sum(df$attempt > 1L, na.rm = TRUE))
    ),
    init = .phase_stats(!is.na(df$phase) & df$phase == "init"),
    init_round_latency = init_round_summary,
    run = .phase_stats(!is.na(df$phase) & df$phase == "run"),
    run_bootstrap = .phase_stats(!is.na(df$phase) & df$phase == "run_bootstrap"),
    round_latency = round_summary,
    timing_split = timing_split
  )
  list(records = df, summary = summary)
}

comm_metrics_write <- function(run_dir,
                               csv_name = "comm_metrics.csv",
                               json_name = "comm_metrics.json") {
  snap <- comm_metrics_snapshot()
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  csv_path <- file.path(run_dir, csv_name)
  json_path <- file.path(run_dir, json_name)
  write.csv(snap$records, csv_path, row.names = FALSE)
  writeLines(
    jsonlite::toJSON(snap$summary, auto_unbox = TRUE, pretty = TRUE, digits = 10),
    json_path
  )
  list(csv = csv_path, json = json_path, summary = snap$summary)
}


.available_post_cores <- function() {
  env <- Sys.getenv("SERVER_POST_CORES", "")
  if (nzchar(env)) {
    v <- suppressWarnings(as.integer(env))
    if (length(v) && is.finite(v) && v >= 1L) return(as.integer(v))
  }

  if (requireNamespace("parallelly", quietly = TRUE)) {
    v <- suppressWarnings(as.integer(parallelly::availableCores()))
    if (length(v) && is.finite(v) && v >= 1L) return(as.integer(v))
  }

  v <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
  if (!length(v) || !is.finite(v) || v < 1L) return(1L)
  as.integer(v)
}

.extract_round_timing <- function(metrics_by_client, compute_by_client = NULL) {
  if (!length(metrics_by_client)) {
    return(list(
      round_start_epoch_sec = NA_real_,
      round_end_epoch_sec = NA_real_,
      round_wall_sec = NA_real_,
      round_broadcast_window_sec = NA_real_,
      round_collect_barrier_sec = NA_real_,
      round_first_response_sec = NA_real_,
      round_compute_wall_sec = NA_real_,
      round_sync_wait_sec = NA_real_,
      round_exchange_compute_sec = NA_real_,
      per_client = .new_comm_df(0L)
    ))
  }

  if (is.null(names(metrics_by_client))) {
    names(metrics_by_client) <- paste0("client", seq_along(metrics_by_client))
  }

  rows <- lapply(seq_along(metrics_by_client), function(i) {
    client_name <- names(metrics_by_client)[[i]]
    df <- metrics_by_client[[i]]

    elapsed <- NA_real_
    net_overhead <- NA_real_
    start_epoch <- NA_real_
    end_epoch <- NA_real_
    start_ts <- NA_character_
    end_ts <- NA_character_

    curl_namelookup <- NA_real_
    curl_connect <- NA_real_
    curl_pretransfer <- NA_real_
    curl_starttransfer <- NA_real_
    curl_total <- NA_real_
    curl_ttfb <- NA_real_
    curl_resp_transfer <- NA_real_

    if (is.data.frame(df) && nrow(df) > 0L) {
      ok_idx <- which(df$ok %in% TRUE)
      if (length(ok_idx)) {
        jj <- ok_idx[[length(ok_idx)]]
        elapsed <- suppressWarnings(as.numeric(df$elapsed_sec[[jj]] %||% NA_real_))
        net_overhead <- suppressWarnings(as.numeric(df$net_overhead_sec[[jj]] %||% NA_real_))
        start_epoch <- suppressWarnings(as.numeric(df$request_start_epoch_sec[[jj]] %||% NA_real_))
        end_epoch <- suppressWarnings(as.numeric(df$request_end_epoch_sec[[jj]] %||% NA_real_))
        start_ts <- as.character(df$request_start_ts_utc[[jj]] %||% NA_character_)
        end_ts <- as.character(df$request_end_ts_utc[[jj]] %||% NA_character_)

        curl_namelookup <- suppressWarnings(as.numeric(df$curl_namelookup_sec[[jj]] %||% NA_real_))
        curl_connect <- suppressWarnings(as.numeric(df$curl_connect_sec[[jj]] %||% NA_real_))
        curl_pretransfer <- suppressWarnings(as.numeric(df$curl_pretransfer_sec[[jj]] %||% NA_real_))
        curl_starttransfer <- suppressWarnings(as.numeric(df$curl_starttransfer_sec[[jj]] %||% NA_real_))
        curl_total <- suppressWarnings(as.numeric(df$curl_total_sec[[jj]] %||% NA_real_))
        curl_ttfb <- suppressWarnings(as.numeric(df$curl_ttfb_sec[[jj]] %||% NA_real_))
        curl_resp_transfer <- suppressWarnings(as.numeric(df$curl_response_transfer_sec[[jj]] %||% NA_real_))
      }
    }

    compute_sec <- NA_real_
    if (!is.null(compute_by_client) && length(compute_by_client)) {
      if (!is.null(names(compute_by_client)) && client_name %in% names(compute_by_client)) {
        compute_sec <- suppressWarnings(as.numeric(compute_by_client[[client_name]]))
      } else if (length(compute_by_client) >= i) {
        compute_sec <- suppressWarnings(as.numeric(compute_by_client[[i]]))
      }
    }

    data.frame(
      client = as.character(client_name),
      elapsed_sec = as.numeric(elapsed),
      compute_sec = as.numeric(compute_sec),
      net_overhead_sec = as.numeric(net_overhead),
      request_start_ts_utc = as.character(start_ts),
      request_end_ts_utc = as.character(end_ts),
      request_start_epoch_sec = as.numeric(start_epoch),
      request_end_epoch_sec = as.numeric(end_epoch),
      curl_namelookup_sec = as.numeric(curl_namelookup),
      curl_connect_sec = as.numeric(curl_connect),
      curl_pretransfer_sec = as.numeric(curl_pretransfer),
      curl_starttransfer_sec = as.numeric(curl_starttransfer),
      curl_total_sec = as.numeric(curl_total),
      curl_ttfb_sec = as.numeric(curl_ttfb),
      curl_response_transfer_sec = as.numeric(curl_resp_transfer),
      stringsAsFactors = FALSE
    )
  })

  per_client <- do.call(rbind, rows)
  rownames(per_client) <- NULL

  start_vals <- suppressWarnings(as.numeric(per_client$request_start_epoch_sec))
  end_vals <- suppressWarnings(as.numeric(per_client$request_end_epoch_sec))
  elapsed_vals <- suppressWarnings(as.numeric(per_client$elapsed_sec))

  has_abs <- any(is.finite(start_vals)) && any(is.finite(end_vals))
  round_start <- if (has_abs) min(start_vals[is.finite(start_vals)], na.rm = TRUE) else NA_real_
  round_end <- if (has_abs) max(end_vals[is.finite(end_vals)], na.rm = TRUE) else NA_real_

  round_wall_sec <- if (has_abs) {
    as.numeric(round_end - round_start)
  } else if (any(is.finite(elapsed_vals))) {
    as.numeric(max(elapsed_vals[is.finite(elapsed_vals)], na.rm = TRUE))
  } else {
    NA_real_
  }

  round_broadcast_window_sec <- if (sum(is.finite(start_vals)) >= 2L) {
    as.numeric(max(start_vals[is.finite(start_vals)], na.rm = TRUE) - min(start_vals[is.finite(start_vals)], na.rm = TRUE))
  } else {
    NA_real_
  }

  round_collect_barrier_sec <- if (sum(is.finite(end_vals)) >= 2L) {
    as.numeric(max(end_vals[is.finite(end_vals)], na.rm = TRUE) - min(end_vals[is.finite(end_vals)], na.rm = TRUE))
  } else {
    NA_real_
  }

  round_first_response_sec <- if (has_abs && any(is.finite(end_vals))) {
    as.numeric(min(end_vals[is.finite(end_vals)], na.rm = TRUE) - round_start)
  } else {
    NA_real_
  }

  compute_vals <- suppressWarnings(as.numeric(per_client$compute_sec))
  round_compute_wall_sec <- if (any(is.finite(compute_vals))) max(compute_vals[is.finite(compute_vals)], na.rm = TRUE) else NA_real_

  round_sync_wait_sec <- if (is.finite(round_wall_sec) && is.finite(round_compute_wall_sec)) {
    pmax(round_wall_sec - round_compute_wall_sec, 0)
  } else {
    NA_real_
  }

  round_exchange_compute_sec <- if (is.finite(round_wall_sec)) {
    bw <- ifelse(is.finite(round_broadcast_window_sec), round_broadcast_window_sec, 0)
    cb <- ifelse(is.finite(round_collect_barrier_sec), round_collect_barrier_sec, 0)
    pmax(round_wall_sec - bw - cb, 0)
  } else {
    NA_real_
  }

  per_client$site_wait_sec <- if (is.finite(round_end)) {
    ifelse(is.finite(end_vals), pmax(round_end - end_vals, 0), NA_real_)
  } else if (is.finite(round_wall_sec)) {
    ifelse(is.finite(elapsed_vals), pmax(round_wall_sec - elapsed_vals, 0), NA_real_)
  } else {
    NA_real_
  }

  per_client$site_start_lag_sec <- if (is.finite(round_start)) {
    ifelse(is.finite(start_vals), pmax(start_vals - round_start, 0), NA_real_)
  } else {
    NA_real_
  }

  list(
    round_start_epoch_sec = as.numeric(round_start),
    round_end_epoch_sec = as.numeric(round_end),
    round_wall_sec = as.numeric(round_wall_sec),
    round_broadcast_window_sec = as.numeric(round_broadcast_window_sec),
    round_collect_barrier_sec = as.numeric(round_collect_barrier_sec),
    round_first_response_sec = as.numeric(round_first_response_sec),
    round_compute_wall_sec = as.numeric(round_compute_wall_sec),
    round_sync_wait_sec = as.numeric(round_sync_wait_sec),
    round_exchange_compute_sec = as.numeric(round_exchange_compute_sec),
    per_client = per_client
  )
}

.map_clients <- function(named_values, fn) {
  stopifnot(!is.null(names(named_values)))
  n <- length(named_values)
  if (n == 0L) return(list())

  keys <- names(named_values)
  vals <- unname(named_values)
  cores <- min(n, .available_post_cores())

  if (.Platform$OS.type == "windows" || cores <= 1L) {
    out <- lapply(seq_len(n), function(i) fn(vals[[i]], keys[[i]]))
  } else {
    out <- parallel::mclapply(
      X = seq_len(n),
      FUN = function(i) fn(vals[[i]], keys[[i]]),
      mc.cores = cores,
      mc.preschedule = TRUE
    )
  }
  names(out) <- keys
  out
}

# Send /init to all clients.
# client_map maps base_url -> dataPath.
send_init <- function(client_map, modelInfo, initPar,
                      max_tries = fedpoppk_const("comm.init_max_tries", 20L),
                      pause = fedpoppk_const("comm.init_pause", 5),
                      timeout = fedpoppk_const("comm.init_timeout", 300),
                      use_cor = FALSE, eps_pos = fedpoppk_const("objective.eps_pos", 1e-6),
                      lower = NULL, bootstrap = NULL,
                      paramSpec = NULL, omegaBlocks = NULL, grad_cores = NULL) {
  round_id <- comm_metrics_next_round("init")
  responses <- .map_clients(client_map, function(dataPath, base_url) {
    ip <- unlist(initPar, use.names = TRUE)
    storage.mode(ip) <- "double"
    if (is.null(names(ip)) || any(!nzchar(names(ip)))) {
      stop("initPar must be a **named** numeric vector")
    }

    payload <- list(
      modelInfo = modelInfo,
      initPar   = as.list(ip),
      dataPath  = dataPath,
      use_cor   = isTRUE(use_cor),
      eps_pos   = as.numeric(eps_pos)
    )
    if (!is.null(grad_cores)) {
      payload$gradCores <- as.integer(grad_cores)
    }
    if (!is.null(paramSpec)) {
      payload$paramSpec <- paramSpec
    }
    if (!is.null(omegaBlocks)) {
      payload$omegaBlocks <- omegaBlocks
    }
    if (!is.null(lower)) {
      lo <- unlist(lower, use.names = TRUE)
      storage.mode(lo) <- "double"
      if (is.null(names(lo)) || any(!nzchar(names(lo)))) {
        stop("lower must be a named numeric vector")
      }
      payload$lower <- as.list(lo)
    }
    if (!is.null(bootstrap)) {
      payload$bootstrap <- bootstrap
    }
    attempt <- 1L
    metric_rows <- list()
    repeat {
      req <- http_post_json_retry(
        url = paste0(base_url, "/init"),
        payload = payload,
        timeout = timeout,
        max_tries = 1,
        pause = pause,
        connecttimeout = 60,
        user_agent = "fed-nlmixr2/1.0",
        endpoint = "init"
      )
      metric_rows[[length(metric_rows) + 1L]] <- req$metrics %||% .new_metrics_df(0L)
      if (isTRUE(req$ok)) {
        message(sprintf("[send_init] %s init succeeded", base_url))
        parsed <- content(req$response, "parsed", simplifyVector = FALSE)
        parsed$.comm <- if (length(metric_rows)) do.call(rbind, metric_rows) else .new_metrics_df(0L)
        return(parsed)
      }
      if (attempt >= max_tries) {
        stop(req$error %||% sprintf("init failed for %s", base_url))
      }
      msg <- req$error %||% "unknown error"
      message(sprintf("[send_init] %s failed (%d/%d): %s ... retrying in %ds",
                      base_url, attempt, max_tries, msg, pause))
      Sys.sleep(pause + runif(1, 0, 0.5))  # small jitter to avoid synchronized retries
      attempt <- attempt + 1L
    }
  })
  metrics_by_client <- lapply(responses, function(r) r$.comm %||% .new_metrics_df(0L))
  comm_metrics_add(metrics_by_client, context = list(phase = "init", round_id = round_id))
  init_timing <- .extract_round_timing(metrics_by_client, compute_by_client = NULL)
  responses_clean <- lapply(responses, function(r) {
    r$.comm <- NULL
    r
  })
  attr(responses_clean, "round_id") <- round_id
  attr(responses_clean, "timing") <- init_timing
  responses_clean
}

# Send /run to all clients and parse objf/grad payloads.
poll_clients <- function(named_payloads,
                         timeout   = fedpoppk_const("comm.run_timeout", 300),
                         max_tries = fedpoppk_const("comm.run_max_tries", 3L),
                         pause     = fedpoppk_const("comm.run_pause", 1),
                         param_names = NULL) {
  round_id <- comm_metrics_next_round("run")
  results <- .map_clients(named_payloads, function(payload, base_url) {
    attempt <- 1L
    metric_rows <- list()
    repeat {
      req <- http_post_json_retry(
        url = paste0(base_url, "/run"),
        payload = payload,
        timeout = timeout,
        max_tries = 1,
        pause = pause,
        connecttimeout = 60,
        low_speed_limit = 1,
        low_speed_time = 1800,
        user_agent = "fed-nlmixr2/1.0",
        endpoint = "run"
      )
      metric_rows[[length(metric_rows) + 1L]] <- req$metrics %||% .new_metrics_df(0L)
      if (isTRUE(req$ok)) break
      if (attempt >= max_tries) {
        if (nzchar(req$text %||% "")) message(req$text)
        stop(req$error %||% sprintf("run failed for %s", base_url))
      }
      msg <- req$error %||% "unknown error"
      message(sprintf("[poll] %s retry %d/%d: %s", base_url, attempt, max_tries, msg))
      Sys.sleep(pause + runif(1, 0, 0.5))  # jitter to avoid synchronized retries
      attempt <- attempt + 1L
    }

    parsed <- content(req$response, "parsed", simplifyVector = FALSE)
    compute_sec <- suppressWarnings(as.numeric(parsed$compute_sec %||% NA_real_))
    if (!length(compute_sec) || !is.finite(compute_sec[[1L]])) compute_sec <- NA_real_
    metric_df <- if (length(metric_rows)) do.call(rbind, metric_rows) else .new_metrics_df(0L)
    if (nrow(metric_df) > 0L) {
      metric_df$client_compute_sec <- NA_real_
      metric_df$net_overhead_sec <- NA_real_
      ok_idx <- which(metric_df$ok %in% TRUE)
      if (length(ok_idx)) {
        jj <- ok_idx[[length(ok_idx)]]
        if (is.finite(compute_sec)) {
          metric_df$client_compute_sec[[jj]] <- as.numeric(compute_sec)
          metric_df$net_overhead_sec[[jj]] <- pmax(as.numeric(metric_df$elapsed_sec[[jj]]) - as.numeric(compute_sec), 0)
        }
      }
    }

    objf_raw <- parsed$objf
    if (is.list(objf_raw) && length(objf_raw) == 1L) {
      objf_raw <- objf_raw[[1L]]
    }
    objf_val <- suppressWarnings(as.numeric(objf_raw))
    if (!length(objf_val)) {
      stop("Client response must contain numeric scalar 'objf'")
    }
    if (!is.finite(objf_val)) {
      objf_val <- .PENALTY_OBJF
    }

    grad_num <- NULL
    if (!is.null(parsed$grad)) {
      grad_raw <- parsed$grad
      if (!is.list(grad_raw)) {
        grad_raw <- as.list(grad_raw)
      }
      g <- unlist(grad_raw, use.names = TRUE)
      storage.mode(g) <- "double"
      known <- param_names %||% names(payload$p)
      if (is.null(known) || !length(known)) {
        stop("parameter names are required to decode client gradient response")
      }
      if (is.null(names(g)) || any(!nzchar(names(g)))) {
        names(g) <- known[seq_along(g)]
      }
      grad_num <- g
    }

    list(
      objf = as.numeric(objf_val),
      grad = grad_num,
      compute_sec = as.numeric(compute_sec),
      .comm = metric_df
    )
  })
  metrics_by_client <- lapply(results, function(r) r$.comm %||% .new_metrics_df(0L))
  comm_metrics_add(metrics_by_client, context = list(phase = "run", round_id = round_id))
  compute_by_client <- vapply(results, function(r) as.numeric(r$compute_sec %||% NA_real_), numeric(1))
  if (is.null(names(compute_by_client)) && !is.null(names(results))) names(compute_by_client) <- names(results)
  round_timing <- .extract_round_timing(metrics_by_client, compute_by_client = compute_by_client)

  results_clean <- lapply(results, function(r) {
    r$.comm <- NULL
    r
  })
  attr(results_clean, "round_id") <- round_id
  attr(results_clean, "timing") <- round_timing
  results_clean
}
