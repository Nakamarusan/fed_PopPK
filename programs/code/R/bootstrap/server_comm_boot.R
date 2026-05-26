# R/bootstrap/server_comm_boot.R
# Bootstrap-specific /run communication helper.
# Uses the same named_payloads contract as poll_clients(), but tolerates partial failures.
suppressPackageStartupMessages({ library(httr) })
source("/project/R/comm/http_retry.R", chdir = TRUE)
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

.as_named_numeric <- function(x){
  v <- unlist(x, use.names = TRUE)
  storage.mode(v) <- "double"
  v
}

.parse_client_response <- function(res_obj){
  scalarize <- function(x) {
    if (is.list(x) && length(x) == 1L) return(x[[1L]])
    x
  }
  ok_raw <- res_obj$ok
  ok <- if (is.null(ok_raw)) TRUE else isTRUE(scalarize(ok_raw))
  if (!ok) {
    message(sprintf("[poll_clients_bootstrap] raw response str: %s", paste(capture.output(str(res_obj)), collapse = " ")))
    err_txt <- res_obj$error %||% "unknown"
    message(sprintf("[poll_clients_bootstrap] client returned ok=FALSE error=%s", err_txt))
    return(list(ok=FALSE, error = err_txt))
  }
  objf_val <- scalarize(res_obj$objf)
  if (is.null(objf_val) || !is.numeric(objf_val))
    return(list(ok=FALSE, error="objf missing or non-numeric"))
  grad_out <- NULL
  if (!is.null(res_obj$grad)) {
    grad_out <- .as_named_numeric(lapply(res_obj$grad, scalarize))
  }
  compute_sec <- suppressWarnings(as.numeric(scalarize(res_obj$compute_sec %||% NA_real_)))
  if (!length(compute_sec) || !is.finite(compute_sec[[1L]])) compute_sec <- NA_real_
  list(ok=TRUE, objf=as.numeric(objf_val), grad=grad_out, compute_sec = as.numeric(compute_sec))
}

.post_run_once <- function(base_url,
                           payload,
                           timeout = fedpoppk_const("comm_boot.run_timeout", 1200),
                           tries = fedpoppk_const("comm_boot.run_max_tries", 3L),
                           pause = fedpoppk_const("comm_boot.run_pause", 1)) {
  req <- http_post_json_retry(
    url = paste0(base_url, "/run"),
    payload = payload,
    timeout = timeout,
    max_tries = tries,
    pause = pause,
    connecttimeout = 120,
    low_speed_limit = 1,
    low_speed_time = timeout,
    endpoint = "run"
  )
  if (!isTRUE(req$ok)) {
    return(list(ok = FALSE, error = req$error %||% "HTTP error", .comm = req$metrics %||% .new_metrics_df(0L)))
  }
  raw_txt <- req$text %||% ""
  parsed <- tryCatch(jsonlite::fromJSON(raw_txt, simplifyVector = FALSE), error = function(e) {
    message(sprintf("[poll_clients_bootstrap] failed to parse JSON from %s: %s | body=%s", base_url, conditionMessage(e), raw_txt))
    NULL
  })
  if (is.null(parsed)) return(list(ok = FALSE, error = "invalid JSON", .comm = req$metrics %||% .new_metrics_df(0L)))
  ans <- .parse_client_response(parsed)
  metric_df <- req$metrics %||% .new_metrics_df(0L)
  if (nrow(metric_df) > 0L) {
    metric_df$client_compute_sec <- NA_real_
    metric_df$net_overhead_sec <- NA_real_
    ok_idx <- which(metric_df$ok %in% TRUE)
    if (length(ok_idx) && is.finite(ans$compute_sec %||% NA_real_)) {
      jj <- ok_idx[[length(ok_idx)]]
      metric_df$client_compute_sec[[jj]] <- as.numeric(ans$compute_sec)
      metric_df$net_overhead_sec[[jj]] <- pmax(as.numeric(metric_df$elapsed_sec[[jj]]) - as.numeric(ans$compute_sec), 0)
    }
  }
  ans$.comm <- metric_df
  ans
}

poll_clients_bootstrap <- function(named_payloads,
                                   timeout   = fedpoppk_const("comm_boot.run_timeout", 1200),
                                   max_tries = fedpoppk_const("comm_boot.run_max_tries", 3L),
                                   pause     = fedpoppk_const("comm_boot.run_pause", 1),
                                   param_names = NULL) {
  if (is.null(names(named_payloads))) stop("named_payloads must be a named list")
  round_id <- if (exists("comm_metrics_next_round", mode = "function")) comm_metrics_next_round("run_bootstrap") else NA_integer_

  mapper <- if (exists(".map_clients", mode = "function")) .map_clients else function(x, fn) {
    out <- lapply(seq_along(x), function(i) fn(x[[i]], names(x)[[i]]))
    names(out) <- names(x)
    out
  }

  results <- mapper(named_payloads, function(payload, base_url) {
    if (getOption("fedpoppk.debug.bootstrap", FALSE)) {
      payload_json <- tryCatch(jsonlite::toJSON(payload, auto_unbox = TRUE), error = function(e) paste("<json error>", conditionMessage(e)))
      message(sprintf("[poll_clients_bootstrap] payload -> %s", payload_json))
    }
    one <- .post_run_once(
      base_url,
      payload,
      timeout = as.integer(timeout),
      tries = as.integer(max_tries),
      pause = as.numeric(pause)
    )
    if (!isTRUE(one$ok)) {
      dbg <- paste(capture.output(str(one)), collapse = " ")
      err_msg <- one$error %||% "unknown"
      message(sprintf("[poll_clients_bootstrap] %s error: %s | detail: %s", base_url, err_msg, dbg))
      return(list(
        .failed = TRUE,
        client = base_url,
        error = err_msg,
        .comm = one$.comm %||% .new_metrics_df(0L)
      ))
    }
    grad <- one$grad
    if (!is.null(grad) && !is.null(param_names) && length(param_names)) {
      if (is.null(names(grad)) || any(!nzchar(names(grad)))) {
        names(grad) <- param_names[seq_along(grad)]
      }
      grad <- grad[param_names]
    }
    list(
      .failed = FALSE,
      objf = as.numeric(one$objf),
      grad = grad,
      compute_sec = as.numeric(one$compute_sec %||% NA_real_),
      .comm = one$.comm %||% .new_metrics_df(0L)
    )
  })

  if (exists("comm_metrics_add", mode = "function")) {
    metrics_by_client <- lapply(results, function(r) r$.comm %||% .new_metrics_df(0L))
    names(metrics_by_client) <- names(results)
    comm_metrics_add(metrics_by_client, context = list(phase = "run_bootstrap", round_id = round_id))
  }

  success <- list()
  failure <- list()
  for (i in seq_along(results)) {
    r <- results[[i]]
    if (isTRUE(r$.failed)) {
      failure[[length(failure) + 1L]] <- list(client = r$client, error = r$error)
    } else {
      success[[length(success) + 1L]] <- list(objf = r$objf, grad = r$grad, compute_sec = r$compute_sec %||% NA_real_)
    }
  }
  attr(success, "failed") <- failure
  success
}
