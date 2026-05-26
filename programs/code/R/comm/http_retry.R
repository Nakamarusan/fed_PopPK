# http_retry.R
# Shared HTTP POST + retry helper for server/client communication.

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

.bytes_utf8 <- function(x) {
  if (is.null(x)) return(0L)
  txt <- paste(as.character(x), collapse = "")
  as.numeric(nchar(enc2utf8(txt), type = "bytes"))
}

.infer_endpoint <- function(url) {
  m <- regmatches(url, regexpr("/[^/?#]+(?=\\?|#|$)", url, perl = TRUE))
  if (!length(m) || !nzchar(m[[1L]])) return("unknown")
  sub("^/", "", m[[1L]])
}

.as_num_or_na <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (!length(v) || !is.finite(v)) NA_real_ else as.numeric(v[[1L]])
}

.extract_httr_times <- function(res) {
  out <- list(
    namelookup_sec = NA_real_,
    connect_sec = NA_real_,
    pretransfer_sec = NA_real_,
    starttransfer_sec = NA_real_,
    total_sec = NA_real_,
    ttfb_sec = NA_real_,
    response_transfer_sec = NA_real_
  )
  if (is.null(res) || inherits(res, "error")) return(out)

  tvec <- suppressWarnings(res$times)
  if (is.null(tvec) || !length(tvec)) return(out)

  nm <- names(tvec)
  getv <- function(k) {
    if (is.null(nm) || !(k %in% nm)) return(NA_real_)
    .as_num_or_na(tvec[[k]])
  }

  out$namelookup_sec <- getv("namelookup")
  out$connect_sec <- getv("connect")
  out$pretransfer_sec <- getv("pretransfer")
  out$starttransfer_sec <- getv("starttransfer")
  out$total_sec <- getv("total")

  if (is.finite(out$starttransfer_sec) && is.finite(out$pretransfer_sec)) {
    out$ttfb_sec <- pmax(out$starttransfer_sec - out$pretransfer_sec, 0)
  }
  if (is.finite(out$total_sec) && is.finite(out$starttransfer_sec)) {
    out$response_transfer_sec <- pmax(out$total_sec - out$starttransfer_sec, 0)
  }
  out
}

.new_metrics_df <- function() {
  data.frame(
    endpoint = character(),
    url = character(),
    attempt = integer(),
    ok = logical(),
    status = integer(),
    request_bytes = numeric(),
    response_bytes = numeric(),
    elapsed_sec = numeric(),
    request_start_ts_utc = character(),
    request_end_ts_utc = character(),
    request_start_epoch_sec = numeric(),
    request_end_epoch_sec = numeric(),
    curl_namelookup_sec = numeric(),
    curl_connect_sec = numeric(),
    curl_pretransfer_sec = numeric(),
    curl_starttransfer_sec = numeric(),
    curl_total_sec = numeric(),
    curl_ttfb_sec = numeric(),
    curl_response_transfer_sec = numeric(),
    stringsAsFactors = FALSE
  )
}

http_post_json_retry <- function(url,
                                 payload,
                                 timeout = fedpoppk_const("comm.run_timeout", 300),
                                 max_tries = fedpoppk_const("comm.run_max_tries", 3L),
                                 pause = fedpoppk_const("comm.run_pause", 1),
                                 connecttimeout = 60,
                                 low_speed_limit = NULL,
                                 low_speed_time = NULL,
                                 user_agent = NULL,
                                 endpoint = NULL) {
  last_error <- "HTTP error"
  last_status <- NA_integer_
  last_text <- ""
  metric_rows <- list()
  endpoint_name <- as.character(endpoint %||% .infer_endpoint(url))
  payload_json <- tryCatch(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = NA),
    error = function(e) ""
  )
  request_bytes <- .bytes_utf8(payload_json)

  for (attempt in seq_len(max_tries)) {
    cfg <- list(httr::timeout(timeout), httr::config(connecttimeout = connecttimeout))
    if (!is.null(low_speed_limit) && !is.null(low_speed_time)) {
      cfg <- c(cfg, list(httr::config(low_speed_limit = low_speed_limit, low_speed_time = low_speed_time)))
    }
    if (!is.null(user_agent) && nzchar(user_agent)) {
      cfg <- c(cfg, list(httr::user_agent(user_agent)))
    }

    t0 <- Sys.time()
    res <- tryCatch(
      do.call(httr::POST, c(list(url = url, body = payload, encode = "json"), cfg)),
      error = function(e) e
    )
    t1 <- Sys.time()

    elapsed_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    start_ts <- format(t0, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    end_ts <- format(t1, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    start_epoch <- as.numeric(t0)
    end_epoch <- as.numeric(t1)
    tt <- .extract_httr_times(if (inherits(res, "error")) NULL else res)

    if (!inherits(res, "error") && httr::status_code(res) < 300) {
      txt <- tryCatch(httr::content(res, as = "text", encoding = "UTF-8"), error = function(e) "")
      metric_rows[[length(metric_rows) + 1L]] <- data.frame(
        endpoint = endpoint_name,
        url = as.character(url),
        attempt = as.integer(attempt),
        ok = TRUE,
        status = as.integer(httr::status_code(res)),
        request_bytes = as.numeric(request_bytes),
        response_bytes = as.numeric(.bytes_utf8(txt)),
        elapsed_sec = as.numeric(elapsed_sec),
        request_start_ts_utc = as.character(start_ts),
        request_end_ts_utc = as.character(end_ts),
        request_start_epoch_sec = as.numeric(start_epoch),
        request_end_epoch_sec = as.numeric(end_epoch),
        curl_namelookup_sec = as.numeric(tt$namelookup_sec),
        curl_connect_sec = as.numeric(tt$connect_sec),
        curl_pretransfer_sec = as.numeric(tt$pretransfer_sec),
        curl_starttransfer_sec = as.numeric(tt$starttransfer_sec),
        curl_total_sec = as.numeric(tt$total_sec),
        curl_ttfb_sec = as.numeric(tt$ttfb_sec),
        curl_response_transfer_sec = as.numeric(tt$response_transfer_sec),
        stringsAsFactors = FALSE
      )
      return(list(
        ok = TRUE,
        response = res,
        status = httr::status_code(res),
        text = txt,
        metrics = do.call(rbind, metric_rows)
      ))
    }

    if (inherits(res, "error")) {
      last_error <- conditionMessage(res)
      last_status <- NA_integer_
      last_text <- ""
      resp_bytes <- 0
    } else {
      last_status <- httr::status_code(res)
      last_text <- tryCatch(httr::content(res, as = "text", encoding = "UTF-8"), error = function(e) "")
      last_error <- sprintf("HTTP %s %s", last_status, last_text)
      resp_bytes <- .bytes_utf8(last_text)
    }

    metric_rows[[length(metric_rows) + 1L]] <- data.frame(
      endpoint = endpoint_name,
      url = as.character(url),
      attempt = as.integer(attempt),
      ok = FALSE,
      status = as.integer(last_status),
      request_bytes = as.numeric(request_bytes),
      response_bytes = as.numeric(resp_bytes),
      elapsed_sec = as.numeric(elapsed_sec),
      request_start_ts_utc = as.character(start_ts),
      request_end_ts_utc = as.character(end_ts),
      request_start_epoch_sec = as.numeric(start_epoch),
      request_end_epoch_sec = as.numeric(end_epoch),
      curl_namelookup_sec = as.numeric(tt$namelookup_sec),
      curl_connect_sec = as.numeric(tt$connect_sec),
      curl_pretransfer_sec = as.numeric(tt$pretransfer_sec),
      curl_starttransfer_sec = as.numeric(tt$starttransfer_sec),
      curl_total_sec = as.numeric(tt$total_sec),
      curl_ttfb_sec = as.numeric(tt$ttfb_sec),
      curl_response_transfer_sec = as.numeric(tt$response_transfer_sec),
      stringsAsFactors = FALSE
    )

    if (attempt < max_tries) Sys.sleep(pause + runif(1, 0, 0.5))
  }

  list(
    ok = FALSE,
    error = last_error,
    status = last_status,
    text = last_text,
    metrics = if (length(metric_rows)) do.call(rbind, metric_rows) else .new_metrics_df()
  )
}
