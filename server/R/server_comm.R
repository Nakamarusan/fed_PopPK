# R/server_comm.R

# Dependencies:
# install.packages(c("httr","jsonlite","logger","future.apply","future"))
library(httr)
library(jsonlite)
library(logger)
library(future.apply)
library(future)
#' Send a single request to a client endpoint, with retry and timeout
#'
#' @param url       Character. The client URL, e.g. "http://client1:8000/run"
#' @param payload   Named list. Will be JSON-encoded.
#' @param timeout   Numeric. Seconds to wait per HTTP request.
#' @param max_tries Integer. How many times to retry on failure (default 3).
#' @param pause     Numeric. Base seconds between retries (exponential back-off).
#' @return A list with elements `objf` (numeric) and `grad` (numeric vector).
#' @throws Error if all retries fail or if response is malformed.
send_to_client <- function(url,
                           payload,
                           timeout   = 60,
                           max_tries = 3,
                           pause     = 1) {
  # Prepare JSON body
  body_json <- toJSON(payload, auto_unbox = TRUE)

  # Wrap in RETRY()
  resp <- tryCatch(
    {
      RETRY(
        "POST",
        url,
        body   = body_json,
        encode = "json",
        timeout(timeout),
        times      = max_tries,
        pause_base = pause,
        pause_cap  = pause * max_tries
      )
    },
    error = function(e) {
      log_error("HTTP error contacting {url}: {e$message}", url = url, e = e)
      stop(e)
    }
  )

  # Check status
  if (status_code(resp) != 200) {
    txt <- content(resp, "text", encoding = "UTF-8")
    stop(sprintf("Bad status %s from %s: %s",
                 status_code(resp), url, substr(txt, 1, 200)))
  }

  # Parse JSON
  parsed <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8")),
    error = function(e) {
      stop("Failed to parse JSON from ", url, ": ", e$message)
    }
  )

  # Validate shape
  if (!all(c("objf", "grad") %in% names(parsed))) {
    stop("Response from ", url, " is missing 'objf' or 'grad'")
  }

  parsed
}

#' Poll multiple clients in parallel and collect their responses
#'
#' @param urls        Character vector of client endpoints.
#' @param payload     Named list to send to each client.
#' @param workers     Integer or NULL. Number of parallel workers (default: all cores).
#' @param future.seed Logical. Seed each job for reproducibility.
#' @param ...         Additional args passed to `send_to_client()`.
#' @return Named list of responses (each is a list with `objf` and `grad`).
poll_clients <- function(urls,
                         payload,
                         workers     = NULL,
                         future.seed = TRUE,
                         ...) {
  # Set up future plan (multisession by default)
  plan(multisession, workers = workers %||% parallel::detectCores())

  # Fire off parallel requests
  results <- future_lapply(
    urls,
    function(u) send_to_client(u, payload, ...),
    future.seed = future.seed
  )

  # Name the list by URL
  names(results) <- urls
  results
}
