# R/server_comm.R

# Dependencies:
# install.packages(c("httr","jsonlite","logger","future.apply","future"))
library(httr)
library(jsonlite)
library(logger)
library(future.apply)
library(future)

#* Send a single request to a client `/compute` endpoint, with retry and timeout
#*
#* @param url       Character. Base client URL, e.g. "http://client1:8080"
#* @param payload   Named list with fields:
#*                   - modelInfo: list(...)
#*                   - dataPath : string
#*                   - params   : named numeric vector
#* @param timeout   Numeric. Seconds to wait per HTTP request.
#* @param max_tries Integer. How many times to retry on failure.
#* @param pause     Numeric. Base seconds between retries (exponential back-off).
#* @return A list with elements `objf` (numeric) and `grad` (numeric vector).
#* @throws Error if all retries fail or if response is malformed.
send_to_client <- function(url,
                           payload,
                           timeout   = 60,
                           max_tries = 3,
                           pause     = 1) {
  # 1) JSON body
  body_json <- toJSON(payload, auto_unbox = TRUE)

  # 2) call `/compute` path
  endpoint <- paste0(url, "/compute")

  # 3) retry logic via httr::RETRY()
  resp <- tryCatch({
    RETRY("POST", endpoint,
          body    = body_json,
          encode  = "json",
          timeout(timeout),
          times      = max_tries,
          pause_base = pause,
          pause_cap  = pause * max_tries)
  }, error = function(e) {
    log_error("HTTP error contacting {endpoint}: {e$message}", endpoint = endpoint, e = e)
    stop(e)
  })

  # 4) status check
  sc <- status_code(resp)
  if (sc != 200) {
    txt <- content(resp, "text", encoding = "UTF-8")
    stop(sprintf("Bad status %d from %s: %s", sc, endpoint, substr(txt, 1, 200)))
  }

  # 5) parse JSON
  parsed <- tryCatch({
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = TRUE)
  }, error = function(e) {
    stop("Failed to parse JSON from ", endpoint, ": ", e$message)
  })

  # 6) validate shape
  if (!all(c("obj", "grad") %in% names(parsed))) {
    stop("Response from ", endpoint, " is missing 'obj' or 'grad'")
  }

  # 7) rename to objf for consistency
  list(objf = parsed$obj, grad = parsed$grad)
}


#* Poll multiple clients in parallel and collect their responses
#*
#* @param urls        Character vector of base client URLs.
#* @param payload     Named list as for `send_to_client()`.
#* @param workers     Integer or NULL. Number of parallel workers.
#* @param future.seed Logical. Seed each job for reproducibility.
#* @param ...         Additional args passed to `send_to_client()`.
#* @return Named list of responses (each is a list with `objf` and `grad`).
poll_clients <- function(urls,
                         payload,
                         workers     = NULL,
                         future.seed = TRUE,
                         ...) {
  plan(multisession, workers = workers %||% parallel::detectCores())

  results <- future_lapply(
    urls,
    function(u) send_to_client(u, payload, ...),
    future.seed = future.seed
  )

  names(results) <- urls
  results
}
