# tests/testthat/test-server_comm.R
library(testthat)
library(httptest2)    # exposes `with_mock_dir()` and `:::` for request_hash
library(jsonlite)
library(withr)
library(logger)
# Load the module under test
source("//root/test/server/R/server_comm.R")
test_that("poll_clients runs send_to_client in parallel and collects results", {
  # 1) モック版 send_to_client をグローバルに上書き
  assign("send_to_client",
         function(url, payload, timeout, max_tries, pause) {
           # 単純にURL末尾で objf を決め、grad はその数値を返す
           idx <- as.integer(sub(".*/run([0-9]*)?$", "\\1", url))
           if (is.na(idx) || idx == 0) idx <- 1
           list(objf = idx * 10, grad = rep(idx, idx))
         },
         envir = .GlobalEnv)

  # 2) ダミーの URL と payload
  urls    <- c("http://c1/run1", "http://c2/run2", "http://c3/run3")
  payload <- list(foo = "bar")

  # 3) future.apply のプランを固定
  plan("sequential")

  # 4) poll_clients を実行
  res <- poll_clients(
    urls, payload,
    workers     = 2,
    future.seed = FALSE,
    timeout     = 1,
    max_tries   = 1,
    pause       = 0
  )

  # 5) 結果検証
  expect_named(res, urls)
  for (i in seq_along(urls)) {
    url <- urls[i]
    expect_equal(res[[url]]$objf, i * 10)
    expect_equal(res[[url]]$grad, rep(i, i))
  }
})

# Helper: fake httr response
make_fake_resp <- function(status = 200, content_obj = list(), text_body = NULL) {
  body_text <- text_body %||% toJSON(content_obj, auto_unbox = TRUE)
  structure(
    list(
      status_code = status,
      headers     = list(`Content-Type`="application/json"),
      content     = charToRaw(body_text),
      .opts       = list(encoding = "UTF-8")
    ),
    class = "response"
  )
}

test_that("send_to_client succeeds on good JSON and 200", {
  url     <- "http://dummy/run"
  payload <- list(x = 1)
  fake    <- list(objf = 42, grad = c(1,2,3))

  # Mock RETRY() just within this test
  local_mocked_bindings(
    RETRY = function(verb, url, ..., encode, timeout, times, pause_base, pause_cap) {
      make_fake_resp(200, fake)
    },
    .package = "httr"
  )

  out <- send_to_client(url, payload, timeout = 1, max_tries = 1, pause = 0.1)
  expect_equal(out$objf, fake$objf)
  expect_equal(out$grad, fake$grad)
})

test_that("send_to_client errors on non-200 status", {
  url     <- "http://dummy/run"
  payload <- list(x = 1)

  local_mocked_bindings(
    RETRY = function(...) make_fake_resp(500, NULL, "Server error"),
    .package = "httr"
  )

  expect_error(
    send_to_client(url, payload, timeout = 1, max_tries = 1, pause = 0.1),
    "Bad status 500"
  )
})

test_that("send_to_client errors on invalid JSON", {
  url     <- "http://dummy/run"
  payload <- list(x = 1)
  bad_json <- "{not valid JSON}"

  local_mocked_bindings(
    RETRY = function(...) make_fake_resp(200, NULL, bad_json),
    .package = "httr"
  )

  expect_error(
    send_to_client(url, payload, timeout = 1, max_tries = 1, pause = 0.1),
    "Failed to parse JSON"
  )
})

test_that("send_to_client errors when objf or grad missing", {
  url     <- "http://dummy/run"
  payload <- list(x = 1)
  resp    <- list(wrong = 123)

  local_mocked_bindings(
    RETRY = function(...) make_fake_resp(200, resp),
    .package = "httr"
  )

  expect_error(
    send_to_client(url, payload, timeout = 1, max_tries = 1, pause = 0.1),
    "missing 'objf' or 'grad'"
  )
})
