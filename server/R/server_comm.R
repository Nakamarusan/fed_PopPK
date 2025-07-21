# server_comm.R -------------------------------------------------
suppressPackageStartupMessages({
  library(httr)
  library(purrr)
})

## ---------- 1. /init -------------------------------------------------
send_init <- function(client_map, modelInfo, initPar,
                      max_tries = 10, pause = 1, timeout = 300) {

  imap(client_map, function(dataPath, base_url) {
    payload <- list(
      modelInfo = modelInfo,
      initPar   = initPar,
      dataPath  = dataPath
    )

    attempt <- 1L
    repeat {
      res <- POST(
        url     = paste0(base_url, "/init"),
        body    = payload,
        encode  = "json",
        timeout(timeout)
      )
      if (status_code(res) < 300) {
        message(sprintf("[send_init] %s 初期化 成功", base_url))
        break
      }
      if (attempt >= max_tries) stop_for_status(res)

      message(sprintf(
        "[send_init] %s に接続失敗 (%d/%d)…%d 秒後リトライ",
        base_url, attempt, max_tries, pause
      ))
      Sys.sleep(pause); attempt <- attempt + 1L
    }
  })
}

## ---------- 2. /run --------------------------------------------------
# named_payloads : names = baseURL, value = list(p = <list>, dataPath = <chr>, …)
poll_clients <- function(named_payloads,
                         timeout   = 180,
                         max_tries = 3,
                         pause     = 1) {

  imap(named_payloads, function(payload, base_url) {
    message("POST payload size (bytes): ", object.size(payload))
    message("\n>>> POST to ", base_url,
            "  | names(p) = ", paste(names(payload$p), collapse = ","))

    attempt <- 1L
    repeat {
      res <- POST(
        url     = paste0(base_url, "/run"),
        body    = payload,
        encode  = "json",
        timeout(300),  # 全体タイムアウト
        config = config(
        connecttimeout = 60, # 接続までの最大時間
        low_speed_limit = 1, # 信速度が 1 byte/sec 未満でも 300 秒間は切断しない
        low_speed_time = 300  # 応答が遅くても 300秒まで維持
        )
      )
      message(rawToChar(res$content))
      message(sprintf(">>> HTTP status from %s: %d", base_url, status_code(res)))

      if (status_code(res) < 300) break
      if (attempt >= max_tries) {
        message(">>> Response (text):")
        message(content(res, as = "text", encoding = "UTF-8"))
        stop_for_status(res)
      }

      message(sprintf(">>> Retry %d/%d after failure", attempt, max_tries))
      Sys.sleep(pause); attempt <- attempt + 1L
    }

    parsed <- content(res, "parsed", simplifyVector = TRUE)
    message(sprintf(">>> Response objf = %s", parsed$objf))
    message(sprintf(">>> Response grad = %s", paste(parsed$grad, collapse = ", ")))
    return(parsed)
  })
}
