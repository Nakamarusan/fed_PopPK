# server_comm.R -------------------------------------------------
suppressPackageStartupMessages({
  library(httr)
  library(purrr)
})

## ---------- 1. /init -------------------------------------------------
send_init <- function(client_map, modelInfo, initPar,
                      max_tries = 10, pause = 1, timeout = 5) {

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
                         timeout   = 30,
                         max_tries = 3,
                         pause     = 1) {

  imap(named_payloads, function(payload, base_url) {

    ## ちょっとだけデバッグ表示（必要ならコメントアウト）
    message("\n>>> POST to ", base_url,
            "  | names(p) = ", paste(names(payload$p), collapse = ","))

    attempt <- 1L
    repeat {
      res <- POST(
        url     = paste0(base_url, "/run"),
        body    = payload,
        encode  = "json",
        timeout(timeout)
      )
      if (status_code(res) < 300) break
      if (attempt >= max_tries) stop_for_status(res)

      Sys.sleep(pause); attempt <- attempt + 1L
    }
    content(res, "parsed", simplifyVector = TRUE)
  })
}
