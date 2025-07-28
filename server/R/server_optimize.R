suppressPackageStartupMessages({
  library(logger)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

server_optimize <- function(init_par,
                            client_map,
                            common_info = list(),
                            opts        = list(maxit = 100),
                            comm_fn     = poll_clients,
                            agg_fn      = aggregate_responses) {
  
  # --- (引数のチェックは変更なし) ---
  stopifnot(
    is.numeric(init_par), !is.null(names(init_par)), all(nzchar(names(init_par))),
    is.character(client_map), !is.null(names(client_map)), all(nzchar(names(client_map)))
  )

  par_names <- names(init_par)
  urls      <- names(client_map)
  iter      <- 0L
  optim_log <- list()

  # --- ▼▼▼ ここから修正 ▼▼▼ ---

  # --- 計算結果キャッシュ用環境 ---
  cache_env <- new.env(parent = emptyenv())
  cache_env$p_vec <- NULL
  cache_env$obj   <- NULL
  cache_env$grad  <- NULL

run_and_cache <- function(p_vec) {
  if (!is.null(cache_env$p_vec) && all(cache_env$p_vec == p_vec)) return()

  iter <<- iter + 1L
  log_info("iter %d: calling %d clients", iter, length(urls))

  p_named <- setNames(p_vec, par_names)
  named_payloads <- setNames(
    lapply(urls, \(u) c(common_info, list(dataPath = client_map[[u]], p = as.list(p_named)))),
    urls
  )

  responses <- comm_fn(
    named_payloads,
    timeout   = opts$timeout   %||% 30,
    max_tries = opts$max_tries %||% 3,
    pause     = opts$pause     %||% 1
  )

  agg <- agg_fn(responses)

  cache_env$p_vec <- p_vec
  cache_env$obj   <- as.numeric(agg$objf)
  cache_env$grad  <- as.numeric(agg$grad)

  # 最初にチェック（NaNやInfで止める前にログ出力）
  if (!is.finite(cache_env$obj)) stop("Objective value is not finite.")

  # ログ記録（R内）
  optim_log[[length(optim_log) + 1L]] <<- list(
    iter = iter, par = p_vec, objf = cache_env$obj, grad = cache_env$grad
  )

  # ファイルに追記
  log_path <- "/project/logs/optimization_log.csv"
  log_df <- data.frame(
    iter = iter,
    objf = cache_env$obj,
    as.list(setNames(p_vec, par_names))
  )
  write.table(
    log_df,
    file = log_path,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(log_path),
    append = TRUE
  )
}

  # --- 目的関数 (キャッシュを利用) ---
  obj_fn <- function(p_vec) {
    run_and_cache(p_vec)
    return(cache_env$obj)
  }

  # --- 勾配関数 (キャッシュを利用) ---
  grad_fn <- function(p_vec) {
    run_and_cache(p_vec)
    stopifnot(length(cache_env$grad) == length(p_vec), all(is.finite(cache_env$grad)))
    return(cache_env$grad)
  }
  
  # --- ▲▲▲ ここまで修正 ▲▲▲ ---

  # --- 最適化実行 (変更なし) ---
  res <- optim(
    par     = init_par,
    fn      = obj_fn,
    gr      = grad_fn,
    method  = "L-BFGS-B",
    control = opts
  )

  list(
    par         = setNames(res$par, par_names),
    value       = res$value,
    convergence = res$convergence,
    log         = optim_log
  )
}