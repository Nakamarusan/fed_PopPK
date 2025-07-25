# server_optimize.R
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
  stopifnot(
    is.numeric(init_par),
    !is.null(names(init_par)),
    all(nzchar(names(init_par))),
    is.character(client_map),
    !is.null(names(client_map)),
    all(nzchar(names(client_map)))
  )

  par_names <- names(init_par)
  urls      <- names(client_map)
  iter      <- 0L

  # --- ログ保存リスト
  optim_log <- list()

  # --- 目的関数
  obj_fn <- function(p_vec) {
    iter <<- iter + 1L
    log_info("iter %d: calling %d clients", iter, length(urls))

    p_named <- setNames(p_vec, par_names)

    named_payloads <- setNames(
      lapply(urls, \(u) c(
        common_info,
        list(
          dataPath = client_map[[u]],
          p        = as.list(p_named)
        )
      )),
      urls
    )

    responses <- comm_fn(
      named_payloads,
      timeout   = opts$timeout   %||% 30,
      max_tries = opts$max_tries %||% 3,
      pause     = opts$pause     %||% 1
    )

    agg <- agg_fn(responses)
    obj <- as.numeric(agg$objf)
    grad <- as.numeric(agg$grad)

    # ログ記録
    optim_log[[length(optim_log) + 1]] <<- list(
      iter = iter,
      par  = p_vec,
      objf = obj,
      grad = grad
    )

    stopifnot(is.finite(obj))
    obj
  }

  # --- 勾配関数
  grad_fn <- function(p_vec) {
    p_named <- setNames(p_vec, par_names)

    named_payloads <- setNames(
      lapply(urls, \(u) c(
        common_info,
        list(
          dataPath = client_map[[u]],
          p        = as.list(p_named)
        )
      )),
      urls
    )

    responses <- comm_fn(
      named_payloads,
      timeout   = opts$timeout   %||% 30,
      max_tries = opts$max_tries %||% 3,
      pause     = opts$pause     %||% 1
    )

    agg  <- agg_fn(responses)
    grad <- as.numeric(agg$grad)
    stopifnot(length(grad) == length(p_vec), all(is.finite(grad)))
    grad
  }

  # --- 最適化実行
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
