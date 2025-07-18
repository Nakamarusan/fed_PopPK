# ── server_optimize.R ────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(nloptr)
  library(logger)
})

`%||%` <- function(a, b) if (is.null(a)) b else a   # NULL 合体演算子

#' Run the global optimisation loop (federated LBFGS)
#'
#' @param init_par     named numeric – unconstrained starting values
#' @param client_map   named character – *names = URL*, *value = dataPath*
#' @param common_info  list – fields common to **all** clients (modelInfo など)
#' @param opts         list – passed verbatim to **nloptr**
#' @param comm_fn      function – low‑level RPC (default `poll_clients`)
#' @param agg_fn       function – aggregation (default `aggregate_responses`)
#' @return list(par, value, convergence)
server_optimize <- function(init_par,
                            client_map,
                            common_info = list(),
                            opts        = list(
                              algorithm   = "NLOPT_LD_LBFGS",
                              print_level = 5,
                              maxeval     = 100,
                              xtol_rel    = 1e-6
                            ),
                            comm_fn = poll_clients,
                            agg_fn  = aggregate_responses) {

  ## 0. 引数バリデーション
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

  ## 1. nloptr から呼ばれる “目的関数 + 勾配”
  eval_grad_fn <- function(p_vec) {
    iter <<- iter + 1L
    log_info("iter %d: calling %d clients", iter, length(urls))

    # (a) 名前を復元
    p_named <- setNames(p_vec, par_names)

    # (b) 各クライアント向けペイロードを作成
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

    # (c) 並列リクエスト
    responses <- comm_fn(
      named_payloads,
      timeout   = opts$timeout   %||% 30,
      max_tries = opts$max_tries %||% 3,
      pause     = opts$pause     %||% 1
    )

    # (d) 集約
    agg  <- agg_fn(responses)
    obj  <- as.numeric(agg$objf)
    grad <- as.numeric(agg$grad)

    # (e) 整合性チェック
    stopifnot(
      length(grad) == length(p_vec),
      is.finite(obj),
      all(is.finite(grad))
    )

    list(objective = obj, gradient = grad)
  }

  ## 2. nloptr 実行
  res <- nloptr::nloptr(
    x0          = as.numeric(init_par),
    eval_f      = \(x) eval_grad_fn(x)$objective,
    eval_grad_f = \(x) eval_grad_fn(x)$gradient,
    opts        = opts
  )

  ## 3. 戻り値
  list(
    par         = setNames(res$solution, par_names),
    value       = res$objective,
    convergence = res$status
  )
}
