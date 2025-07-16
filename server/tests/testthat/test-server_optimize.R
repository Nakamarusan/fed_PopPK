library(testthat)
library(nloptr)
# Load all modules

source("//root/test/server/R/server_comm.R")
source("//root/test/server/R/server_objective.R")
source("//root/test/server/R/server_optimize.R")

test_that("server_optimize runs end-to-end with mocked comm_fn/agg_fn", {
  # 1) モックの polling 関数：パラメータによって線形な objf/grad を返す
  mock_comm <- function(urls, payload, ...) {
    p <- payload$params
    # objective = sum(p^2) per client, grad = 2*p
    lapply(urls, function(u) list(objf = sum(p^2), grad = 2*p))
  }

  # 2) モックの aggregate（パラメータ数倍を合計）
  mock_agg <- function(responses) {
    # n clients
    n <- length(responses)
    # each objf = sum(p^2), so total = n*sum(p^2)
    p2 <- responses[[1]]$grad/2
    list(
      objf = n * sum(p2^2),
      grad = n * (2 * p2)
    )
  }

  # 3) パラメータ初期値
  init_par <- c(1, -2)

  # 4) 実行
  res <- server_optimize(
    init_par     = init_par,
    client_urls  = paste0("dummy", 1:3),
    payload_base = list(other = "info"),
    opts         = list(
      algorithm = "NLOPT_LD_LBFGS",
      maxeval   = 10,
      xtol_rel  = 1e-8
    ),
    comm_fn = mock_comm,
    agg_fn  = mock_agg
  )

  # 5) 解析的に最適解は p=0, obj=0
  expect_equal(res$par, c(0,0), tolerance = 1e-4)
  expect_equal(res$value, 0, tol = 1e-6)
  expect_true(res$convergence > 0)
})
