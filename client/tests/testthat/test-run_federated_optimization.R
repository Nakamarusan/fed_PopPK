# tests/testthat/test-run_federated_optimization.R
library(testthat)

# テスト対象スクリプトを読み込む
source("R/run_federated_optimization.R", chdir = TRUE)

test_that("run_federated_optimization works with stubbed compute_obj_grad", {
  # --- モック定義: compute_obj_grad を二次関数に置き換え ---
  # ここでは obj = sum(p^2), grad = 2*p
  assign("compute_obj_grad", 
         function(p, model, dt) list(obj = sum(p^2), grad = 2 * p),
         envir = .GlobalEnv)
  
  # ダミーの model, dt（使われないのでNULLでOK）
  dummy_model <- NULL
  dummy_dt    <- NULL

  # 初期パラメータ
  init_par <- c(1, -2, 3)

  # 最適化（maxitを小さくしても、簡単な関数なので収束するはず）
  res <- run_federated_optimization(
    init_par = init_par,
    model    = dummy_model,
    dt       = dummy_dt,
    control  = list(maxit = 50)
  )

  # optim が最小 (0,0,0) に近い解を返す
  expect_true(all(abs(res$opt$par) < 1e-3))

  # history の構造チェック
  hist <- res$history
  expect_s3_class(hist, "data.frame")
  expect_true(nrow(hist) >= 1)
  # 列名: iter, par1, par2, par3, obj
  expect_true(all(c("iter", "par1", "par2", "par3", "obj") %in% names(hist)))

  # 最終行の obj は res$opt$value と一致
  last_row <- hist[nrow(hist), ]
  expect_equal(res$opt$value, last_row$obj)
})
