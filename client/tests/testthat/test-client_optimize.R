# tests/testthat/test-client_optimize_fn.R
library(testthat)
library(jsonlite)
library(data.table)

#── client_optimize.R を読み込む ────────────────────────────────
# ここではテストファイルが tests/testthat にあるので、
# ../R/client_optimize.R を指定し、chdir=TRUE でそのフォルダに移動します。
source("../R/client_optimize.R", chdir = TRUE)

test_that("client_optimize() wiring works end-to-end", {
  # --- モック用のダミーデータ・モデル・結果を用意 ---
  stub_model_info <- list(
    compartment    = "1cmt",
    administration = "iv",
    iiv            = list(cl = FALSE, v = FALSE),
    res            = "prop"
  )
  stub_dt   <- data.table(dummy = 1:3)
  stub_model <- structure(list(), class = "dummyModel")
  # client_optimize() は以下の形で返しますので、dummy_res も合わせます
  dummy_res <- list(
    par         = c(propSd = 0.1),
    value       = 42,
    convergence = 0,
    history     = data.frame(iter = 1L, par1 = 0.1, obj = 42)
  )

  # --- モック関数をグローバル環境に差し替え ---
  assign("parse_model_json_basic",
         function(path) stub_model_info,
         envir = .GlobalEnv)
  assign("load_data",
         function(cfg) stub_dt,
         envir = .GlobalEnv)
  assign("construct_model_from_JSON",
         function(mi) stub_model,
         envir = .GlobalEnv)
  assign("run_federated_optimization",
         function(init_par, model, dt, control) dummy_res,
         envir = .GlobalEnv)

  # --- 一時的な config.json を作成 ---
  cfg <- list(
    modelInfo    = stub_model_info,
    dataPath     = "dummy.csv",
    initPar      = list(propSd = 0.1),
    optimControl = list(maxit = 1)
  )
  tmp_cfg <- tempfile(fileext = ".json")
  write_json(cfg, tmp_cfg, auto_unbox = TRUE)

  # --- client_optimize() を呼び出して結果を取得 ---
  res <- client_optimize(tmp_cfg)

  # --- モック結果とまったく一致するかチェック ---
  expect_identical(res, dummy_res)
})
