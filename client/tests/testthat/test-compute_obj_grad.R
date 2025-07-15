# tests/testthat/test-compute_obj_grad.R
library(testthat)
library(data.table)
library(rxode2)
library(nlmixr2lib)
library(nlmixr2)
library(numDeriv)

# 関数定義を読み込む
source("R/update_model_params.R",       chdir = TRUE)
source("R/construct_model_from_JSON.R", chdir = TRUE)
source("R/compute_obj_grad.R",          chdir = TRUE)

test_that("compute_obj_grad returns numeric obj and grad (res='prop') on rxUi", {
  # デモデータ読み込み
  dt_path <- "/root/test/data/data1_5.csv"
  dt <- fread(dt_path)

  # rxUi モデルの構築（residual only: prop）
  model_info <- list(
    compartment    = "1cmt",
    administration = "iv",
    iiv            = list(cl = FALSE, v = FALSE),
    res            = "prop"
  )
  rxUi_mod <- construct_model_from_JSON(model_info)
  expect_true(inherits(rxUi_mod, "rxUi"))

  # propSd の名前と初期値を取得
  err_names <- rxUi_mod$errParams
  prop_name <- grep("PropSd$", err_names, value = TRUE)[1]
  p0        <- setNames(rxUi_mod$iniDf$est[rxUi_mod$iniDf$name == prop_name],
                        "propSd")

  # compute_obj_grad を呼び出し（rxUi モデルを直接渡す）
  res <- compute_obj_grad(p0, rxUi_mod, dt)

  # 型と長さのチェック
  expect_type(res$obj,  "double")
  expect_length(res$obj, 1)

  expect_type(res$grad, "double")
  expect_length(res$grad, 1)
})
