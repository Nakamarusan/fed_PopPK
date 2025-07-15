# tests/testthat/test-update_model_params.R
library(testthat)
library(rxode2)
library(nlmixr2lib)

# テスト対象スクリプトを読み込む
source("R/json_parser.R",               chdir = TRUE)
source("R/construct_model_from_JSON.R", chdir = TRUE)
source("R/update_model_params.R",       chdir = TRUE)

test_that("update_model_params updates iniDf for rxUi models", {
  # 1) モデル情報で rxUi を作成
  model_info <- list(
    compartment    = "1cmt",
    administration = "iv",
    iiv            = list(cl = TRUE, v = TRUE),
    res            = "mix"
  )
  mdl0 <- construct_model_from_JSON(model_info)
  expect_true(inherits(mdl0, "rxUi"))

  # 2) 元の iniDf を確認
  ini0 <- mdl0$iniDf
  expect_true("lcl"      %in% ini0$name)
  expect_true("lvc"      %in% ini0$name)
  expect_true(any(grepl("AddSd$",  ini0$name)))
  expect_true(any(grepl("PropSd$", ini0$name)))

  # 3) パラメータ更新を適用
  new_params <- list(
    lcl    = 0.77,
    addSd  = 0.11,
    propSd = 0.22
  )
  mdl1 <- update_model_params(mdl0, new_params)
  ini1 <- mdl1$iniDf

  # 4) fixed effect (lcl) の更新確認
  expect_equal( ini1$est[ini1$name == "lcl"], 0.77 )

  # 5) residual-error (AddSd, PropSd) の更新確認
  idx_add  <- grep("AddSd$",  ini1$name)
  idx_prop <- grep("PropSd$", ini1$name)
  expect_true(all(ini1$est[idx_add]  == 0.11))
  expect_true(all(ini1$est[idx_prop] == 0.22))
})