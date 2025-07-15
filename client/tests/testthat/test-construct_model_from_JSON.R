library(testthat)
library(rxode2)
library(nlmixr2lib)

# 関数定義を読み込む
source("R/json_parser.R", chdir = TRUE)
source("Rconstruct_model_from_JSON.R", chdir = TRUE)

test_that("construct_model_from_JSON builds a valid model object", {
  model_info <- list(
    compartment    = "1cmt",
    administration = "iv",
    iiv            = list(cl = TRUE, v = TRUE),
    res            = "mix"
  )
  mdl <- construct_model_from_JSON(model_info)
  # 1) クラスチェック
  expect_s3_class(mdl, "rxUi")

  # 2) depot が含まれていないか（静注モデル）
  #    mdl$state はベクトルで state 名を返す
  states <- mdl$state
  expect_false("depot" %in% states)

# ini ブロックに lcl, lvc が含まれているか
  ini_text <- paste(capture.output(ini(mdl)), collapse = "\n")
  expect_true(grepl("lcl <-", ini_text), info = "ini block に lcl がない")
  expect_true(grepl("lvc <-", ini_text), info = "ini block に lvc がない")

  # 残差誤差モデルのパラメータ名を取得
  err_names <- mdl$errParams

  # 末尾が "AddSd" / "PropSd" になっているか
  expect_true(any(grepl("AddSd$",  err_names)),
              info = sprintf("errParams に AddSd 系がない: %s", paste(err_names, collapse = ", ")))
  expect_true(any(grepl("PropSd$", err_names)),
              info = sprintf("errParams に PropSd 系がない: %s", paste(err_names, collapse = ", ")))
})