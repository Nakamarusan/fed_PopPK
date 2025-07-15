# tests/testthat/test-data_loader.R
library(testthat)
library(data.table)

# 関数定義を読み込む
source("R/data_loader.R", chdir = TRUE)

test_that("load_data returns a data.table with correct contents", {
  # 1) 一時CSVを作成
  tmp <- tempfile(fileext = ".csv")
  dt_in <- data.table(id = 1:3, value = c(10.5, 20.5, 30.5))
  fwrite(dt_in, tmp)
  
  # 2) load_dataを呼び出し
  cfg <- list(dataPath = tmp)
  dt_out <- load_data(cfg)
  
  # 3) 結果の型と内容を検証
  expect_s3_class(dt_out, "data.table")
  expect_equal(nrow(dt_out), 3L)
  expect_equal(names(dt_out), c("id", "value"))
  expect_equal(dt_out$id, dt_in$id)
  expect_equal(dt_out$value, dt_in$value)
})

test_that("load_data throws error when dataPath is missing or invalid", {
  # 空文字列の場合
  cfg1 <- list(dataPath = "")
  expect_error(load_data(cfg1), "cfg\\$dataPath must be a non-empty string")
  
  # 存在しないファイルパスの場合
  fake <- tempfile(fileext = ".csv")
  cfg2 <- list(dataPath = fake)
  expect_error(load_data(cfg2), "Data file not found")
})
