# tests/test_json_parser.R
library(testthat)
source("Rscript/json_parser.R", chdir = TRUE)

test_that("parse_model_json_basic parses minimal JSON", {
  # 一時ファイルにサンプル JSON を書き込む
  json_txt <- '{"modelInfo": {"compartment":"1cpt","administration":"iv","iiv":{"cl":true,"v":true},"res":"prop"}}'
  tmp <- tempfile(fileext = ".json")
  writeLines(json_txt, tmp)

  # 関数呼び出し
  mi <- parse_model_json_basic(tmp)

  # 中身を検証
  expect_equal(mi$compartment, "1cpt")
  expect_equal(mi$administration, "iv")
  expect_true(mi$iiv$cl)
  expect_true(mi$iiv$v)
  expect_equal(mi$res, "prop")
})
#' # Suppose model.json contains:
#' # {
#' #   "modelInfo": {
#' #     "compartment": "1cpt",
#' #     "administration": "iv",
#' #     "iiv": { "cl": true, "v": true }
#' #   }
#' # }
#' parse_model_json_basic("path/to/model.json")
#' #> $compartment
#' #> [1] "1cpt"
#' #> 
#' #> $administration
#' #> [1] "iv"
#' #> 
#' #> $iiv
#' #> $iiv$cl
#' #> [1] TRUE
#' #> 
#' #> $iiv$v
#' #> [1] TRUE
#'