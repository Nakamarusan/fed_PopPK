# tests/testthat/test-compute_obj_grad.R
library(testthat)
library(mockery)   # CRAN: mockery, stub() で関数差し替え

# テスト対象を読み込む
source("../../R/compute_obj_grad.R")

# テスト用ダミー ui
make_dummy_ui <- function() {
  # simulationIniModel() だけあればよい
  sim <- function() {
    ini({ x <- 1 })
    model({ dx <- 0 })
  }
  ui <- list(simulationIniModel = sim)
  class(ui) <- c("rxUi", "raw")
  ui
}

test_that("compile_once() が rxode2/rxCompile を通して rxDll を返す", {
  dummy_ui <- make_dummy_ui()

  # rxode2(), rxCompile() をモックしてしまう
  stub(compile_once, 'rxode2', function(x) structure(list(modVars=TRUE), class="rxode2"))
  stub(compile_once, 'rxCompile', function(x) structure(list(dll="ok"), class="rxDll"))

  out1 <- compile_once(dummy_ui)
  expect_s3_class(out1, "rxDll")
  expect_equal(out1$dll, "ok")
})

test_that("同じモデルであればキャッシュから返る", {
  dummy_ui <- make_dummy_ui()

  # まずはカウントするrxCompile をモック
  call_cnt <- 0
  stub(compile_once, 'rxode2', function(x) structure(list(modVars=TRUE), class="rxode2"))
  stub(compile_once, 'rxCompile', function(x) { call_cnt <<- call_cnt + 1; structure(list(dll = call_cnt), class="rxDll") })

  # 1回目: キャッシュなし→call_cnt==1
  out1 <- compile_once(dummy_ui)
  expect_equal(out1$dll, 1)
  expect_equal(call_cnt, 1)

  # 2回目: 同じキーなのでキャッシュヒット→call_cnt は増えない
  out2 <- compile_once(dummy_ui)
  expect_equal(out2$dll, 1)
  expect_equal(call_cnt, 1)
})

test_that("不正なオブジェクトを渡すとエラーになる", {
  expect_error(compile_once(123), "unsupported model_ui type")
})
