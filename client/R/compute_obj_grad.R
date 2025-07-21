compute_obj_grad <- function(p, model_info, dt) {
  message("\n=== compute_obj_grad 開始 ===")
  
  # 引数の確認
  message("・names(p):")
  print(names(p))
  
  message("・p の値:")
  print(p)
  
  message("・model_info の構造:")
  str(model_info)
  
  message("・データ（dt）の概要:")
  print(summary(dt))
  print(head(dt))

  # 1) モデル構築
  message("== construct_model_from_JSON 開始 ==")
  ui_p <- tryCatch({
    construct_model_from_JSON(model_info, init_par = as.list(p))
  }, error = function(e) {
    message("!! construct_model_from_JSON 失敗: ", e$message)
    stop(e)
  })
  message("== construct_model_from_JSON 終了 ==")

  # 2) FOCEi目的関数の評価
  message("== nlmixr2 FOCEi 評価開始 ==")
  fit <- tryCatch({
    nlmixr2(
      ui_p, dt,
      est     = "focei",
      control = foceiControl(
        maxOuterIterations = 0,
        maxInnerIterations = 0,
        print = 0
      )
    )
  }, error = function(e) {
    message("!! nlmixr2 実行中にエラー: ", e$message)
    stop(e)
  })
  objf <- as.numeric(fit$objDf["FOCEi", "OBJF"])
  message("・目的関数値 (objf): ", objf)

  # 3) 数値勾配評価
  message("== 勾配計算開始 ==")
  obj_fun <- function(par) {
    tryCatch({
      ui_inner <- construct_model_from_JSON(model_info, init_par = as.list(setNames(par, names(p))))
      fit_i <- nlmixr2(
        ui_inner, dt,
        est     = "focei",
        control = foceiControl(
          maxOuterIterations = 0,
          maxInnerIterations = 0,
          print = 0
        )
      )
      as.numeric(fit_i$objDf["FOCEi", "OBJF"])
    }, error = function(e) {
      message("!! 勾配評価中の nlmixr2 エラー: ", e$message)
      stop(e)
    })
  }

  grad <- tryCatch({
    numDeriv::grad(obj_fun, p)
  }, error = function(e) {
    message("!! numDeriv::grad エラー: ", e$message)
    stop(e)
  })
  message("・勾配 (grad):")
  print(grad)

  message("=== compute_obj_grad 終了 ===")
  list(objf = objf, grad = grad)
}
