### compute_obj_grad.R

suppressPackageStartupMessages({
  library(nlmixr2)
  library(numDeriv)
})

inverse_transform_par <- function(par) {
  par_named <- par

  if (!is.null(par[["etaLcl"]])) par_named[["etaLcl"]] <- exp(par[["etaLcl"]])
  if (!is.null(par[["etaLvc"]])) par_named[["etaLvc"]] <- exp(par[["etaLvc"]])
  if (!is.null(par[["CcPropSd"]])) par_named[["CcPropSd"]] <- exp(par[["CcPropSd"]])

  if (!is.null(par[["(etaLcl,etaLvc)"]])) {
    rho <- tanh(par[["(etaLcl,etaLvc)"]])
    # 共分散の計算のために、内部で一時的に標準偏差を計算する
    sd_etaLcl <- sqrt(par_named[["etaLcl"]])
    sd_etaLvc <- sqrt(par_named[["etaLvc"]])
    cov_val <- rho * sd_etaLcl * sd_etaLvc
    par_named[["(etaLcl,etaLvc)"]] <- cov_val

    message(sprintf("[LOG] 相関係数変換: z = %.6f → rho = %.6f", par[["(etaLcl,etaLvc)"]], rho))
    message(sprintf("[LOG] 共分散に変換: cov(etaLcl,etaLvc) = %.6f", cov_val))
  }

  return(par_named)
}

update_model_estimates <- function(model_ui, par_named) {
  iniDf <- model_ui$iniDf

  for (name_to_update in names(par_named)) {
    val <- par_named[[name_to_update]]
    # 'name'列に一致するものを探す
    idx <- match(name_to_update, iniDf$name)
    if (!is.na(idx)) {
      iniDf$est[idx] <- val
    } else {
      warning(sprintf("parameter '%s' に対応する iniDf$name エントリが見つかりません", name_to_update))
    }
  }

  model_ui$iniDf <- iniDf
  model_ui
}

compute_obj_grad <- function(p_unconstrained, state_env) {
  # === デバッグコードを追加 ===
  message("### DEBUG: state_env$model_ui$iniDf の構造 ###")
  print(str(state_env$model_ui$iniDf))
  message("### DEBUG: state_env$model_ui$iniDf の内容 ###")
  print(state_env$model_ui$iniDf)
  # =========================
  message("=== compute_obj_grad 開始 ===")

  p_list <- as.list(p_unconstrained)
  names(p_list) <- names(p_unconstrained)
  print(p_list)

  p_real <- inverse_transform_par(p_list)
  print(p_real)

  model_ui_updated <- update_model_estimates(state_env$model_ui, p_real)

  fit <- tryCatch({
    nlmixr2(
      model_ui_updated,
      state_env$dt,
      est = "focei",
      control = foceiControl(maxOuterIterations = 0, print = 0)
    )
  }, error = function(e) {
    message("!! nlmixr2 error in objf: ", e$message)
    stop(e)
  })

  objf_val <- as.numeric(fit$objDf["FOCEi", "OBJF"])
  message(sprintf("・目的関数値 = %.6f", objf_val))

  grad_val <- tryCatch({
    numDeriv::grad(
      func = function(par_vec) {
        p_tmp <- as.list(par_vec)
        names(p_tmp) <- names(p_unconstrained)
        p_tmp_real <- inverse_transform_par(p_tmp)
        ui_tmp <- update_model_estimates(state_env$model_ui, p_tmp_real)

        fit_tmp <- nlmixr2(
          ui_tmp, state_env$dt,
          est = "focei",
          control = foceiControl(maxOuterIterations = 0, maxInnerIterations = 0, print = 0)
        )
        as.numeric(fit_tmp$objDf["FOCEi", "OBJF"])
      },
      x = unlist(p_unconstrained)
    )
  }, error = function(e) {
    message("!! numDeriv::grad error: ", e$message)
    stop(e)
  })

  print(grad_val)
  message("=== compute_obj_grad 終了 ===")

  list(objf = objf_val, grad = grad_val)
}