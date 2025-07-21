### compute_obj_grad.R

suppressPackageStartupMessages({
  library(nlmixr2)
  library(numDeriv)
})

parameter_name_map <- list(
  "etaLcl" = "omega(1,1)",
  "etaLvc" = "omega(2,2)",
  "(etaLcl,etaLvc)" = "omega(1,2)",
  "CcPropSd" = "prop.sd",
  "lcl" = "lcl",
  "lvc" = "lvc"
)

inverse_transform_par <- function(par) {
  par_named <- par

  if (!is.null(par[["etaLcl"]])) par_named[["etaLcl"]] <- sqrt(exp(par[["etaLcl"]]))
  if (!is.null(par[["etaLvc"]])) par_named[["etaLvc"]] <- sqrt(exp(par[["etaLvc"]]))
  if (!is.null(par[["CcPropSd"]])) par_named[["CcPropSd"]] <- exp(par[["CcPropSd"]])

  if (!is.null(par[["(etaLcl,etaLvc)"]])) {
    rho <- tanh(par[["(etaLcl,etaLvc)"]])
    sd_etaLcl <- par_named[["etaLcl"]]
    sd_etaLvc <- par_named[["etaLvc"]]
    cov_val <- rho * sd_etaLcl * sd_etaLvc
    par_named[["(etaLcl,etaLvc)"]] <- cov_val

    message(sprintf("[LOG] 相関係数変換: z = %.6f → rho = %.6f", par[["(etaLcl,etaLvc)"]], rho))
    message(sprintf("[LOG] 共分散に変換: cov(etaLcl,etaLvc) = %.6f", cov_val))
  }

  return(par_named)
}

update_model_estimates <- function(model_ui, par_named) {
  iniDf <- model_ui$iniDf
  name_map <- setNames(iniDf$name, iniDf$label)

  for (label in names(par_named)) {
    val <- par_named[[label]]
    if (label %in% names(name_map)) {
      name <- name_map[[label]]
      iniDf$est[iniDf$name == name] <- val
    } else {
      warning(sprintf("parameter '%s' に対応する iniDf エントリが見つかりません", label))
    }
  }

  model_ui$iniDf <- iniDf
  model_ui
}

compute_obj_grad <- function(p_unconstrained, state_env) {
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
      control = foceiControl(maxOuterIterations = 0, maxInnerIterations = 0, print = 0)
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