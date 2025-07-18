# compute_obj_grad.R

suppressPackageStartupMessages({
  library(rxode2)
  library(nlmixr2)
  library(numDeriv)
})

#' Compute FOCEi objective & numeric gradient for one client
#'
#' @param p          named numeric vector of unconstrained parameters
#' @param model_info list, construct_model_from_JSON() に渡す modelInfo
#' @param dt         data.table: このクライアントの観測データ
#' @return list(objf, grad)
compute_obj_grad <- function(p, model_info, dt) {
  message("\n=== compute_obj_grad: names(p) ===")
  print(names(p))
  # 1) rxUi を p で再構築
  ui_p <- construct_model_from_JSON(
    model_info,
    init_par = as.list(p)
  )

  # 2) FOCEi 目的関数評価
  fit <- nlmixr2(
    ui_p, dt,
    est     = "focei",
    control = foceiControl(
      maxOuterIterations = 0,
      maxInnerIterations = 0,
      print = 0
    )
  )
  objf <- as.numeric(fit$objDf["FOCEi", "OBJF"])

  # 3) 数値勾配
  obj_fun <- function(par) {
    ui_inner <- construct_model_from_JSON(
      model_info,
      init_par = as.list(setNames(par, names(p)))
    )
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
  }
  grad <- numDeriv::grad(obj_fun, p)

  list(objf = objf, grad = grad)
}
