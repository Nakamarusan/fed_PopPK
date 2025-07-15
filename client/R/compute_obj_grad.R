# R/compute_obj_grad.R
library(nlmixr2)
library(numDeriv)

#' Compute objective and gradient for one client
#'
#' @param p     Named numeric vector of parameters.
#'              Names must match iniDf$name or compiled slots.
#' @param model An rxUi or rxModel object.
#' @param dt    A data.table containing this client's data.
#' @return A list with elements:
#'   - obj:  Numeric, the client's objective (–2LL)
#'   - grad: Numeric vector, gradient ∂obj/∂p (may contain NA)
compute_obj_grad <- function(p, model, dt) {
  # 1) Update model parameters (rxUi or rxModel handled)
  mdl_p <- update_model_params(model, as.list(p))

  # 2) FOCEI run with nlmixr2 (compiles internally if needed)
  fit     <- nlmixr2::nlmixr(
    mdl_p, dt,
    est     = "focei",
    control = foceiControl(maxOuterIterations = 0, print = 0)
  )
  obj_val <- fit$objDf["FOCEi", "OBJF"]

  # 3) Numeric gradient via central differences
  obj_fun <- function(x) {
    mdl_x <- update_model_params(model, as.list(setNames(x, names(p))))
    ff    <- nlmixr2::nlmixr(
      mdl_x, dt,
      est     = "focei",
      control = foceiControl(maxOuterIterations = 0, print = 0)
    )
    ff$objDf["FOCEi", "OBJF"]
  }
  grad_val <- tryCatch(
    numDeriv::grad(obj_fun, p),
    error = function(e) rep(NA_real_, length(p))
  )

  list(obj  = obj_val,
       grad = grad_val)
}
