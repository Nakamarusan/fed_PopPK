# R/run_federated_optimization.R
library(nlmixr2)
library(numDeriv)

#' Run FOCEI optimization for one client, without caching
#'
#' @param init_par Numeric vector of initial, unconstrained parameters.
#' @param model    An rxUi or rxModel object.
#' @param dt       A data.table with the client's data.
#' @param control  List of control args for optim().
#' @return A list with elements:
#'   - opt    : result of optim()
#'   - history: data.frame of parameters and obj at each call of fn()
#' @export
run_federated_optimization <- function(init_par, model, dt, control = list()) {
  history <- list()
  iter    <- 0

  # 目的関数
  obj_fun <- function(p) {
    iter <<- iter + 1
    res   <- compute_obj_grad(p, model, dt)
    # 履歴にパラメータと obj を記録
    history[[iter]] <<- c(iter = iter,
                          setNames(p, paste0("par", seq_along(p))),
                          obj = res$obj)
    res$obj
  }

  # 勾配関数
  grad_fun <- function(p) {
    compute_obj_grad(p, model, dt)$grad
  }

  # 最適化実行
  opt <- optim(
    par     = init_par,
    fn      = obj_fun,
    gr      = grad_fun,
    method  = "L-BFGS-B",
    control = control
  )

  # 履歴を data.frame に変換
  history_df <- do.call(rbind, lapply(history, function(x) as.data.frame(as.list(x))))
  rownames(history_df) <- NULL

  list(opt     = opt,
       history = history_df)
}
