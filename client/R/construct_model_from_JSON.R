suppressPackageStartupMessages({
  library(rxode2)
  library(nlmixr2lib)
  library(nlmixr2)
})

#' Convert `modelInfo` + (optional) `init_par` to an **rxUi** object
#'
#' @param model_info list    – 必須キー: `compartment`, `administration`, `iiv`, `res`
#' @param init_par   numeric vector or list of numerics (named) – iniDf$est を上書き（無制約スケールとみなす）
#' @return **rxUi** （まだコンパイルしていない）
#' @export
construct_model_from_JSON <- function(model_info, init_par = NULL) {
  ## ❶ validate model_info
  req <- c("compartment", "administration", "iiv", "res")
  miss <- setdiff(req, names(model_info))
  if (length(miss)) {
    stop("model_info に欠落: ", paste(miss, collapse = ", "))
  }
  adm <- match.arg(tolower(model_info$administration), c("iv", "po"))
  res <- match.arg(tolower(model_info$res), c("add", "prop", "mix"))

  ## ❷ load template
  base <- sprintf("PK_%s_des", model_info$compartment)
  mdl <- tryCatch(
    readModelDb(base),
    error = function(e)
      stop("readModelDb('", base, "') 失敗: ", e$message)
  )
  if (adm == "iv") mdl <- mdl |> removeDepot()

  ## ❸ add IIV
  eta_pars <- c(
    if (isTRUE(model_info$iiv$cl)) "lcl",
    if (isTRUE(model_info$iiv$v )) "lvc"
  )
  if (length(eta_pars)) {
    mdl <- suppressMessages({
      mdl <- mdl |> addEta(eta_pars)
      if (isTRUE(model_info$iiv$cor)) {
        mdl <- mdl |> ini(etaLcl + etaLvc ~ c(0.1, 0.2, 0.1))
      }
      rxode2::as.rxUi(mdl)
    })
  }

  ## ❹ add residual error
  err_slots <- switch(
    res,
    add  = "addSd",
    prop = "propSd",
    mix  = c("addSd", "propSd")
  )
  ## ❹ add residual error
  mdl <- suppressMessages({
    mdl |> addResErr(err_slots)
  })

  ## ❺ overwrite ini parameters
  if (!is.null(init_par)) {
    if (is.numeric(init_par) && !is.list(init_par)) {
      stopifnot(!is.null(names(init_par)), all(nzchar(names(init_par))))
      init_par <- as.list(init_par)
    }
    if (is.list(init_par)) {
      if (!all(vapply(init_par, is.numeric, TRUE)))
        stop("init_par のすべての要素は数値でなければなりません")
      stopifnot(!is.null(names(init_par)), all(nzchar(names(init_par))))
    }

    mdl <- rxode2::as.rxUi(mdl)

    # 共分散 (etaLcl, etaLvc) の処理
    rho_nm <- "(etaLcl,etaLvc)"
    if (!is.null(init_par[[rho_nm]])) {
      cov_val <- init_par[[rho_nm]]
      message(sprintf("[LOG] 共分散として受け取った '(etaLcl,etaLvc)' = %.6f", cov_val))

      idx_cov <- match(rho_nm, mdl$iniDf$name)
      if (!is.na(idx_cov)) {
        mdl$iniDf$est[idx_cov] <- cov_val
      } else {
        etaLcl_idx <- which(mdl$iniDf$name == "etaLcl")
        etaLvc_idx <- which(mdl$iniDf$name == "etaLvc")
        cor_idx <- which(
          (mdl$iniDf$lower == etaLcl_idx & mdl$iniDf$upper == etaLvc_idx) |
          (mdl$iniDf$lower == etaLvc_idx & mdl$iniDf$upper == etaLcl_idx)
        )
        if (length(cor_idx)) {
          mdl$iniDf$est[cor_idx] <- cov_val
          log_info("[LOG] 共分散 '(etaLcl,etaLvc)' を index {cor_idx} に設定: {cov_val}")
        } else {
          warning("共分散項 '(etaLcl,etaLvc)' を iniDf 内で見つけられませんでした")
        }
      }

      # 残りのループで処理されないよう削除
      init_par[[rho_nm]] <- NULL
    }

    # 残りの通常パラメータを上書き
    for (nm in names(init_par)) {
      val <- init_par[[nm]]
      idx <- match(nm, mdl$iniDf$name)
      if (is.na(idx)) {
        warning("init_par: unknown parameter '", nm, "' – 無視します")
      } else {
        mdl$iniDf$est[idx] <- val
      }
    }

    # クラス修正
    class(mdl) <- unique(c("rxUi", setdiff(class(mdl), "rxUi")))
  }

  mdl
}
