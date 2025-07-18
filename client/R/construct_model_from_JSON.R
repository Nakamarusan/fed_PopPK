suppressPackageStartupMessages({
  library(rxode2)
  library(nlmixr2lib)
})

#' Convert `modelInfo` + (optional) `init_par` to an **rxUi** object
#'
#' @param model_info list    – 必須キー: `compartment`, `administration`, `iiv`, `res`
#' @param init_par   numeric vector or list of numerics (named) – iniDf$est を上書き
#' @return **rxUi** （まだコンパイルしていない）
#' @export
construct_model_from_JSON <- function(model_info, init_par = NULL) {

  ## ❶ validate model_info
  req <- c("compartment","administration","iiv","res")
  miss <- setdiff(req, names(model_info))
  if (length(miss)) {
    stop("model_info に欠落: ", paste(miss, collapse = ", "))
  }
  adm <- match.arg(tolower(model_info$administration), c("iv","po"))
  res <- match.arg(tolower(model_info$res),           c("add","prop","mix"))

  ## ❷ load template
  base <- sprintf("PK_%s_des", model_info$compartment)
  mdl <- tryCatch(
    readModelDb(base),
    error = function(e)
      stop("readModelDb('", base,"') 失敗: ", e$message)
  )
  if (adm == "iv") mdl <- mdl |> removeDepot()

  ## ❸ add IIV
  eta_pars <- c(
    if (isTRUE(model_info$iiv$cl)) "lcl",
    if (isTRUE(model_info$iiv$v )) "lvc"
  )
  if (length(eta_pars)) {
    mdl <- mdl |> addEta(eta_pars)
  }

  ## ❹ add residual error
  err_slots <- switch(
    res,
    add  = "addSd",
    prop = "propSd",
    mix  = c("addSd","propSd")
  )
  mdl <- mdl |> addResErr(err_slots)

  ## ❺ overwrite ini parameters, if any
  if (!is.null(init_par)) {
    # 数値ベクトルならリストに変換
    if (is.numeric(init_par) && !is.list(init_par)) {
      stopifnot(!is.null(names(init_par)), all(nzchar(names(init_par))))
      init_par <- as.list(init_par)
      names(init_par) <- names(init_par)
    }
    # リストなら中身がすべて数値かチェック
    if (is.list(init_par)) {
      if (!all(vapply(init_par, is.numeric, TRUE)))
        stop("init_par のすべての要素は数値でなければなりません")
      stopifnot(!is.null(names(init_par)), all(nzchar(names(init_par))))
    }
    # 書き込み
    for (nm in names(init_par)) {
      idx <- match(nm, mdl$iniDf$name)
      if (is.na(idx)) {
        warning("init_par: unknown parameter '", nm, "' – 無視します")
      } else {
        mdl$iniDf$est[idx] <- init_par[[nm]]
      }
    }
  }

  # 返り値は rxUi クラス
  class(mdl) <- unique(c("rxUi", setdiff(class(mdl), "rxUi")))
  mdl
}
