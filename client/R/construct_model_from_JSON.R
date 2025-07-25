suppressPackageStartupMessages({
  library(rxode2)
  library(nlmixr2lib)
  library(nlmixr2)
})

#' Convert `modelInfo` + (optional) `init_par` to an **rxUi** object
#'
#' @param model_info list – 必須キー: `compartment`, `administration`, `iiv`, `res`
#' @param init_par   named numeric/list – iniDf$est を上書き（制約スケール）
#' @return rxUi object
construct_model_from_JSON <- function(model_info, init_par = NULL) {
  ## ❶ validate model_info
  req <- c("compartment", "administration", "iiv", "res")
  miss <- setdiff(req, names(model_info))
  if (length(miss)) stop("model_info に欠落: ", paste(miss, collapse = ", "))
  adm <- match.arg(tolower(model_info$administration), c("iv", "po"))
  res <- match.arg(tolower(model_info$res), c("add", "prop", "mix"))

  ## ❷ load template
  base <- sprintf("PK_%s", model_info$compartment)
  mdl <- tryCatch(
    readModelDb(base),
    error = function(e) stop("readModelDb('", base, "') 失敗: ", e$message)
  )
  if (adm == "iv") mdl <- mdl |> ini(-lka) |> model(-ka)
  message("--- DEBUG: base model ---"); print(mdl)
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
  message("--- DEBUG: ❸ add IIV 直後のiniDf ---"); print(mdl$iniDf)
  ## ❹ add residual error
  err_slots <- switch(res,
    add  = "addSd",
    prop = "propSd",
    mix  = c("addSd", "propSd")
  )
  mdl <- suppressMessages(mdl |> addResErr(err_slots))
  message("--- DEBUG: ❹ add residual error 直後のiniDf ---"); print(mdl$iniDf)
  ## ❺ iniDf$est を上書き（共分散項含む）
  if (!is.null(init_par)) {
    # iniDfを一度だけ取り出す
    iniDf <- mdl$iniDf
    
    # init_parの全ての要素をループで処理
    for (nm in names(init_par)) {
      # 'name'列に一致するものを探す
      idx <- match(nm, iniDf$name)
      if (!is.na(idx)) {
        iniDf$est[idx] <- init_par[[nm]]
      } else {
        warning(sprintf("init_par: '%s' に対応する iniDf$name が見つかりません – 無視", nm))
      }
    }
    # 更新したiniDfをモデルに戻す
    mdl$iniDf <- iniDf
  }

  # デバッグメッセージはここで最終確認
  message("--- DEBUG: 最終的なiniDf ---"); print(mdl$iniDf)

  # 明示的に rxUi クラスを保持
  class(mdl) <- unique(c("rxUi", setdiff(class(mdl), "rxUi")))
  return(mdl)
}
