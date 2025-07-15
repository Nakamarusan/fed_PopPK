# R/construct_model_from_JSON.R
library(rxode2)
library(nlmixr2lib)

#' Construct a PK model from parsed JSON configuration
#'
#' @param model_info A list with at least:
#'   - compartment   : character, e.g. "1cmt"
#'   - administration: character, "po" or "iv"
#'   - iiv           : list(cl=TRUE/FALSE, v=TRUE/FALSE)
#'   - res           : character, one of "add", "prop", or "mix"
#' @return An RxODE2 model (uncompiled), ready for rxCompile() or nlmixr()
#' @export
construct_model_from_JSON <- function(model_info) {
  # 1) 必須フィールドの存在チェック
  required <- c("compartment", "administration", "iiv", "res")
  missing <- setdiff(required, names(model_info))
  if (length(missing) > 0) {
    stop("Missing fields in model_info: ", paste(missing, collapse = ", "))
  }

  # 2) compartment の形式チェック
  if (!grepl("^[0-9]+cmt$", model_info$compartment)) {
    stop("Invalid 'compartment': must be like '1cmt' or '2cmt'.")
  }

  # 3) administration のチェック（大文字小文字混在を許容）
  adm <- tolower(model_info$administration)
  if (!adm %in% c("po", "iv")) {
    stop("Invalid 'administration': must be 'po' or 'iv'.")
  }

  # 4) residual error のチェック
  res <- tolower(model_info$res)
  res <- match.arg(res, c("add", "prop", "mix"))
  # 1) モデル名を作成（常に"_des"版を利用）
  base_name <- paste0("PK_", model_info$compartment, "_des")
  model     <- readModelDb(name = base_name)
  
  # 2) i.v.投与なら depot を除去
  if (tolower(model_info$administration) == "iv") {
    model <- model |> removeDepot()
  }
  
  # 3) 個体間変動（ETA）の追加
  eta_pars <- character()
  if (isTRUE(model_info$iiv$cl)) eta_pars <- c(eta_pars, "lcl")
  if (isTRUE(model_info$iiv$v )) eta_pars <- c(eta_pars, "lvc")
  if (length(eta_pars) > 0) {
    model <- model |> addEta(eta_pars)
  }
  
  # 4) 個体内変動（Residual Error）の追加
  err_types <- switch(
    tolower(model_info$res),
    add  = "addSd",
    prop = "propSd",
    mix  = c("addSd", "propSd"),
    stop("Unknown res type: must be 'add', 'prop', or 'mix'")
  )
  model <- model |> addResErr(err_types)
  
  return(model)
}