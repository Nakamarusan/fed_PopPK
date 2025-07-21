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
  base <- sprintf("PK_%s_des", model_info$compartment)
  mdl <- tryCatch(
    readModelDb(base),
    error = function(e) stop("readModelDb('", base, "') 失敗: ", e$message)
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
  err_slots <- switch(res,
    add  = "addSd",
    prop = "propSd",
    mix  = c("addSd", "propSd")
  )
  mdl <- suppressMessages(mdl |> addResErr(err_slots))

  ## ❺ prepare iniDf with `label`
  iniDf <- mdl$iniDf
  iniDf$label <- iniDf$name  # 初期状態として name を label にコピー

  # 論理名 → name 対応のマップ
  name_map <- list(
    "etaLcl" = "omega(1,1)",
    "etaLvc" = "omega(2,2)",
    "(etaLcl,etaLvc)" = "omega(1,2)",
    "CcPropSd" = "prop.sd",
    "lcl" = "lcl",
    "lvc" = "lvc"
  )

  # label 列を上書き
  for (label in names(name_map)) {
    name <- name_map[[label]]
    if (name %in% iniDf$name) {
      iniDf$label[iniDf$name == name] <- label
    }
  }

  mdl$iniDf <- iniDf

  ## ❻ iniDf$est を上書き（共分散項含む）
  if (!is.null(init_par)) {
    if (is.numeric(init_par) && !is.list(init_par)) {
      stopifnot(!is.null(names(init_par)), all(nzchar(names(init_par))))
      init_par <- as.list(init_par)
    }
    if (!all(vapply(init_par, is.numeric, TRUE))) {
      stop("init_par のすべての要素は数値でなければなりません")
    }

    rho_nm <- "(etaLcl,etaLvc)"
    if (!is.null(init_par[[rho_nm]])) {
      cov_val <- init_par[[rho_nm]]
      message(sprintf("[LOG] 共分散として受け取った '%s' = %.6f", rho_nm, cov_val))

      idx_cov <- match(rho_nm, iniDf$label)
      if (!is.na(idx_cov)) {
        iniDf$est[idx_cov] <- cov_val
      } else {
        warning(sprintf("共分散 '%s' を iniDf$label に見つけられません", rho_nm))
      }

      init_par[[rho_nm]] <- NULL  # 他と重複しないよう削除
    }

    for (nm in names(init_par)) {
      idx <- match(nm, iniDf$label)
      if (!is.na(idx)) {
        iniDf$est[idx] <- init_par[[nm]]
      } else {
        warning(sprintf("init_par: '%s' に対応する iniDf$label が見つかりません – 無視", nm))
      }
    }

    mdl$iniDf <- iniDf
  }

  # 明示的に rxUi クラスを保持
  class(mdl) <- unique(c("rxUi", setdiff(class(mdl), "rxUi")))
  return(mdl)
}
