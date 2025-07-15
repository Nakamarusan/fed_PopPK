# R/update_model_params.R
library(rxode2)

#' Update model initial parameters for rxUi or compiled rxModel
#'
#' @param mdl       An uncompiled rxUi (has iniDf) or compiled rxModel (S4 class)
#' @param param_map Named numeric list of parameters to update.
#'                  - For fixed effects: names like "lcl", "lvc", etc.
#'                  - For residual error: "addSd" or "propSd".
#' @return The model object (same class as input) with updated init values.
#' @export
update_model_params <- function(mdl, param_map) {
  # ─────────────────────────────────────────────────────────────
  # ① Uncompiled model (rxUi) の場合
  # ─────────────────────────────────────────────────────────────
  if (inherits(mdl, "rxUi")) {
    ini_df   <- mdl$iniDf
    err_list <- mdl$errParams
    
    for (nm in names(param_map)) {
      val <- param_map[[nm]]
      # residual-error の場合
      if (tolower(nm) %in% c("addsd","propsd")) {
        # errParams 内の実名 ("CcAddSd" など) を探して更新
        matches <- grep(nm, err_list, ignore.case = TRUE, value = TRUE)
        if (length(matches) == 0) {
          warning("No matching residual-error param for '", nm, "'")
        }
        for (ename in matches) {
          idx <- which(ini_df$name == ename)
          ini_df$est[idx] <- val
        }
      } else {
        # fixed effect の場合
        idx <- which(ini_df$name == nm)
        if (length(idx) == 1) {
          ini_df$est[idx] <- val
        } else {
          warning("Parameter '", nm, "' not found or ambiguous in iniDf")
        }
      }
    }
    mdl$iniDf <- ini_df
    return(mdl)
  }
  
  # ─────────────────────────────────────────────────────────────
  # ② Compiled model (rxModel) の場合
  # ─────────────────────────────────────────────────────────────
  if (inherits(mdl, "rxModel")) {
    for (nm in names(param_map)) {
      val <- param_map[[nm]]
      if (tolower(nm) %in% c("addsd","propsd")) {
        # errNames slot からマッチする名前を探して更新
        err_names <- slot(mdl@model, "errNames")
        matches   <- grep(nm, err_names, ignore.case = TRUE, value = TRUE)
        if (length(matches) == 0) {
          warning("No matching residual-error param for '", nm, "' in compiled model")
        }
        for (ename in matches) {
          mdl@model@err[ename] <- val
        }
      } else {
        # theta slot を更新
        if (nm %in% names(mdl@model@theta)) {
          mdl@model@theta[nm] <- val
        } else {
          warning("Theta '", nm, "' not found in compiled model")
        }
      }
    }
    return(mdl)
  }
  
  stop("Unsupported model object: must inherit from 'rxUi' or 'rxModel'")
}
