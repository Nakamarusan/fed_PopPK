# client/R/plumber.R

#* @apiTitle FedPopPK Client API

library(plumber)
library(jsonlite)
library(data.table)

# ユーティリティ読み込み
source("construct_model_from_JSON.R", chdir = TRUE)
source("data_loader.R",               chdir = TRUE)
source("compute_obj_grad.R",          chdir = TRUE)

# グローバルに保持する状態
.global_state <- new.env(parent = emptyenv())

# 無制約 -> 制約スケールへの逆変換
inverse_transform_par <- function(p) {
  if (!is.null(p$etaLcl)) p$etaLcl <- sqrt(exp(p$etaLcl))
  if (!is.null(p$etaLvc)) p$etaLvc <- sqrt(exp(p$etaLvc))
  if (!is.null(p[["(etaLcl,etaLvc)"]])) p[["(etaLcl,etaLvc)"]] <- tanh(p[["(etaLcl,etaLvc)"]])
  if (!is.null(p$CcPropSd)) p$CcPropSd <- exp(p$CcPropSd)
  p
}

#* 初期化エンドポイント
#* @param modelInfo:list   JSON の modelInfo 
#* @param initPar:list     JSON の initPar 
#* @param dataPath:string  データ CSV/RDS へのパス
#* @post /init
function(req, res){
  message("=== /init エンドポイント ===")
  payload <- fromJSON(req$postBody, simplifyVector = TRUE)

  .global_state$modelInfo <- payload$modelInfo
  .global_state$initPar   <- payload$initPar
  .global_state$dt        <- load_data(payload$dataPath)

  message("モデル情報・初期値・データのロード完了")
  list(status = "initialized")
}

#* OBJ & 勾配を返すエンドポイント
#* @param p:numeric  パラメータベクトル（名前付き numeric）
#* @post /run
function(req, res){
  message("=== /run エンドポイント ===")
  message("\n=== RAW req$postBody ===\n", req$postBody)

  result <- tryCatch({
    payload <- fromJSON(req$postBody, simplifyVector = FALSE)

    message("\n=== JSON 変換後の payload$p ===")
    str(payload$p)

    # --- 逆変換関数 ---
    inv_transform_par <- function(par) {
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

      par_named
    }

    # 逆変換の適用
    p_transformed <- inv_transform_par(payload$p)
    message("\n=== 逆変換後のパラメータ ===")
    str(unlist(p_transformed))

    p_vec <- unlist(p_transformed, use.names = TRUE)

    out <- compute_obj_grad(
      p_vec,
      .global_state$modelInfo,
      .global_state$dt
    )

    message("=== compute_obj_grad() の出力 ===")
    message("objf = ", out$objf)
    message("grad (length) = ", length(out$grad))

    # 非有限値の除去（jsonlite 対応）
    grad_safe <- lapply(out$grad, function(x) if (is.finite(x)) x else NA_real_)

    list(objf = as.numeric(out$objf), grad = grad_safe)

  }, error = function(e) {
    message("!!! compute_obj_grad() でエラー:", conditionMessage(e))
    res$status <- 500
    list(error = conditionMessage(e))
  })
  message("レスポンス返却直前: ", jsonlite::toJSON(result, auto_unbox = TRUE))
  flush.console()
  return(result)
}
