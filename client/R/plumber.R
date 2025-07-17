# plumber.R
print(getwd())
print(list.files())

library(plumber)
library(jsonlite)
library(data.table)
library(rxode2)
library(nlmixr2lib)
library(nlmixr2)
library(numDeriv)

# ユーティリティ関数群を読み込む
source("json_parser.R",               chdir = TRUE)
source("data_loader.R",               chdir = TRUE)
source("construct_model_from_JSON.R", chdir = TRUE)
source("update_model_params.R",       chdir = TRUE)
source("compute_obj_grad.R",          chdir = TRUE)

#* @apiTitle Federated popPK Client
#* @apiDescription
#*   Given JSON with modelInfo, dataPath, and initial parameters p,
#*   compute and return the objective (‐2LL) and its gradient.
#* @post /run
#* @serializer unboxedJSON
function(req, res) {
  # 1) JSON パース
  body <- tryCatch(
    fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) {
      res$status <- 400
      return(list(error = sprintf("Invalid JSON: %s", e$message)))
    }
  )

  # 2) 必須フィールドチェック
  if (is.null(body$modelInfo) || is.null(body$dataPath) || is.null(body$p)) {
    res$status <- 400
    return(list(error = "Fields 'modelInfo', 'dataPath', and 'p' are required"))
  }

  # 3) データ読み込みとモデル構築
  model_info <- body$modelInfo
  dt         <- tryCatch(
    load_data(list(dataPath = body$dataPath)),
    error = function(e) {
      res$status <- 400
      return(list(error = sprintf("Data load error: %s", e$message)))
    }
  )
  if (inherits(dt, "list") && !is.data.table(dt)) {
    # load_data が list(error=...) を返した場合
    return(dt)
  }

  rxUi_mod <- tryCatch(
    construct_model_from_JSON(model_info),
    error = function(e) {
      res$status <- 400
      return(list(error = sprintf("Model build error: %s", e$message)))
    }
  )
  if (is.list(rxUi_mod) && !inherits(rxUi_mod, "rxUi")) {
    return(rxUi_mod)
  }

  # 4) 目的関数・勾配計算
  p_vec <- unlist(body$p)
  cg <- tryCatch(
    compute_obj_grad(p_vec, rxUi_mod, dt),
    error = function(e) {
      res$status <- 500
      return(list(error = sprintf("Computation error: %s", e$message)))
    }
  )
  if (!is.list(cg) || is.null(cg$obj) || is.null(cg$grad)) {
    res$status <- 500
    return(list(error = "Unexpected compute_obj_grad output"))
  }

  # 5) 正常レスポンス
  res$status <- 200
  list(objf = cg$obj, grad = cg$grad)
}

# Plumber サーバーの起動
# このファイルを Rscript で直接叩くと以下が実行される
# if (!interactive()) {
#   pr <- plumb("plumber.R")   # 再帰させない
#   pr$run(host = "0.0.0.0", port = 8000)
# }
