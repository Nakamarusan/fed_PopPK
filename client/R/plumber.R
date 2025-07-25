### plumber.R

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(data.table)
})

# --- 外部スクリプト読込 ---
source("construct_model_from_JSON.R", chdir = TRUE)
source("data_loader.R",               chdir = TRUE)
source("compute_obj_grad.R",          chdir = TRUE)

# --- グローバル状態（初期化済みモデルなど保持） ---
.global_state <- new.env(parent = emptyenv())

#* 初期化エンドポイント
#* @param modelInfo:list   JSON の modelInfo 
#* @param initPar:list     JSON の initPar（無制約スケール）
#* @param dataPath:string  データ CSV/RDS へのパス
#* @post /init
function(req, res){
  message("=== /init エンドポイント ===")
  payload <- fromJSON(req$postBody, simplifyVector = TRUE)

  .global_state$modelInfo <- payload$modelInfo
  .global_state$initPar   <- payload$initPar
  .global_state$dt        <- load_data(payload$dataPath)

  .global_state$model_ui <- construct_model_from_JSON(model_info = payload$modelInfo, init_par = payload$initPar)

  message("モデル情報・初期値・データのロードと構築完了")
  list(status = "initialized")
}

#* OBJ & 勾配を返すエンドポイント
#* @param p:numeric  パラメータベクトル（名前付き numeric, 無制約スケール）
#* @post /run
function(req, res){
  message("=== /run エンドポイント ===")
  message("\n=== RAW req$postBody ===\n", req$postBody)

  result <- tryCatch({
    payload <- fromJSON(req$postBody, simplifyVector = FALSE)
    str(payload$p)
    p_vec <- unlist(payload$p, use.names = TRUE)

    out <- compute_obj_grad(
      p_unconstrained = p_vec,
      state_env = .global_state
    )

    grad_safe <- lapply(out$grad, function(x) if (is.finite(x)) x else NA_real_)
    list(objf = as.numeric(out$objf), grad = grad_safe)

  }, error = function(e) {
    message("!!! compute_obj_grad() エラー: ", conditionMessage(e))
    res$status <- 500
    list(error = conditionMessage(e))
  })
}