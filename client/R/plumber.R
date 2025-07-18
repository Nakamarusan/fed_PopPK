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

#* 初期化エンドポイント
#* @param modelInfo:list   JSON の modelInfo 
#* @param initPar:list     JSON の initPar 
#* @param dataPath:string  データ CSV/RDS へのパス
#* @post /init
function(req, res){
  payload <- fromJSON(req$postBody, simplifyVector = TRUE)
  .global_state$modelInfo <- payload$modelInfo
  .global_state$initPar   <- payload$initPar
  .global_state$dt        <- load_data(payload$dataPath)
  list(status = "initialized")
}

#* OBJ & 勾配を返すエンドポイント
#* @param p:numeric  パラメータベクトル（名前付き numeric）
#* @post /run
function(req, res){
  message("\n=== RAW /run body ===\n", req$postBody,
          "\n=== after jsonlite::fromJSON ===")
  payload <- fromJSON(req$postBody, simplifyVector = FALSE)
  str(payload$p)
  p_vec   <- unlist(payload$p, use.names = TRUE)  # names を保持
  str(p_vec)
  out     <- compute_obj_grad(
    p_vec,
    .global_state$modelInfo,
    .global_state$dt
  )
  # サーバー側で期待されるフィールド名は "objf"
  list(
    objf = out$objf,
    grad = out$grad
  )
}
