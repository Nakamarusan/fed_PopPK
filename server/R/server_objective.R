# ── server_objective.R ───────────────────────────────────────────────────────
# 目的  : すべてのクライアントが返した “部分目的関数 & 勾配” を
#         単純加算（＝完全データ対数尤度の合計）で集計する。
# 期待値: 各クライアントは
#           list(objf = <numeric(1)>, grad = <numeric(k)>)
#         を返す。k はパラメータ数で全施設で共通。

aggregate_responses <- function(responses) {
  ## 0) 基本チェック ---------------------------------------------------------
  stopifnot(
    is.list(responses),
    length(responses) > 0,
    !is.null(names(responses))         # URL を名前にしているはず
  )

  ## 1) objf と grad を抽出 & 妥当性確認 -----------------------------------
  obj_vec <- vapply(responses, \(r){
    if (!is.numeric(r$objf) || length(r$objf) != 1L)
      stop("Each response$objf must be a numeric scalar")
    r$objf
  }, numeric(1))

  grad_list <- lapply(responses, function(r) {
    g <- r$grad
    if (!is.numeric(g)) stop("Each response$grad must be numeric")
    as.numeric(unlist(g))  # ★★ ここを追加 ★★
  })
  log_info("Aggregated objf = %f", sum(obj_vec))
  log_info("Aggregated grad = %s", paste(round(Reduce(`+`, grad_list), 4), collapse = ", "))
  ## 2) 勾配長が全施設で一致するか？ --------------------------------------
  g_len <- vapply(grad_list, length, integer(1))
  if (!all(g_len == g_len[1]))
    stop("All gradient vectors must have identical length (got: ",
         paste(g_len, collapse = ","), ")")

  ## 3) 集約 (単純総和) ------------------------------------------------------
  list(
    objf = sum(obj_vec),
    grad = Reduce(`+`, grad_list)
  )
}
