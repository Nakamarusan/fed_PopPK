suppressPackageStartupMessages({
  library(data.table)      # 必須
})

# optional back‑ends (使えるときだけロード)
.has_arrow <- requireNamespace("arrow", quietly = TRUE)
.has_qs    <- requireNamespace("qs",    quietly = TRUE)

# -------------------------------------------------------------------------
#' Read a local data set into **data.table**
#'
#' Supported formats  | extension | 依存パッケージ
#' ------------------ | --------- | -------------
#' Comma‑Separated    | `.csv`    | data.table  (fread)
#' RDS (saveRDS)      | `.rds`    | base R      (readRDS)
#' Apache Parquet     | `.parquet`| **arrow**
#' qs (fast serial.)  | `.qs`     | **qs**
#'
#' @param data_path character – path inside the container (ABSOLUTE RECOMMENDED)
#' @param require_cols character|NULL – 必須列を指定すると存在確認してくれる
#' @return data.table
#' @export
# -------------------------------------------------------------------------
load_data <- function(data_path){

  ## ❶ path validation -----------------------------------------------------
  if (is.null(data_path) || !nzchar(data_path))
    stop("`data_path` must be a non‑empty string")

  if (!file.exists(data_path))
    stop("Data file not found: ", data_path)

  ## ❷ dispatch by extension ----------------------------------------------
  ext <- tolower(tools::file_ext(data_path))

  dt  <- switch(
    ext,
    csv = tryCatch(
            fread(data_path, showProgress = FALSE),
            error = function(e)
              stop("fread() failed for CSV (", basename(data_path), "): ",
                   e$message, call. = FALSE)
          ),

    rds = as.data.table(readRDS(data_path)),

    parquet = {
      if (!.has_arrow)
        stop("Reading parquet requires the `arrow` package; please add it to renv.lock")
      as.data.table(arrow::read_parquet(data_path))
    },

    qs = {
      if (!.has_qs)
        stop("Reading qs requires the `qs` package; please add it to renv.lock")
      as.data.table(qs::qread(data_path))
    },

    stop("Unsupported file extension: .", ext)
  )

  ## ❸ basic sanity check --------------------------------------------------
  if (!is.data.table(dt))
    dt <- as.data.table(dt)

  invisible(setDT(dt))   # return by reference
}
