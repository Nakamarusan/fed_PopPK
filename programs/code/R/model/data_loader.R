# data_loader.R
# Loads local datasets into data.table for client-side evaluation.
# Supports CSV, RDS, Parquet, and qs files.
suppressPackageStartupMessages({
  library(data.table)      # always return data.table
})

.has_arrow <- requireNamespace("arrow", quietly = TRUE)
.has_qs    <- requireNamespace("qs",    quietly = TRUE)

# -------------------------------------------------------------------------
#' Load a local data file as data.table.
#'
#' Supported formats:
#' -------- | ------ | --------------
#' CSV      | `.csv` | data.table (fread)
#' RDS      | `.rds` | base R (readRDS)
#' Parquet  | `.parquet` | arrow
#' qs       | `.qs`  | qs
#'
#' @param data_path Path to an input dataset file.
#' @return data.table
#' @export
# -------------------------------------------------------------------------
load_data <- function(data_path){

  if (is.null(data_path) || !nzchar(data_path))
    stop("`data_path` must be a non-empty string")

  if (!file.exists(data_path))
    stop("Data file not found: ", data_path)

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

  if (!is.data.table(dt))
    dt <- as.data.table(dt)

  invisible(setDT(dt))   # return by reference
}
