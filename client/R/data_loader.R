# R/data_loader.R
library(data.table)
# (必要なら library(arrow); library(qs) も)

#' Load client data (CSV, RDS, Parquet, etc.) into a data.table
#'
#' @param cfg A list with:
#'   - cfg$dataPath: character, full path to the data file
#' @return A data.table
#' @export
load_data <- function(cfg) {
  if (is.null(cfg$dataPath) || !nzchar(cfg$dataPath)) {
    stop("cfg$dataPath must be a non-empty string")
  }
  if (!file.exists(cfg$dataPath)) {
    stop("Data file not found: ", cfg$dataPath)
  }
  ext <- tolower(tools::file_ext(cfg$dataPath))
  dt <- switch(ext,
    csv     = fread(cfg$dataPath),
    rds     = as.data.table(readRDS(cfg$dataPath)),
    parquet = as.data.table(arrow::read_parquet(cfg$dataPath)),
    qsm     = as.data.table(qs::qread(cfg$dataPath)),
    stop("Unsupported file extension: ", ext)
  )
  return(dt)
}
