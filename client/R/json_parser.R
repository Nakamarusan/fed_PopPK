# json_parser_basic.R
library(jsonlite)

#' @param json_path Path to JSON file
#' @return List of modelInfo
parse_model_json_basic <- function(json_path) {
  if (!file.exists(json_path)) {
    stop("JSON file not found: ", json_path)
  }
  # Wrap in try() to catch malformed JSON
  data <- try(fromJSON(json_path), silent = TRUE)
  if (inherits(data, "try-error")) {
    stop("Failed to parse JSON: ", attr(data, "condition")$message)
  }
  model_info <- data$modelInfo
  if (is.null(model_info)) {
    stop("Missing 'modelInfo' element in JSON.")
  }
  return(model_info)
}