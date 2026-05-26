# utils.R
# Shared tiny runtime helpers.

`%||%` <- function(a, b) if (is.null(a)) b else a
