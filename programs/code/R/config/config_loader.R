# config_loader.R
# Shared JSON config loader with recursive `extends` support.

suppressPackageStartupMessages({
  library(jsonlite)
})

local({
  candidates <- c(
    "/project/programs/code/R/common/utils.R",
    "/project/R/common/utils.R",
    "programs/code/R/common/utils.R",
    "R/common/utils.R"
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) {
    source(hit[[1L]], chdir = TRUE)
  } else {
    `%||%` <<- function(a, b) if (is.null(a)) b else a
  }
})

.cfg_is_named_list <- function(x) is.list(x) && !is.null(names(x))

.cfg_deep_merge <- function(base, override) {
  if (is.null(base)) return(override)
  if (is.null(override)) return(base)
  if (!.cfg_is_named_list(base) || !.cfg_is_named_list(override)) return(override)

  out <- base
  for (nm in names(override)) {
    if (nm %in% names(out)) {
      out[[nm]] <- .cfg_deep_merge(out[[nm]], override[[nm]])
    } else {
      out[nm] <- list(override[[nm]])
    }
  }
  out
}

.cfg_resolve_ext_path <- function(path, parent_dir) {
  if (grepl("^/", path)) return(path)
  normalizePath(file.path(parent_dir, path), winslash = "/", mustWork = FALSE)
}

cfg_load_json_with_extends <- function(json_path, seen = character()) {
  json_path <- normalizePath(json_path, winslash = "/", mustWork = FALSE)
  if (json_path %in% seen) {
    stop("circular config extends detected: ", paste(c(seen, json_path), collapse = " -> "))
  }
  if (!file.exists(json_path)) stop("config file not found: ", json_path)

  cfg <- fromJSON(json_path, simplifyVector = FALSE)
  ext <- cfg$extends
  cfg$extends <- NULL
  if (is.null(ext)) return(cfg)

  parents <- if (is.list(ext)) unlist(ext, use.names = FALSE) else ext
  parents <- as.character(parents)
  if (!length(parents)) return(cfg)

  merged <- list()
  parent_dir <- dirname(json_path)
  for (p in parents) {
    pth <- .cfg_resolve_ext_path(p, parent_dir)
    parent_cfg <- cfg_load_json_with_extends(pth, seen = c(seen, json_path))
    merged <- .cfg_deep_merge(merged, parent_cfg)
  }

  .cfg_deep_merge(merged, cfg)
}
