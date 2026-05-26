# source_utils.R
# Shared helpers to resolve project-rooted source paths consistently.

fedpoppk_project_root <- local({
  cached <- NULL

  find_root <- function() {
    env_root <- trimws(Sys.getenv("FEDPOPPK_ROOT", ""))
    if (nzchar(env_root)) {
      p <- normalizePath(env_root, winslash = "/", mustWork = FALSE)
      if (file.exists(file.path(p, "renv.lock"))) return(p)
    }

    if (dir.exists("/project") && file.exists("/project/renv.lock")) {
      return("/project")
    }

    cur <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    repeat {
      if (file.exists(file.path(cur, "renv.lock")) &&
          dir.exists(file.path(cur, "R"))) {
        return(cur)
      }
      parent <- dirname(cur)
      if (identical(parent, cur)) break
      cur <- parent
    }

    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }

  function(refresh = FALSE) {
    if (isTRUE(refresh) || is.null(cached)) {
      cached <<- find_root()
    }
    cached
  }
})

fedpoppk_resolve_path <- function(path) {
  if (grepl("^/", path)) return(path)
  file.path(fedpoppk_project_root(), path)
}

fedpoppk_source <- function(path, chdir = TRUE, envir = parent.frame()) {
  resolved <- fedpoppk_resolve_path(path)
  if (!file.exists(resolved)) {
    stop("source file not found: ", resolved)
  }
  source(resolved, chdir = chdir, local = envir)
  invisible(resolved)
}
