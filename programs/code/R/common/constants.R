# constants.R
# Shared runtime constants used across federated and analysis workflows.

local({
  candidates <- c("/project/R/common/utils.R", "R/common/utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

fedpoppk_constants <- function() {
  list(
    schema_version = "1.1.0",
    penalty_objf = 1e15,
    objective = list(
      richardson_eps = 1e-5,
      eps_pos = 1e-6
    ),
    comm = list(
      init_max_tries = 20L,
      init_pause = 5,
      init_timeout = 300,
      run_max_tries = 3L,
      run_pause = 1,
      run_timeout = 300
    ),
    comm_boot = list(
      run_max_tries = 3L,
      run_pause = 1,
      run_timeout = 1200
    ),
    optim = list(
      maxit = 100L,
      pgtol = 5e-3,
      factr = 1e8,
      maxls = 5L,
      stepmax = 0.5
    ),
    bootstrap = list(
      B_per_client = 500L,
      seed_base = 4869L
    )
  )
}

fedpoppk_const <- function(path, default = NULL) {
  parts <- strsplit(path, "\\.", fixed = FALSE)[[1L]]
  cur <- fedpoppk_constants()
  for (p in parts) {
    if (!is.list(cur) || is.null(cur[[p]])) return(default)
    cur <- cur[[p]]
  }
  cur
}
