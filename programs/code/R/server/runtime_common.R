# runtime_common.R
# Shared server-side runtime helpers used by standard and bootstrap entrypoints.

local({
  candidates <- c("/project/R/common/utils.R", "R/common/utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})
local({
  candidates <- c("/project/R/common/constants.R", "R/common/constants.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/constants.R not found")
  source(hit[[1L]], chdir = TRUE)
})

runtime_parse_cli <- function(args,
                              default_config,
                              default_seed = NULL,
                              allow_seed = FALSE) {
  cfg_path <- default_config
  seed_val <- default_seed
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--config") || identical(arg, "-c")) {
      if (i == length(args)) stop("--config requires a path argument")
      cfg_path <- args[[i + 1L]]
      i <- i + 2L
      next
    }
    if (startsWith(arg, "--config=")) {
      cfg_path <- sub("^--config=", "", arg, perl = TRUE)
      i <- i + 1L
      next
    }
    if (isTRUE(allow_seed) && (identical(arg, "--seed") || identical(arg, "-s"))) {
      if (i == length(args)) stop("--seed requires a numeric argument")
      seed_val <- as.integer(args[[i + 1L]])
      i <- i + 2L
      next
    }
    if (isTRUE(allow_seed) && startsWith(arg, "--seed=")) {
      seed_val <- as.integer(sub("^--seed=", "", arg, perl = TRUE))
      i <- i + 1L
      next
    }
    if (identical(cfg_path, default_config)) {
      cfg_path <- arg
    }
    i <- i + 1L
  }
  list(config = cfg_path, seed = seed_val)
}

runtime_prepare_cfg <- function(cfg_raw) {
  # Preferred input is the normalized output from server_config().
  param_spec <- cfg_raw$param_spec
  if (is.null(param_spec)) stop("param_spec is required (pass normalized server_config output)")

  out <- list(
    clients = cfg_raw$client_map %||% cfg_raw$clients,
    modelInfo = cfg_raw$model_info %||% cfg_raw$modelInfo %||% list(),
    param_spec = param_spec,
    param_spec_payload = cfg_raw$param_spec_payload,
    init_par = cfg_raw$init_par %||% cfg_raw$initPar %||% param_spec$init,
    lower = cfg_raw$lower %||% param_spec$lower,
    upper = cfg_raw$upper %||% param_spec$upper,
    objective = cfg_raw$objective %||% list(),
    optim_control = (cfg_raw$optim$control %||% cfg_raw$optimControl %||% list()),
    comm = cfg_raw$comm %||% list(),
    logging = cfg_raw$logging %||% list(),
    bootstrap = cfg_raw$bootstrap %||% list(),
    config_hash_resolved = cfg_raw$config_hash_resolved %||% NA_character_
  )

  if (is.null(out$clients) || length(out$clients) == 0L) {
    stop("config must supply client mappings")
  }

  nms <- out$param_spec$names
  out$init_par <- out$init_par[nms]
  out$lower <- out$lower[nms]
  out$upper <- out$upper[nms]
  out
}

runtime_make_run <- function(logging,
                             default_base,
                             default_scenario) {
  run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
  base_dir <- logging$base_dir %||% default_base
  scenario <- logging$scenario %||% default_scenario
  run_dir <- file.path(base_dir, scenario, paste0("run_", run_id))
  ok <- dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(ok) && !dir.exists(run_dir)) {
    stop(sprintf("failed to create run directory: %s", run_dir))
  }
  list(run_id = run_id, run_dir = run_dir, base_dir = base_dir, scenario = scenario)
}

runtime_detect_grad_cores <- function(objective, env_name = "GRAD_CORES", default = 1L) {
  gc <- suppressWarnings(as.integer(objective$grad_cores %||% Sys.getenv(env_name, as.character(default))))
  if (!length(gc) || is.na(gc) || !is.finite(gc) || gc < 1L) return(as.integer(default))
  as.integer(gc)
}

runtime_send_init <- function(cfg, grad_cores = 1L, bootstrap = NULL) {
  comm <- cfg$comm %||% list()
  send_init(
    client_map = cfg$clients,
    modelInfo = cfg$modelInfo,
    initPar = as.list(cfg$init_par),
    lower = as.list(cfg$lower),
    paramSpec = cfg$param_spec_payload,
    omegaBlocks = cfg$param_spec$omega_blocks %||% NULL,
    bootstrap = bootstrap,
    grad_cores = as.integer(grad_cores),
    max_tries = comm$init_max_tries %||% fedpoppk_const("comm.init_max_tries", 20L),
    pause = comm$init_pause %||% fedpoppk_const("comm.init_pause", 5),
    timeout = comm$init_timeout %||% fedpoppk_const("comm.init_timeout", 300),
    use_cor = isTRUE(cfg$objective$use_cor),
    eps_pos = as.numeric(cfg$objective$eps_pos %||% fedpoppk_const("objective.eps_pos", 1e-6))
  )
}

runtime_require_named_numeric <- function(x, required_names, label = "value") {
  if (is.null(x)) stop(label, " must not be NULL")
  if (is.list(x) && !is.numeric(x)) x <- unlist(x, use.names = TRUE)
  storage.mode(x) <- "double"
  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    if (length(x) != length(required_names)) {
      stop(label, " must supply names: ", paste(required_names, collapse = ", "))
    }
    names(x) <- required_names
  }
  missing <- setdiff(required_names, names(x))
  if (length(missing)) stop(label, " missing fields: ", paste(missing, collapse = ", "))
  x[required_names]
}

runtime_strip_nonoptim_fields <- function(control) {
  ctrl <- control %||% list()
  ctrl$return_grad <- NULL
  ctrl$timeout <- NULL
  ctrl$max_tries <- NULL
  ctrl$pause <- NULL
  ctrl$method <- NULL
  ctrl$maxls <- NULL
  ctrl$stepmax <- NULL
  ctrl
}

runtime_read_int_env <- function(name, default, min_value = 1L) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) return(as.integer(default))
  out <- suppressWarnings(as.integer(raw))
  if (is.na(out) || !is.finite(out) || out < min_value) return(as.integer(default))
  as.integer(out)
}

runtime_git_meta <- function() {
  git_sha <- NA_character_
  git_dirty <- NA
  sha <- tryCatch(suppressWarnings(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)
  ),
    error = function(e) character()
  )
  if (length(sha)) git_sha <- trimws(sha[[1L]])

  dirty_status <- tryCatch(suppressWarnings(
    system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE)
  ),
    error = function(e) character()
  )
  if (length(dirty_status) || (length(dirty_status) == 0L && !all(is.na(git_sha)))) {
    git_dirty <- length(dirty_status) > 0L
  }
  list(git_sha = git_sha, git_dirty = git_dirty)
}

runtime_schema_meta <- function(config_path = NULL,
                                seed = NULL,
                                config_hash = NA_character_) {
  cfg_path <- config_path %||% ""
  cfg_hash <- NA_character_
  if (nzchar(cfg_path) && file.exists(cfg_path)) {
    cfg_hash <- unname(tools::md5sum(cfg_path)[[1L]])
  }
  if (is.character(config_hash) && nzchar(config_hash[[1L]])) {
    cfg_hash <- config_hash[[1L]]
  }
  git_meta <- runtime_git_meta()
  list(
    schema_version = fedpoppk_const("schema_version", "1.1.0"),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    config_path = if (nzchar(cfg_path)) cfg_path else NA_character_,
    config_md5 = cfg_hash,
    seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
    host = Sys.info()[["nodename"]] %||% NA_character_,
    pid = Sys.getpid(),
    image_tag = Sys.getenv("FEDPOPPK_IMAGE_TAG", unset = NA_character_),
    git_sha = git_meta$git_sha,
    git_dirty = git_meta$git_dirty
  )
}
