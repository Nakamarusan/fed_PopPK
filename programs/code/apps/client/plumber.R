# plumber.R
# Unified client API for standard and bootstrap modes.

suppressPackageStartupMessages({
  library(plumber)
  library(data.table)
  library(jsonlite)
})

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

fedpoppk_source("R/client/api_helpers.R")
fedpoppk_source("R/client/api_contract.R")
fedpoppk_source("R/client/bootstrap_sampling.R")
fedpoppk_source("R/model/param_spec.R")
fedpoppk_source("R/model/construct_model_from_JSON.R")
fedpoppk_source("R/model/data_loader.R")
fedpoppk_source("R/objective/compute_obj_grad.R")
fedpoppk_source("R/common/constants.R")

.OBJF_PENALTY <- fedpoppk_const("penalty_objf", 1e15)
.save_boot_data <- tolower(Sys.getenv("SAVE_BOOT_DATA", "true")) %in% c("1", "true", "yes")
.client_tag <- local({
  hn <- Sys.getenv("HOSTNAME", unset = "")
  if (nzchar(hn)) hn else paste0("client_", as.integer(Sys.getpid()))
})

.global_state <- new.env(parent = emptyenv())
.global_state$model_ui <- NULL
.global_state$dt <- NULL
.global_state$base_dt <- NULL
.global_state$use_cor <- FALSE
.global_state$eps_pos <- fedpoppk_const("objective.eps_pos", 1e-6)
.global_state$lower <- NULL
.global_state$param_spec <- NULL
.global_state$grad_cores <- 1L
.global_state$cache_dir <- "/project/cache"
.global_state$base_data_path <- NULL
.global_state$boot_samples <- list()
.global_state$boot_cache <- list(
  trial_id = NA_integer_,
  dt = NULL,
  spec = NULL,
  sample_path = NULL
)
.global_state$bootstrap <- list(ready = FALSE)
.global_state$run_counter <- 0L
.global_state$gc_every <- {
  raw <- suppressWarnings(as.integer(Sys.getenv("CLIENT_GC_EVERY", "20")))
  if (!length(raw) || is.na(raw) || raw < 1L) 20L else raw
}
.global_state$client_mode <- {
  mode <- tolower(trimws(Sys.getenv("CLIENT_MODE", "standard")))
  if (mode %in% c("standard", "bootstrap")) mode else "standard"
}

.maybe_gc <- function() {
  .global_state$run_counter <- as.integer(.global_state$run_counter %||% 0L) + 1L
  gc_every <- as.integer(.global_state$gc_every %||% 20L)
  if (gc_every < 1L) gc_every <- 20L
  if ((.global_state$run_counter %% gc_every) == 0L) {
    invisible(gc(verbose = FALSE, full = TRUE))
  }
}

.safe_dir <- function(p) {
  tryCatch({
    if (dir.exists(p)) return(TRUE)
    ok <- dir.create(p, showWarnings = FALSE, recursive = TRUE)
    if (!isTRUE(ok) && !dir.exists(p)) {
      message("[cache] failed to create directory: ", p)
      return(FALSE)
    }
    TRUE
  }, error = function(e) {
    message("[cache] failed to create directory: ", p, " (", conditionMessage(e), ")")
    FALSE
  })
}

.safe_write_json <- function(path, obj) {
  tryCatch({
    write_json(obj, path, auto_unbox = TRUE, digits = 10, pretty = TRUE)
    TRUE
  }, error = function(e) {
    message("[cache] write_json failed: ", conditionMessage(e))
    FALSE
  })
}

.safe_save_rds <- function(object, file) {
  tryCatch({
    saveRDS(object, file = file, compress = TRUE)
    TRUE
  }, error = function(e) {
    message("[cache] saveRDS failed: ", conditionMessage(e))
    FALSE
  })
}

.read_data <- function(path) {
  as.data.frame(load_data(path))
}

.resolve_grad_cores <- function(payload) {
  grad_cores_env <- Sys.getenv("GRAD_CORES", "")
  raw <- if (nzchar(grad_cores_env)) {
    grad_cores_env
  } else {
    payload$gradCores %||% payload$grad_cores %||% "1"
  }
  gc <- suppressWarnings(as.integer(raw))
  if (!length(gc) || is.na(gc) || !is.finite(gc) || gc < 1L) 1L else gc
}

.bootstrap_prepare <- function(payload, dp) {
  boot_cfg <- payload$bootstrap %||% list()
  B <- as.integer(boot_cfg$B_per_client %||% boot_cfg$B %||% boot_cfg$trials %||%
                    fedpoppk_const("bootstrap.B_per_client", 500L))
  if (!is.finite(B) || B <= 0L) stop("bootstrap B must be positive")
  seed_base <- as.integer(boot_cfg$seed_base %||% fedpoppk_const("bootstrap.seed_base", 4869L))
  if (!is.finite(seed_base)) stop("bootstrap seed_base must be finite")

  .global_state$bootstrap <- list(B = B, seed_base = seed_base, ready = FALSE)
  .global_state$bootstrap$pre_generated_root <- boot_cfg$pre_generated_root %||% NULL
  .global_state$cache_dir <- Sys.getenv("CACHE_DIR", "/project/cache")
  if (!.safe_dir(.global_state$cache_dir)) {
    stop("cache_dir is not writable or cannot be created: ", .global_state$cache_dir)
  }

  boot_root <- file.path(.global_state$cache_dir, "boot_samples")
  if (!.safe_dir(boot_root)) {
    stop("boot_root is not writable or cannot be created: ", boot_root)
  }
  # Reset per-trial runtime cache; trials are generated lazily on demand.
  .global_state$boot_cache <- list(
    trial_id = NA_integer_,
    dt = NULL,
    spec = NULL,
    sample_path = NULL
  )
  .global_state$boot_samples <- list()
  .global_state$bootstrap$ready <- TRUE

  .safe_write_json(
    file.path(.global_state$cache_dir, "init_meta.json"),
    list(
      ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      dataPath = dp,
      nrow = nrow(.global_state$base_dt),
      ncol = ncol(.global_state$base_dt),
      client = .client_tag,
      grad_cores = .global_state$grad_cores,
      bootstrap = list(B = B, seed_base = seed_base)
    )
  )

  list(B = B, seed_base = seed_base)
}

.bootstrap_resolve_data <- function(payload) {
  rs <- payload$resample %||% list()
  mode_boot <- is.list(rs) && is.character(rs$mode) && tolower(rs$mode) == "bootstrap"
  if (!mode_boot) stop("bootstrap mode requires resample.mode='bootstrap'")
  if (!isTRUE(.global_state$bootstrap$ready)) stop("bootstrap samples not prepared")

  trial_id <- as.integer(rs$trial_id %||% 1L)
  if (!is.finite(trial_id) || trial_id < 1L) stop("invalid bootstrap trial_id")

  # Reuse same in-memory sample within the same trial to avoid repeated rebuilds.
  cache <- .global_state$boot_cache %||% list()
  if (isTRUE(cache$trial_id == trial_id) && is.data.frame(cache$dt)) {
    return(list(
      dt = cache$dt,
      trial_id = trial_id,
      seed_base = as.integer(rs$seed_base %||% (.global_state$bootstrap$seed_base %||% 4869L)),
      stamp = if (!is.null(rs$stamp) && nzchar(rs$stamp)) rs$stamp else format(Sys.time(), "%Y%m%d-%H%M%S"),
      sample_path = cache$sample_path,
      spec = cache$spec
    ))
  }

  boot_root <- file.path(.global_state$cache_dir %||% "/project/cache", "boot_samples")
  if (!.safe_dir(boot_root)) stop("failed to create bootstrap cache root: ", boot_root)
  trial_dir <- file.path(boot_root, sprintf("trial_%04d", trial_id))
  if (!.safe_dir(trial_dir)) stop("failed to create trial cache directory: ", trial_dir)

  pre_root <- .global_state$bootstrap$pre_generated_root %||% NULL
  base_path <- .global_state$base_data_path %||% ""
  base_stem <- if (nzchar(base_path)) tools::file_path_sans_ext(basename(base_path)) else ""
  pre_trial_dir <- if (!is.null(pre_root) && nzchar(pre_root)) file.path(pre_root, sprintf("trial_%04d", trial_id)) else NULL
  pre_data_rds <- if (!is.null(pre_trial_dir) && nzchar(base_stem)) file.path(pre_trial_dir, sprintf("%s.rds", base_stem)) else NULL
  pre_data_csv <- if (!is.null(pre_trial_dir) && nzchar(base_stem)) file.path(pre_trial_dir, sprintf("%s.csv", base_stem)) else NULL
  pre_spec_path <- if (!is.null(pre_trial_dir) && nzchar(base_stem)) file.path(pre_trial_dir, sprintf("%s_spec.json", base_stem)) else NULL

  data_path <- file.path(trial_dir, "data_boot.rds")
  spec_path <- file.path(trial_dir, "spec.json")

  dt_use <- NULL
  sample_path <- NULL
  boot_spec <- NULL

  if (!is.null(pre_data_rds) && file.exists(pre_data_rds)) {
    sample_path <- pre_data_rds
    dt_use <- .read_data(pre_data_rds)
    if (!is.null(pre_spec_path) && file.exists(pre_spec_path)) {
      boot_spec <- tryCatch(read_json(pre_spec_path, simplifyVector = FALSE), error = function(e) NULL)
    }
  } else if (!is.null(pre_data_csv) && file.exists(pre_data_csv)) {
    sample_path <- pre_data_csv
    dt_use <- .read_data(pre_data_csv)
    if (!is.null(pre_spec_path) && file.exists(pre_spec_path)) {
      boot_spec <- tryCatch(read_json(pre_spec_path, simplifyVector = FALSE), error = function(e) NULL)
    }
  } else if (!is.null(pre_spec_path) && file.exists(pre_spec_path)) {
    boot_spec <- tryCatch(read_json(pre_spec_path, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(boot_spec)) {
      dt_use <- bootstrap_reconstruct(.global_state$base_dt, boot_spec)
    }
  } else if (.save_boot_data && file.exists(data_path)) {
    sample_path <- data_path
    dt_use <- readRDS(sample_path)
  } else {
    if (file.exists(spec_path)) {
      boot_spec <- tryCatch(read_json(spec_path, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(boot_spec)) {
        dt_use <- bootstrap_reconstruct(.global_state$base_dt, boot_spec)
      }
    }
    if (is.null(dt_use)) {
      facility_col <- bootstrap_detect_facility_column(.global_state$base_dt)
      set.seed(as.integer(.global_state$bootstrap$seed_base %||% 4869L) + trial_id)
      boot_info <- bootstrap_build_spec(.global_state$base_dt, facility_col = facility_col)
      dt_use <- boot_info$data
      boot_spec <- boot_info$spec
    }
    if (!is.null(boot_spec)) {
      .safe_write_json(spec_path, boot_spec)
    }
    if (.save_boot_data) {
      .safe_save_rds(dt_use, data_path)
      if (file.exists(data_path)) sample_path <- data_path
    }
  }

  .global_state$boot_cache <- list(
    trial_id = trial_id,
    dt = dt_use,
    spec = boot_spec,
    sample_path = sample_path
  )

  list(
    dt = dt_use,
    trial_id = trial_id,
    seed_base = as.integer(rs$seed_base %||% (.global_state$bootstrap$seed_base %||% 4869L)),
    stamp = if (!is.null(rs$stamp) && nzchar(rs$stamp)) rs$stamp else format(Sys.time(), "%Y%m%d-%H%M%S"),
    sample_path = sample_path,
    spec = boot_spec
  )
}

.bootstrap_log_eval <- function(payload, dt_use, calc_out, calc_err, started_at, ended_at, resolved_boot) {
  cache_root <- .global_state$cache_dir %||% "/project/cache"
  trial_dir <- file.path(
    cache_root,
    sprintf("trial_%03d", resolved_boot$trial_id),
    resolved_boot$stamp,
    .client_tag
  )
  .safe_dir(trial_dir)

  .safe_write_json(
    file.path(trial_dir, "meta.json"),
    list(
      ts_start = format(started_at, "%Y-%m-%d %H:%M:%S"),
      ts_end = format(ended_at, "%Y-%m-%d %H:%M:%S"),
      elapsed = as.numeric(difftime(ended_at, started_at, units = "secs")),
      client = .client_tag,
      resample = list(
        mode = "bootstrap",
        seed_base = resolved_boot$seed_base,
        trial_id = resolved_boot$trial_id,
        stamp = resolved_boot$stamp,
        sample_path = resolved_boot$sample_path
      )
    )
  )
  .safe_write_json(file.path(trial_dir, "params.json"), as.list(payload$p))
  if (!is.null(resolved_boot$spec)) {
    .safe_write_json(file.path(trial_dir, "bootstrap_spec.json"), resolved_boot$spec)
  }
  if (.save_boot_data && is.null(resolved_boot$sample_path)) {
    .safe_save_rds(dt_use, file.path(trial_dir, "data_boot.rds"))
  }
  if (is.null(calc_out)) {
    .safe_write_json(file.path(trial_dir, "result.json"), list(ok = FALSE, error = calc_err %||% "unknown"))
    return(invisible(NULL))
  }
  resp <- .format_obj_grad(calc_out, objf_penalty = .OBJF_PENALTY, include_ok = TRUE)
  if (!isTRUE(payload$return_grad %||% TRUE)) resp$grad <- NULL
  .safe_write_json(file.path(trial_dir, "result.json"), resp)
}

#* @apiTitle federated-nlmixr2 client

#* @get /healthz
function() {
  list(status = "ok", mode = .global_state$client_mode)
}

#* @get /ping
function() {
  list(status = "ok")
}

#* @post /init
function(req, res) {
  payload <- .parse_json_payload(req, simplify = FALSE)
  if (is.null(payload)) {
    res$status <- 400
    return(list(error = "invalid JSON"))
  }

  err <- tryCatch({
    client_validate_init_payload(payload)
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(err)) {
    res$status <- 400
    return(list(error = err))
  }

  param_spec <- tryCatch(.param_spec_from_payload(payload), error = function(e) {
    res$status <<- 400
    return(list(error = conditionMessage(e)))
  })
  if (is.list(param_spec) && !is.null(param_spec$error)) return(param_spec)
  .global_state$param_spec <- param_spec

  .global_state$grad_cores <- .resolve_grad_cores(payload)
  options(pnd.max.cores = .global_state$grad_cores)

  .global_state$model_ui <- tryCatch(
    construct_model_from_JSON(payload$modelInfo, payload$initPar, param_spec = param_spec),
    error = function(e) {
      res$status <<- 500
      return(list(error = sprintf("failed to build model UI: %s", conditionMessage(e))))
    }
  )
  if (is.list(.global_state$model_ui) && !is.null(.global_state$model_ui$error)) return(.global_state$model_ui)

  dp <- payload$dataPath
  loaded_dt <- if (is.list(dp) || (is.atomic(dp) && length(dp) > 1L)) {
    lapply(dp, function(x) .read_data(as.character(x)))
  } else {
    .read_data(as.character(dp))
  }
  .global_state$base_data_path <- if (is.list(dp) || (is.atomic(dp) && length(dp) > 1L)) "" else as.character(dp)
  .global_state$base_dt <- loaded_dt
  .global_state$dt <- .global_state$base_dt
  .global_state$use_cor <- isTRUE(payload$use_cor %||% FALSE)
  .global_state$eps_pos <- as.numeric(payload$eps_pos %||% fedpoppk_const("objective.eps_pos", 1e-6))

  if (identical(.global_state$client_mode, "bootstrap")) {
    if (!is.data.frame(.global_state$base_dt)) {
      res$status <- 400
      return(list(error = "bootstrap mode requires a single tabular dataPath"))
    }
    boot <- tryCatch(.bootstrap_prepare(payload, dp), error = function(e) {
      res$status <<- 400
      return(list(error = conditionMessage(e)))
    })
    if (is.list(boot) && !is.null(boot$error)) return(boot)
    return(list(
      status = "ready",
      nrow = nrow(.global_state$base_dt),
      dataPath = dp,
      cache_dir = .global_state$cache_dir,
      client = .client_tag,
      grad_cores = .global_state$grad_cores,
      bootstrap = boot
    ))
  }

  list(status = "initialized", grad_cores = .global_state$grad_cores, mode = .global_state$client_mode)
}

#* @post /run
function(req, res) {
  if (is.null(.global_state$model_ui) || is.null(.global_state$base_dt)) {
    res$status <- 409
    return(list(error = "client is not initialized; call /init first"))
  }

  payload <- .parse_json_payload(req, simplify = FALSE)
  if (is.null(payload)) {
    res$status <- 400
    return(list(error = "invalid JSON"))
  }
  err <- tryCatch({
    client_validate_run_payload(payload)
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(err)) {
    res$status <- 400
    return(list(error = err))
  }

  parsed <- .normalize_param_vector(payload$p, .global_state$param_spec$names)
  if (!isTRUE(parsed$ok)) {
    res$status <- 400
    return(list(error = parsed$error))
  }
  p_vec <- parsed$value
  rgrad <- isTRUE(payload$return_grad %||% TRUE)
  r_eps <- payload$richardson_eps %||% fedpoppk_const("objective.richardson_eps", 1e-5)

  base_dt <- .global_state$base_dt
  dt_use <- base_dt
  resolved_boot <- NULL
  if (identical(.global_state$client_mode, "bootstrap")) {
    resolved_boot <- tryCatch(.bootstrap_resolve_data(payload), error = function(e) {
      res$status <<- 400
      return(list(error = conditionMessage(e)))
    })
    if (is.list(resolved_boot) && !is.null(resolved_boot$error)) return(resolved_boot)
    dt_use <- resolved_boot$dt
  }

  .global_state$dt <- dt_use
  on.exit({ .global_state$dt <- base_dt }, add = TRUE)

  started_at <- Sys.time()
  calc_err <- NULL
  ans <- tryCatch({
    compute_obj_grad(
      p_z = p_vec,
      state_env = .global_state,
      return_grad = rgrad,
      richardson_eps = r_eps
    )
  }, error = function(e) {
    calc_err <<- conditionMessage(e)
    NULL
  })
  ended_at <- Sys.time()

  if (identical(.global_state$client_mode, "bootstrap") && !is.null(resolved_boot)) {
    .bootstrap_log_eval(payload, dt_use, ans, calc_err, started_at, ended_at, resolved_boot)
  }

  if (is.null(ans)) {
    res$status <- 500
    .maybe_gc()
    return(list(error = calc_err %||% "compute_obj_grad error"))
  }

  out <- .format_obj_grad(ans, objf_penalty = .OBJF_PENALTY, include_ok = identical(.global_state$client_mode, "bootstrap"))
  if (!rgrad) out$grad <- NULL
  # Expose client-side compute duration for server-side comm/compute decomposition.
  out$compute_sec <- as.numeric(difftime(ended_at, started_at, units = "secs"))
  .maybe_gc()
  out
}
