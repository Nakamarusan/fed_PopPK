# server_config.R
# Loads server JSON configuration and normalizes it into a stable runtime shape.
# Backward-compatible parsing is handled here so callers can use one structure.

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

fedpoppk_source("R/config/config_loader.R")
fedpoppk_source("R/config/config_schema.R")
fedpoppk_source("R/common/constants.R")

.scalar_num <- function(x, default) {
  if (is.null(x)) return(default)
  if (is.atomic(x) && length(x) == 1L && is.na(x)) return(default)
  as.numeric(x)[[1L]]
}

.scalar_int <- function(x, default) {
  if (is.null(x)) return(as.integer(default))
  if (is.atomic(x) && length(x) == 1L && is.na(x)) return(as.integer(default))
  val <- suppressWarnings(as.integer(x))
  if (!length(val) || is.na(val[[1L]]) || !is.finite(val[[1L]])) return(as.integer(default))
  as.integer(val[[1L]])
}

.hash_config_object <- function(cfg_obj) {
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf), add = TRUE)
  txt <- jsonlite::toJSON(cfg_obj, auto_unbox = TRUE, pretty = FALSE, null = "null")
  writeLines(txt, tf)
  unname(tools::md5sum(tf)[[1L]])
}

.as_named_char <- function(x) {
  if (is.null(x)) return(character())
  if (is.atomic(x) && is.character(x) && !is.null(names(x))) return(x)
  if (is.list(x) && !is.null(names(x))) {
    v <- vapply(names(x), function(k) as.character(x[[k]] %||% ""), character(1))
    names(v) <- names(x)
    return(v)
  }
  stop("Invalid 'clients' in JSON: must be an object {url: dataPath, ...}")
}

.normalize_init_par <- function(initPar) {
  if (is.null(initPar)) stop("initPar must be provided")
  init <- unlist(initPar, use.names = TRUE)
  storage.mode(init) <- "double"
  if (is.null(names(init)) || any(!nzchar(names(init)))) {
    stop("initPar must be a named numeric vector")
  }
  init
}

.normalize_bounds <- function(bounds, param_names, default_value) {
  out <- setNames(rep(default_value, length(param_names)), param_names)
  if (is.null(bounds)) return(out)
  if (is.list(bounds)) {
    bounds <- unlist(bounds, use.names = TRUE)
    if (!length(bounds)) return(out)
  }
  if (!length(bounds)) return(out)
  if (!is.numeric(bounds) || is.null(names(bounds))) {
    stop("bounds must be a named numeric vector")
  }
  storage.mode(bounds) <- "double"
  shared <- intersect(names(bounds), param_names)
  out[shared] <- bounds[shared]
  out[!is.finite(out)] <- default_value
  out
}

.default_transform <- function(lower, upper, role = NULL) {
  if (is.finite(lower) && is.finite(upper)) return("logit")
  if (identical(role, "eta") || identical(role, "residual")) {
    if (is.finite(lower) && !is.finite(upper)) return("lower_exp")
    if (!is.finite(lower) && !is.finite(upper)) return("exp")
  }
  if (is.finite(lower) && !is.finite(upper)) return("lower_exp")
  "identity"
}

.infer_role <- function(param_name) {
  if (grepl("^eta", param_name)) return("eta")
  if (grepl("Sd$", param_name) || grepl("PropSd|AddSd", param_name)) return("residual")
  "fixed"
}

.normalize_param_list <- function(param_list) {
  if (!is.list(param_list) || !length(param_list)) {
    stop("parameters must be a non-empty list")
  }
  names_vec <- vapply(param_list, function(x) x$name %||% "", character(1))
  if (any(!nzchar(names_vec))) stop("every parameter needs a non-empty name")
  if (any(duplicated(names_vec))) stop("parameter names must be unique")

  init_vals <- vapply(param_list, function(x) .scalar_num(x$init, NA_real_), numeric(1))
  if (any(!is.finite(init_vals))) stop("each parameter must specify a finite init value")

  lower_vals <- vapply(param_list, function(x) .scalar_num(x$lower, -Inf), numeric(1))
  upper_vals <- vapply(param_list, function(x) .scalar_num(x$upper, Inf), numeric(1))

  role_vals <- vapply(param_list, function(x) {
    role <- x$role %||% ""
    role_chr <- as.character(unlist(role, use.names = FALSE))
    if (!length(role_chr)) return(NA_character_)
    role_chr <- role_chr[[1L]]
    if (is.na(role_chr) || !nzchar(role_chr)) return(NA_character_)
    role_chr
  }, character(1))
  for (i in seq_along(role_vals)) {
    if (is.na(role_vals[[i]]) || !nzchar(role_vals[[i]])) role_vals[[i]] <- .infer_role(names_vec[[i]])
  }

  transform_vals <- vapply(param_list, function(x) {
    tr <- x$transform %||% ""
    tr_chr <- as.character(unlist(tr, use.names = FALSE))
    if (!length(tr_chr)) return(NA_character_)
    tr_chr <- tr_chr[[1L]]
    if (is.na(tr_chr) || !nzchar(tr_chr)) return(NA_character_)
    tr_chr
  }, character(1))
  for (i in seq_along(transform_vals)) {
    if (is.na(transform_vals[[i]]) || !nzchar(transform_vals[[i]])) {
      transform_vals[[i]] <- .default_transform(lower_vals[[i]], upper_vals[[i]], role = role_vals[[i]])
    }
  }

  list(
    names = names_vec,
    init = setNames(init_vals, names_vec),
    lower = setNames(lower_vals, names_vec),
    upper = setNames(upper_vals, names_vec),
    transform = setNames(transform_vals, names_vec),
    role = setNames(role_vals, names_vec)
  )
}

.omega_matrix_from_config <- function(x, k, label = "omegaBlocks.init") {
  vals <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (length(vals) != k * k || any(!is.finite(vals))) {
    stop(label, " must be a finite ", k, "x", k, " numeric matrix")
  }
  mat <- matrix(vals, nrow = k, ncol = k, byrow = TRUE)
  if (max(abs(mat - t(mat))) > 1e-10) {
    stop(label, " must be symmetric")
  }
  mat
}

.omega_chol_param_name <- function(block_name, eta_row, eta_col) {
  paste("chol", block_name, eta_row, eta_col, sep = "__")
}

.omega_precision_chol_param_name <- function(block_name, eta_row, eta_col) {
  paste("pchol", block_name, eta_row, eta_col, sep = "__")
}

.omega_block_parameterization <- function(block) {
  raw <- block$parameterization %||%
    block$omegaParameterization %||%
    block$omega_parameterization %||%
    "cov_chol"
  val <- tolower(as.character(unlist(raw, use.names = FALSE))[[1L]])
  val <- gsub("[^a-z0-9]+", "_", val)
  if (val %in% c("cov_chol", "covariance_chol", "covariance_cholesky", "omega_chol")) {
    return("cov_chol")
  }
  if (val %in% c("precision_chol", "precision_cholesky", "omega_inv_chol", "inverse_chol", "inv_chol")) {
    return("precision_chol")
  }
  stop("Unsupported omegaBlocks parameterization: ", val)
}

.omega_cov_name <- function(eta1, eta2) {
  sprintf("(%s,%s)", eta1, eta2)
}

.drop_param_names <- function(spec, drop_names) {
  drop_names <- unique(drop_names[nzchar(drop_names)])
  if (!length(drop_names)) return(spec)
  keep <- setdiff(spec$names, drop_names)
  spec$names <- keep
  for (field in c("init", "lower", "upper", "transform", "role")) {
    if (!is.null(spec[[field]])) spec[[field]] <- spec[[field]][keep]
  }
  spec
}

.append_param <- function(spec, name, init, lower, upper, transform, role) {
  if (name %in% spec$names) stop("duplicated parameter name after omega expansion: ", name)
  spec$names <- c(spec$names, name)
  spec$init <- c(spec$init, setNames(as.numeric(init), name))
  spec$lower <- c(spec$lower, setNames(as.numeric(lower), name))
  spec$upper <- c(spec$upper, setNames(as.numeric(upper), name))
  spec$transform <- c(spec$transform, setNames(as.character(transform), name))
  spec$role <- c(spec$role, setNames(as.character(role), name))
  spec
}

.append_omega_blocks <- function(spec, omega_blocks) {
  if (is.null(omega_blocks) || !length(omega_blocks)) return(spec)
  if (!is.list(omega_blocks)) stop("omegaBlocks must be a list")

  normalized_blocks <- list()
  for (b in seq_along(omega_blocks)) {
    block <- omega_blocks[[b]]
    etas <- as.character(unlist(block$etas %||% character(), use.names = FALSE))
    if (length(etas) < 1L || any(!nzchar(etas))) {
      stop("omegaBlocks[[", b, "]] must define non-empty etas")
    }
    if (any(duplicated(etas))) {
      stop("omegaBlocks[[", b, "]] has duplicated etas")
    }
    block_name <- as.character(unlist(block$name %||% paste(etas, collapse = "_"), use.names = FALSE))[[1L]]
    block_name <- gsub("[^A-Za-z0-9_]+", "_", block_name)
    if (!nzchar(block_name)) block_name <- paste(etas, collapse = "_")
    parameterization <- .omega_block_parameterization(block)

    omega_init <- .omega_matrix_from_config(block$init %||% block$omega %||% NULL,
                                            length(etas),
                                            label = paste0("omegaBlocks[[", b, "]].init"))
    factor_try <- tryCatch({
      if (identical(parameterization, "precision_chol")) {
        chol(solve(omega_init))
      } else {
        t(chol(omega_init))
      }
    }, error = function(e) {
      stop("omegaBlocks[[", b, "]].init must be positive definite: ", conditionMessage(e))
    })

    output_names <- etas
    if (length(etas) >= 2L) {
      for (i in 2:length(etas)) {
        for (j in seq_len(i - 1L)) {
          output_names <- c(output_names, .omega_cov_name(etas[[j]], etas[[i]]))
        }
      }
    }
    spec <- .drop_param_names(spec, output_names)

    k <- length(etas)
    param_names <- matrix("", nrow = k, ncol = k)
    if (identical(parameterization, "precision_chol")) {
      for (i in seq_len(k)) {
        for (j in i:k) {
          nm <- .omega_precision_chol_param_name(block_name, etas[[i]], etas[[j]])
          param_names[i, j] <- nm
          spec <- .append_param(
            spec = spec,
            name = nm,
            init = if (i == j) sqrt(factor_try[i, j]) else factor_try[i, j],
            lower = -Inf,
            upper = Inf,
            transform = "identity",
            role = if (i == j) "omega_precision_chol_diag_sqrt" else "omega_precision_chol_offdiag"
          )
        }
      }
    } else {
      for (i in seq_len(k)) {
        for (j in seq_len(i)) {
          nm <- .omega_chol_param_name(block_name, etas[[i]], etas[[j]])
          param_names[i, j] <- nm
          spec <- .append_param(
            spec = spec,
            name = nm,
            init = factor_try[i, j],
            lower = -Inf,
            upper = Inf,
            transform = if (i == j) "exp" else "identity",
            role = if (i == j) "omega_chol_diag" else "omega_chol_offdiag"
          )
        }
      }
    }

    normalized_blocks[[length(normalized_blocks) + 1L]] <- list(
      name = block_name,
      etas = etas,
      init = omega_init,
      parameterization = parameterization,
      param_names = lapply(seq_len(k), function(i) unname(param_names[i, ]))
    )
  }
  spec$omega_blocks <- normalized_blocks
  spec
}

.build_param_spec <- function(cfg) {
  param_list <- cfg$parameters %||% cfg$paramSpec %||% NULL
  if (!is.null(param_list)) {
    spec <- .normalize_param_list(param_list)
    return(.append_omega_blocks(spec, cfg$omegaBlocks %||% cfg$omega_blocks %||% NULL))
  }

  init_par <- cfg$initPar %||% cfg$init_par
  init_vec <- .normalize_init_par(init_par)
  param_names <- names(init_vec)

  lower_cfg <- cfg$lower %||% NULL
  upper_cfg <- cfg$upper %||% NULL
  lower_vec <- .normalize_bounds(lower_cfg, param_names, -Inf)
  upper_vec <- .normalize_bounds(upper_cfg, param_names, Inf)

  param_list <- lapply(param_names, function(nm) {
    list(
      name = nm,
      init = init_vec[[nm]],
      lower = lower_vec[[nm]],
      upper = upper_vec[[nm]],
      transform = .default_transform(lower_vec[[nm]], upper_vec[[nm]], role = .infer_role(nm)),
      role = .infer_role(nm)
    )
  })
  spec <- .normalize_param_list(param_list)
  .append_omega_blocks(spec, cfg$omegaBlocks %||% cfg$omega_blocks %||% NULL)
}

.param_spec_to_payload <- function(param_spec) {
  lapply(param_spec$names, function(nm) {
    lower_val <- param_spec$lower[[nm]]
    upper_val <- param_spec$upper[[nm]]
    transform_val <- param_spec$transform[[nm]]
    role_val <- param_spec$role[[nm]]
    out <- list(
      name = nm,
      init = param_spec$init[[nm]],
      lower = if (is.finite(lower_val)) lower_val else NULL,
      upper = if (is.finite(upper_val)) upper_val else NULL,
      transform = if (!is.null(transform_val) && !is.na(transform_val) && nzchar(transform_val)) transform_val else NULL,
      role = if (!is.null(role_val) && !is.na(role_val) && nzchar(role_val)) role_val else NULL
    )
    out
  })
}

.build_objective <- function(modelInfo, optimControl) {
  grad_cores <- .scalar_int(optimControl$grad_cores %||% Sys.getenv("GRAD_CORES", "1"), 1L)
  if (!is.finite(grad_cores) || grad_cores < 1L) grad_cores <- 1L
  list(
    return_grad    = isTRUE(optimControl$return_grad %||% TRUE),
    richardson_eps = fedpoppk_const("objective.richardson_eps", 1e-5),
    grad_cores     = as.integer(grad_cores),             # client-side finite-difference cores
    use_cor        = isTRUE((modelInfo$iiv %||% list())$cor %||% FALSE),
    eps_pos        = fedpoppk_const("objective.eps_pos", 1e-6)
  )
}

.build_optim <- function(optimControl) {
  method <- as.character(optimControl$method %||% "L-BFGS-B")
  maxit  <- as.integer(optimControl$maxit %||% fedpoppk_const("optim.maxit", 100L))
  pgtol  <- as.numeric(optimControl$pgtol %||% fedpoppk_const("optim.pgtol", 5e-3))
  factr  <- as.numeric(optimControl$factr %||% fedpoppk_const("optim.factr", 1e8))
  trace  <- as.integer(optimControl$trace %||% 0L)
  maxls  <- as.integer(optimControl$maxls %||% fedpoppk_const("optim.maxls", 5L))
  stepmax <- as.numeric(optimControl$stepmax %||% fedpoppk_const("optim.stepmax", 0.5))

  ctrl <- list(
    maxit = maxit,
    pgtol = pgtol,
    factr = factr,
    trace = trace,
    maxls = maxls,
    stepmax = stepmax
  )
  list(
    method  = method,
    control = ctrl
  )
}

.build_bootstrap <- function(bootstrap_cfg) {
  list(
    B_per_client = as.integer(bootstrap_cfg$B_per_client %||% fedpoppk_const("bootstrap.B_per_client", 500L)),
    seed_base    = as.integer(bootstrap_cfg$seed_base %||% fedpoppk_const("bootstrap.seed_base", 4869L)),
    pre_generated_root = bootstrap_cfg$pre_generated_root %||% NULL
  )
}

.resolve_logging_scenario <- function(logging_scenario, default = "scenario1") {
  env_scenario <- trimws(Sys.getenv("SCENARIO", ""))
  raw <- as.character(logging_scenario %||% "")
  raw <- if (length(raw)) raw[[1L]] else ""
  raw <- trimws(raw)

  if (!nzchar(raw)) {
    return(if (nzchar(env_scenario)) env_scenario else default)
  }

  if (grepl("\\{SCENARIO\\}", raw)) {
    resolved <- if (nzchar(env_scenario)) env_scenario else default
    return(gsub("\\{SCENARIO\\}", resolved, raw))
  }

  # Keep backward compatibility for static labels while segregating by SCENARIO
  # when that environment variable is explicitly supplied by Compose.
  if (nzchar(env_scenario)) {
    norm_raw <- sub("^/+", "", sub("/+$", "", raw))
    norm_env <- sub("^/+", "", sub("/+$", "", env_scenario))
    if (!(identical(norm_raw, norm_env) || startsWith(norm_raw, paste0(norm_env, "/")))) {
      return(file.path(norm_env, norm_raw))
    }
    return(norm_raw)
  }

  raw
}

.default_config <- function() {
  model_info <- list(
    modelName      = "PK_1cmt",
    administration = "iv",
    iiv            = list(lcl = TRUE, lvc = TRUE, cor = FALSE),
    res            = "mix"
  )
  list(
    client_map = c(
      "http://client1:8000" = "/project/data/data1_1.csv",
      "http://client2:8000" = "/project/data/data1_2.csv",
      "http://client3:8000" = "/project/data/data1_3.csv"
    ),
    model_info = model_info,
    init_par   = c(
      lcl = log(0.693), lvc = log(1.0),
      etaLcl = 0.03922071, etaLvc = 0.03922071,
      CcPropSd = 0.1, CcAddSd = 0.1
    ),
    lower     = c(
      lcl = -Inf,
      lvc = -Inf,
      etaLcl = 1e-4,
      etaLvc = 1e-4,
      CcPropSd = 1e-3,
      CcAddSd = 1e-3
    ),
    upper     = c(
      lcl = Inf,
      lvc = Inf,
      etaLcl = Inf,
      etaLvc = Inf,
      CcPropSd = Inf,
      CcAddSd = Inf
    ),
    objective  = .build_objective(model_info, list(return_grad = TRUE)),
    optim      = .build_optim(list(
      method = "L-BFGS-B",
      maxit = fedpoppk_const("optim.maxit", 100L),
      pgtol = fedpoppk_const("optim.pgtol", 5e-3),
      factr = fedpoppk_const("optim.factr", 1e8),
      maxls = fedpoppk_const("optim.maxls", 5L),
      stepmax = fedpoppk_const("optim.stepmax", 0.5)
    )),
    comm       = list(
      init_max_tries = fedpoppk_const("comm.init_max_tries", 20L),
      init_pause = fedpoppk_const("comm.init_pause", 5),
      init_timeout = fedpoppk_const("comm.init_timeout", 300),
      run_max_tries = fedpoppk_const("comm.run_max_tries", 3L),
      run_pause = fedpoppk_const("comm.run_pause", 1),
      run_timeout = fedpoppk_const("comm.run_timeout", 300)
    ),
    logging    = list(
      base_dir = "/project/result/logs",
      scenario = "scenario1",
      save_par_log = TRUE
    ),
    bootstrap = .build_bootstrap(list())
  )
}

server_config <- function(json_path = "/project/configs/standard.json") {
  if (!file.exists(json_path)) {
    message("[server_config] config json not found - using built-in defaults")
    cfg_default <- .default_config()
    param_spec <- .build_param_spec(cfg_default)
    cfg_default$param_spec <- param_spec
    cfg_default$param_spec_payload <- .param_spec_to_payload(param_spec)
    cfg_default$config_hash_resolved <- .hash_config_object(cfg_default)
    return(cfg_default)
  }

  cfg <- cfg_load_json_with_extends(json_path)
  fedpoppk_validate_config(cfg, kind = "auto", path = json_path)
  cfg_hash_resolved <- .hash_config_object(cfg)

  # ---- clients
  client_map <- .as_named_char(cfg$clients %||% list())
  if (length(client_map) == 0L) {
    stop("'clients' must specify at least one client URL -> dataPath mapping")
  }

  # ---- modelInfo
  model_info <- cfg$modelInfo %||% list()
  if (!is.list(model_info) || !length(model_info)) {
    stop("modelInfo must be a non-empty object")
  }
  model_name <- as.character(model_info$modelName %||% "")
  if (!length(model_name) || !nzchar(model_name[[1L]])) {
    stop("modelInfo$modelName must be a non-empty string (e.g., PK_1cmt, PK_2cmt)")
  }
  model_info$administration <- model_info$administration %||% "iv"
  model_info$res <- model_info$res %||% "mix"
  model_info$iiv <- modifyList(list(lcl = FALSE, lvc = FALSE, cor = FALSE), model_info$iiv %||% list())

  param_spec <- .build_param_spec(cfg)
  init_par <- param_spec$init
  lower    <- param_spec$lower
  upper    <- param_spec$upper

  optimControl <- cfg$optimControl %||% list()
  objective    <- .build_objective(model_info, optimControl)
  optim        <- .build_optim(optimControl)

  comm_def <- list(
    init_max_tries = fedpoppk_const("comm.init_max_tries", 20L),
    init_pause = fedpoppk_const("comm.init_pause", 5),
    init_timeout = fedpoppk_const("comm.init_timeout", 300),
    run_max_tries = fedpoppk_const("comm.run_max_tries", 3L),
    run_pause = fedpoppk_const("comm.run_pause", 1),
    run_timeout = fedpoppk_const("comm.run_timeout", 300)
  )
  comm <- modifyList(comm_def, cfg$comm %||% list())

  logging_def <- list(
    base_dir = "/project/result/logs",
    scenario = "scenario1",
    save_par_log = TRUE
  )
  logging <- modifyList(logging_def, cfg$logging %||% list())
  logging$scenario <- .resolve_logging_scenario(logging$scenario, default = logging_def$scenario)
  bootstrap <- .build_bootstrap(cfg$bootstrap %||% list())
  list(
    client_map = client_map,
    model_info = model_info,
    init_par   = init_par,
    lower      = lower,
    upper      = upper,
    param_spec = param_spec,
    param_spec_payload = .param_spec_to_payload(param_spec),
    objective  = objective,
    optim      = optim,
    comm       = comm,
    logging    = logging,
    bootstrap  = bootstrap,
    config_hash_resolved = cfg_hash_resolved
  )
}
