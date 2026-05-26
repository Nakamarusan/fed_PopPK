# param_transform.R
# Shared parameter-spec resolution and z <-> natural transform helpers.

local({
  candidates <- c("/project/R/common/utils.R", "R/common/utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})

param_infer_role <- function(param_name) {
  if (grepl("^eta", param_name)) return("eta")
  if (grepl("Sd$", param_name) || grepl("PropSd|AddSd", param_name)) return("residual")
  "fixed"
}

param_default_transform <- function(lower, upper, role = NULL) {
  if (is.finite(lower) && is.finite(upper)) return("logit")
  if (identical(role, "eta") || identical(role, "residual")) {
    if (is.finite(lower) && !is.finite(upper)) return("lower_exp")
    if (!is.finite(lower) && !is.finite(upper)) return("exp")
  }
  if (is.finite(lower) && !is.finite(upper)) return("lower_exp")
  "identity"
}

param_resolve_spec <- function(param_spec, param_names, lower = NULL, upper = NULL) {
  if (!is.null(param_spec)) {
    param_spec$names <- param_spec$names %||% names(param_spec$init) %||% param_names
    if (is.null(param_spec$names) || any(!nzchar(param_spec$names))) {
      stop("param_spec must define non-empty parameter names")
    }
    missing <- setdiff(param_names, param_spec$names)
    if (length(missing)) {
      stop("param_spec is missing names required by request: ", paste(missing, collapse = ", "))
    }
    param_spec$transform <- param_spec$transform %||% setNames(rep("identity", length(param_spec$names)), param_spec$names)
    param_spec$lower <- param_spec$lower %||% setNames(rep(-Inf, length(param_spec$names)), param_spec$names)
    param_spec$upper <- param_spec$upper %||% setNames(rep(Inf, length(param_spec$names)), param_spec$names)
    return(param_spec)
  }

  lower_vec <- if (!is.null(lower)) {
    if (is.list(lower)) lower <- unlist(lower, use.names = TRUE)
    lower[param_names]
  } else {
    setNames(rep(-Inf, length(param_names)), param_names)
  }
  upper_vec <- if (!is.null(upper)) {
    if (is.list(upper)) upper <- unlist(upper, use.names = TRUE)
    upper[param_names]
  } else {
    setNames(rep(Inf, length(param_names)), param_names)
  }

  roles <- vapply(param_names, param_infer_role, character(1))
  transform <- vapply(seq_along(param_names), function(i) {
    param_default_transform(lower_vec[[i]], upper_vec[[i]], role = roles[[i]])
  }, character(1))

  list(
    names = param_names,
    lower = setNames(as.numeric(lower_vec), param_names),
    upper = setNames(as.numeric(upper_vec), param_names),
    transform = setNames(transform, param_names)
  )
}

param_internal_bounds <- function(param_spec) {
  nms <- param_spec$names
  lower_z <- setNames(rep(-Inf, length(nms)), nms)
  upper_z <- setNames(rep(Inf, length(nms)), nms)

  for (nm in nms) {
    tr <- param_spec$transform[[nm]] %||% "identity"
    if (identical(tr, "identity")) {
      lower_z[[nm]] <- param_spec$lower[[nm]]
      upper_z[[nm]] <- param_spec$upper[[nm]]
    }
  }

  list(lower = lower_z, upper = upper_z)
}

param_optimizer_space <- function(init_nat, param_spec, lower = NULL, upper = NULL) {
  init_nat <- unlist(init_nat, use.names = TRUE)
  storage.mode(init_nat) <- "double"
  spec <- param_resolve_spec(param_spec, names(init_nat), lower = lower, upper = upper)
  init_z <- param_nat_to_int(init_nat, spec)
  bounds_z <- param_internal_bounds(spec)
  list(
    init_z = init_z[spec$names],
    lower_z = bounds_z$lower[spec$names],
    upper_z = bounds_z$upper[spec$names],
    param_spec = spec
  )
}

param_int_to_optimizer_nat <- function(z, param_spec) {
  stopifnot(is.numeric(z), !is.null(names(z)))
  lower_vec <- param_spec$lower
  upper_vec <- param_spec$upper
  transform <- param_spec$transform
  th <- setNames(numeric(length(z)), names(z))

  for (nm in names(z)) {
    v <- z[[nm]]
    lo <- lower_vec[[nm]]
    hi <- upper_vec[[nm]]
    tr <- transform[[nm]] %||% "identity"
    if (identical(tr, "lower_exp")) {
      th[[nm]] <- lo + exp(v)
    } else if (identical(tr, "exp")) {
      th[[nm]] <- exp(v)
    } else if (identical(tr, "tanh")) {
      th[[nm]] <- tanh(v)
    } else if (identical(tr, "logit")) {
      if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
        stop("logit transform requires finite lower/upper with upper > lower")
      }
      th[[nm]] <- lo + (hi - lo) * plogis(v)
    } else {
      th[[nm]] <- v
    }
  }
  th
}

param_has_omega_blocks <- function(param_spec) {
  is.list(param_spec$omega_blocks) && length(param_spec$omega_blocks) > 0L
}

param_cov_name <- function(eta1, eta2) {
  sprintf("(%s,%s)", eta1, eta2)
}

param_rho_name <- function(eta1, eta2) {
  sprintf("rho_(%s,%s)", eta1, eta2)
}

.param_block_param_names <- function(block) {
  nms <- unlist(block$param_names %||% character(), use.names = FALSE)
  nms[nzchar(nms)]
}

.param_omega_outputs <- function(opt_nat, param_spec, include_rho = FALSE) {
  if (!param_has_omega_blocks(param_spec)) {
    return(setNames(numeric(), character()))
  }

  out <- setNames(numeric(), character())
  for (block in param_spec$omega_blocks) {
    etas <- as.character(block$etas %||% character())
    k <- length(etas)
    if (k < 1L) next

    param_names <- block$param_names
    if (is.null(param_names)) {
      stop("omega block is missing param_names")
    }
    param_names <- matrix(unlist(param_names, use.names = FALSE), nrow = k, byrow = TRUE)

    parameterization <- block$parameterization %||% "cov_chol"
    parameterization <- tolower(as.character(unlist(parameterization, use.names = FALSE))[[1L]])
    if (identical(parameterization, "precision_chol")) {
      R <- matrix(0, nrow = k, ncol = k)
      for (i in seq_len(k)) {
        for (j in i:k) {
          nm <- param_names[i, j]
          if (!nzchar(nm) || !(nm %in% names(opt_nat))) {
            stop("omega block parameter missing from optimizer vector: ", nm)
          }
          R[i, j] <- if (i == j) opt_nat[[nm]]^2 else opt_nat[[nm]]
        }
      }
      precision <- t(R) %*% R
      omega <- tryCatch(solve(precision), error = function(e) {
        stop("precision Cholesky omega block could not be inverted: ", conditionMessage(e))
      })
    } else {
      L <- matrix(0, nrow = k, ncol = k)
      for (i in seq_len(k)) {
        for (j in seq_len(i)) {
          nm <- param_names[i, j]
          if (!nzchar(nm) || !(nm %in% names(opt_nat))) {
            stop("omega block parameter missing from optimizer vector: ", nm)
          }
          L[i, j] <- opt_nat[[nm]]
        }
      }
      omega <- L %*% t(L)
    }

    vals <- numeric()
    nms <- character()
    for (i in seq_len(k)) {
      vals <- c(vals, omega[i, i])
      nms <- c(nms, etas[[i]])
    }
    if (k >= 2L) {
      for (i in 2:k) {
        for (j in seq_len(i - 1L)) {
          vals <- c(vals, omega[i, j])
          nms <- c(nms, param_cov_name(etas[[j]], etas[[i]]))
        }
      }
      if (isTRUE(include_rho)) {
        for (i in 2:k) {
          for (j in seq_len(i - 1L)) {
            denom <- sqrt(pmax(omega[j, j], .Machine$double.eps) *
                            pmax(omega[i, i], .Machine$double.eps))
            vals <- c(vals, omega[i, j] / denom)
            nms <- c(nms, param_rho_name(etas[[j]], etas[[i]]))
          }
        }
      }
    }
    names(vals) <- nms
    out <- c(out, vals)
  }
  out
}

param_int_to_ini <- function(z, param_spec) {
  opt_nat <- param_int_to_optimizer_nat(z, param_spec)
  if (!param_has_omega_blocks(param_spec)) return(opt_nat)

  block_names <- unique(unlist(lapply(param_spec$omega_blocks, .param_block_param_names), use.names = FALSE))
  direct <- opt_nat[setdiff(names(opt_nat), block_names)]
  c(direct, .param_omega_outputs(opt_nat, param_spec, include_rho = FALSE))
}

param_int_to_report <- function(z, param_spec) {
  opt_nat <- param_int_to_optimizer_nat(z, param_spec)
  if (!param_has_omega_blocks(param_spec)) return(opt_nat)

  block_names <- unique(unlist(lapply(param_spec$omega_blocks, .param_block_param_names), use.names = FALSE))
  direct <- opt_nat[setdiff(names(opt_nat), block_names)]
  c(direct, .param_omega_outputs(opt_nat, param_spec, include_rho = TRUE))
}

param_int_to_nat <- function(z, param_spec) {
  param_int_to_ini(z, param_spec)
}

param_initial_ini <- function(param_spec) {
  z <- param_nat_to_int(param_spec$init, param_spec)
  param_int_to_ini(z, param_spec)
}

param_initial_report <- function(param_spec) {
  z <- param_nat_to_int(param_spec$init, param_spec)
  param_int_to_report(z, param_spec)
}

param_nat_to_int <- function(p_nat, param_spec) {
  stopifnot(is.numeric(p_nat), !is.null(names(p_nat)))
  z <- setNames(numeric(length(p_nat)), names(p_nat))
  for (nm in names(p_nat)) {
    tr <- param_spec$transform[[nm]] %||% "identity"
    lo <- param_spec$lower[[nm]]
    hi <- param_spec$upper[[nm]]
    val <- p_nat[[nm]]
    if (identical(tr, "lower_exp")) {
      diff <- val - lo
      if (!is.finite(diff) || diff <= 0) diff <- .Machine$double.eps
      z[[nm]] <- log(diff)
    } else if (identical(tr, "exp")) {
      if (!is.finite(val) || val <= 0) val <- .Machine$double.eps
      z[[nm]] <- log(val)
    } else if (identical(tr, "tanh")) {
      z[[nm]] <- atanh(max(min(val, 1 - 1e-12), -1 + 1e-12))
    } else if (identical(tr, "logit")) {
      if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
        stop("logit transform requires finite lower/upper with upper > lower")
      }
      u <- (val - lo) / (hi - lo)
      u <- max(min(u, 1 - 1e-12), 1e-12)
      z[[nm]] <- qlogis(u)
    } else if (identical(tr, "identity")) {
      z[[nm]] <- val
    } else {
      stop("Unsupported transform: ", tr)
    }
  }
  z
}

param_dz_dp <- function(p_nat, param_spec) {
  stopifnot(is.numeric(p_nat), !is.null(names(p_nat)))
  dz <- setNames(rep(1, length(p_nat)), names(p_nat))
  for (nm in names(p_nat)) {
    tr <- param_spec$transform[[nm]] %||% "identity"
    lo <- param_spec$lower[[nm]]
    hi <- param_spec$upper[[nm]]
    val <- p_nat[[nm]]
    if (identical(tr, "lower_exp")) {
      dz[[nm]] <- 1 / pmax(val - lo, .Machine$double.eps)
    } else if (identical(tr, "exp")) {
      dz[[nm]] <- 1 / pmax(val, .Machine$double.eps)
    } else if (identical(tr, "tanh")) {
      dz[[nm]] <- 1 / pmax(1 - val^2, .Machine$double.eps)
    } else if (identical(tr, "logit")) {
      if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
        stop("logit transform requires finite lower/upper with upper > lower")
      }
      u <- (val - lo) / (hi - lo)
      u <- max(min(u, 1 - 1e-12), 1e-12)
      dz[[nm]] <- 1 / ((hi - lo) * u * (1 - u))
    } else {
      dz[[nm]] <- 1
    }
  }
  dz
}
