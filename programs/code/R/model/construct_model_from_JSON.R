# construct_model_from_JSON.R
# Build an nlmixr2 ui object from JSON model metadata plus optional init values.
# Keeps model construction configurable while preserving sensible defaults.
suppressPackageStartupMessages({
  library(rxode2)
  library(nlmixr2lib)
  library(nlmixr2)
})
local({
  candidates <- c("/project/R/common/utils.R", "R/common/utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})
local({
  candidates <- c("/project/R/common/param_transform.R", "R/common/param_transform.R")
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) source(hit[[1L]], chdir = TRUE)
})

.get_exact <- function(x, nm) {
  if (!is.list(x) || is.null(names(x))) return(NULL)
  if (!(nm %in% names(x))) return(NULL)
  x[[nm]]
}

.model_name <- function(model_info) {
  nm <- as.character(.get_exact(model_info, "modelName") %||% "")
  if (!length(nm) || !nzchar(nm[[1L]])) {
    stop("model_info$modelName must be a non-empty string")
  }
  nm[[1L]]
}

.read_model_db <- function(model_info) {
  nm <- .model_name(model_info)
  ui_try <- try(readModelDb(nm), silent = TRUE)
  if (inherits(ui_try, "try-error")) {
    stop("readModelDb failed for modelName='", nm, "': ", conditionMessage(attr(ui_try, "condition")))
  }
  ui_rx <- try(rxode2::as.rxUi(ui_try), silent = TRUE)
  if (inherits(ui_rx, "try-error")) {
    stop("as.rxUi conversion failed for modelName='", nm, "': ", conditionMessage(attr(ui_rx, "condition")))
  }
  ui_rx
}

.read_custom_model <- function(model_info) {
  code <- .get_exact(model_info, "customModel") %||%
    .get_exact(model_info, "custom_model") %||%
    .get_exact(model_info, "modelCode") %||%
    .get_exact(model_info, "model_code")
  if (is.null(code)) return(NULL)
  if (is.list(code)) code <- paste(as.character(unlist(code, use.names = FALSE)), collapse = "\n")
  code <- as.character(code)
  if (!length(code) || !nzchar(code[[1L]])) stop("custom model code is empty")
  model_obj <- eval(parse(text = code[[1L]]), envir = parent.frame())
  ui_rx <- try(rxode2::as.rxUi(model_obj), silent = TRUE)
  if (inherits(ui_rx, "try-error")) {
    stop("as.rxUi conversion failed for custom model: ", conditionMessage(attr(ui_rx, "condition")))
  }
  ui_rx
}

.find_init <- function(ui, nm) {
  iniDf <- ui$iniDf
  if (is.null(iniDf) || is.null(iniDf$name)) return(NA_integer_)
  match(nm, iniDf$name)
}

.drop_absorption_for_iv <- function(ui) {
  ui_no_ka <- try(ui |> model(-ka), silent = TRUE)
  if (!inherits(ui_no_ka, "try-error")) {
    ui <- ui_no_ka
  }
  rxode2::as.rxUi(ui)
}

.normalize_iiv_target <- function(iiv_key) {
  key <- as.character(iiv_key)
  if (!nzchar(key)) return("")

  # Accept eta-form names by converting etaLcl -> lcl, etaQ -> q, etc.
  if (startsWith(key, "eta")) {
    key <- substring(key, 4)
    if (!nzchar(key)) return("")
    key <- paste0(tolower(substring(key, 1, 1)), substring(key, 2))
  }
  key
}

.as_eta_name <- function(x) {
  x <- as.character(x)
  if (!nzchar(x)) return(x)
  if (startsWith(x, "eta")) return(x)
  paste0("eta", toupper(substring(x, 1, 1)), substring(x, 2))
}

.iiv_ref_to_eta <- function(x) {
  ref <- as.character(x)
  if (!nzchar(ref)) return("")
  if (startsWith(ref, "eta")) return(ref)
  .as_eta_name(.normalize_iiv_target(ref))
}

.resolve_iiv_targets <- function(iiv_spec) {
  iiv_spec <- iiv_spec %||% list()
  if (!length(iiv_spec)) return(character())

  enabled <- character()
  if (is.logical(iiv_spec) && !is.null(names(iiv_spec))) {
    enabled <- names(iiv_spec)[!is.na(iiv_spec) & iiv_spec]
  } else if (is.list(iiv_spec) && !is.null(names(iiv_spec))) {
    enabled <- names(iiv_spec)[vapply(iiv_spec, isTRUE, logical(1))]
  } else {
    stop("model_info$iiv must be a named logical vector/list")
  }

  reserved <- c("cor", "corr", "correlation", "cor_pairs", "corPairs")
  enabled <- setdiff(enabled, reserved)

  targets <- vapply(enabled, .normalize_iiv_target, character(1))
  unique(targets[nzchar(targets)])
}

.iiv_flag_true <- function(iiv_spec, nm) {
  if (is.list(iiv_spec) && !is.null(names(iiv_spec)) && (nm %in% names(iiv_spec))) {
    return(isTRUE(iiv_spec[[nm]]))
  }
  if (is.logical(iiv_spec) && !is.null(names(iiv_spec)) && (nm %in% names(iiv_spec))) {
    val <- iiv_spec[[nm]]
    return(isTRUE(val))
  }
  FALSE
}

.iiv_pairs <- function(iiv_spec) {
  if (!is.list(iiv_spec) || is.null(names(iiv_spec))) return(list())
  pairs <- iiv_spec$cor_pairs %||% iiv_spec$corPairs %||% list()
  if (is.null(pairs)) return(list())
  if (is.atomic(pairs) && !is.list(pairs)) return(list(pairs))
  pairs
}

.parse_cov_pair <- function(pair, default_cov = 1e-6) {
  if (is.null(pair)) return(NULL)

  cov_val <- default_cov
  vec <- character()
  if (is.list(pair) && !is.null(names(pair))) {
    nms <- tolower(names(pair))
    cov_idx <- which(nms %in% c("cov", "cor", "corr", "value"))
    if (length(cov_idx)) {
      cov_try <- suppressWarnings(as.numeric(unlist(pair[cov_idx[[1L]]], use.names = FALSE)))
      if (length(cov_try) && is.finite(cov_try[[1L]])) cov_val <- cov_try[[1L]]
    }
    p_idx <- which(!(nms %in% c("cov", "cor", "corr", "value")))
    vec <- as.character(unlist(pair[p_idx], use.names = FALSE))
  } else {
    vec <- as.character(unlist(pair, use.names = FALSE))
    if (length(vec) >= 3L) {
      cov_try <- suppressWarnings(as.numeric(vec[[3L]]))
      if (is.finite(cov_try)) cov_val <- cov_try
    }
  }

  if (length(vec) < 2L) return(NULL)
  eta1 <- .iiv_ref_to_eta(vec[[1L]])
  eta2 <- .iiv_ref_to_eta(vec[[2L]])
  if (!nzchar(eta1) || !nzchar(eta2) || identical(eta1, eta2)) return(NULL)
  list(eta1 = eta1, eta2 = eta2, cov = as.numeric(cov_val))
}

.resolve_cov_specs <- function(iiv_spec, default_cov = 1e-6) {
  pairs <- .iiv_pairs(iiv_spec)
  if (!length(pairs)) return(list())
  out <- list()
  for (pair in pairs) {
    one <- .parse_cov_pair(pair, default_cov = default_cov)
    if (!is.null(one)) out[[length(out) + 1L]] <- one
  }
  out
}

.resolve_residual_slots <- function(model_info) {
  explicit <- .get_exact(model_info, "resErrNames") %||%
    .get_exact(model_info, "residual_errors") %||%
    NULL
  if (!is.null(explicit)) {
    if (is.character(explicit)) return(unique(explicit[nzchar(explicit)]))
    if (is.list(explicit)) {
      vals <- as.character(unlist(explicit, use.names = FALSE))
      return(unique(vals[nzchar(vals)]))
    }
  }
  res <- tolower(as.character(.get_exact(model_info, "res") %||% "mix"))
  switch(
    res,
    add = "addSd",
    prop = "propSd",
    mix = c("addSd", "propSd"),
    stop("unsupported residual error type: ", res)
  )
}

# Ensure a 2x2 ETA covariance block exists for correlated IIV requests.
.ensure_cov_block <- function(ui, eta1, eta2, cov = 1e-6) {
  eta1 <- as.character(eta1)
  eta2 <- as.character(eta2)
  if (!nzchar(eta1) || !nzchar(eta2)) return(ui)

  iniDf <- ui$iniDf
  if (is.null(iniDf) || is.null(iniDf$name)) return(ui)
  i1 <- match(eta1, iniDf$name)
  i2 <- match(eta2, iniDf$name)
  if (is.na(i1) || is.na(i2)) return(ui)

  v1 <- as.numeric(iniDf$est[[i1]])
  v2 <- as.numeric(iniDf$est[[i2]])
  if (!is.finite(v1)) v1 <- 0.1
  if (!is.finite(v2)) v2 <- 0.1
  if (!is.finite(cov)) cov <- 1e-6

  cov_expr <- parse(text = sprintf(
    "%s + %s ~ c(%s, %s, %s)",
    eta1,
    eta2,
    format(v1, digits = 16, scientific = FALSE),
    format(cov, digits = 16, scientific = FALSE),
    format(v2, digits = 16, scientific = FALSE)
  ))[[1L]]
  ui <- do.call(ini, list(ui, cov_expr))
  rxode2::as.rxUi(ui)
}

# model_info:
# - modelName: model DB identifier (required; e.g., PK_1cmt, PK_2cmt)
# - administration: "iv" | "po" (required)
# - iiv: named logicals for model parameter names, e.g. list(lcl=TRUE, lvc=TRUE, cor=TRUE)
#        optional correlation pairs:
#        cor_pairs = list(c("lcl","lvc"), c("lq","lvp"), c("lcl","lq", 0.02))
# - res: "add" | "prop" | "mix" (optional; default "mix")
# - model: optional extra model() lines
construct_model_from_JSON <- function(model_info, init_par = NULL, param_spec = NULL) {
  if (!is.list(model_info) || !length(model_info)) {
    stop("model_info must be a non-empty list")
  }

  adm_raw <- as.character(.get_exact(model_info, "administration") %||% "")
  if (!length(adm_raw) || !nzchar(adm_raw[[1L]])) {
    stop("model_info$administration must be provided ('iv' or 'po')")
  }
  adm <- match.arg(
    tolower(adm_raw[[1L]]),
    c("iv", "po")
  )

  ui <- .read_custom_model(model_info)
  if (is.null(ui)) {
    ui <- .read_model_db(model_info)
    if (identical(adm, "iv")) {
      ui <- .drop_absorption_for_iv(ui)
    }

    iiv_spec <- .get_exact(model_info, "iiv") %||% list()
    iiv_targets <- .resolve_iiv_targets(iiv_spec)
    if (length(iiv_targets)) {
      ui <- ui |> addEta(iiv_targets) |> rxode2::as.rxUi()
    }

    err_slots <- .resolve_residual_slots(model_info)
    ui <- ui |> addResErr(err_slots)

    cor_flag <- .iiv_flag_true(iiv_spec, "cor") ||
      .iiv_flag_true(iiv_spec, "corr") ||
      .iiv_flag_true(iiv_spec, "correlation")
    if (cor_flag && length(iiv_targets) >= 2L) {
      specs <- .resolve_cov_specs(iiv_spec, default_cov = 1e-6)
      if (length(specs)) {
        for (sp in specs) {
          ui <- .ensure_cov_block(ui, sp$eta1, sp$eta2, cov = sp$cov)
        }
      } else {
        ui <- .ensure_cov_block(
          ui,
          .iiv_ref_to_eta(iiv_targets[[1L]]),
          .iiv_ref_to_eta(iiv_targets[[2L]]),
          cov = 1e-6
        )
      }
    }
  }

  ini_lines <- .get_exact(model_info, "ini")
  if (!is.null(ini_lines)) {
    if (is.character(ini_lines)) ini_lines <- as.list(ini_lines)
    for (line in ini_lines) {
      expr <- parse(text = line)[[1L]]
      ui <- do.call(ini, list(ui, expr))
      ui <- rxode2::as.rxUi(ui)
    }
  }

  model_lines <- .get_exact(model_info, "model")
  if (!is.null(model_lines)) {
    if (is.character(model_lines)) model_lines <- as.list(model_lines)
    for (line in model_lines) {
      expr <- parse(text = line)[[1L]]
      ui <- do.call(model, list(ui, expr))
      ui <- rxode2::as.rxUi(ui)
    }
  }

  init_src <- init_par
  if (!is.null(param_spec) && !is.null(param_spec$init)) {
    init_src <- if (exists("param_initial_ini", mode = "function")) {
      param_initial_ini(param_spec)
    } else {
      param_spec$init
    }
  }
  if (!is.null(init_src)) {
    iniDf <- ui$iniDf
    init_vec <- unlist(init_src, use.names = TRUE)
    storage.mode(init_vec) <- "double"
    unknown <- setdiff(names(init_vec), iniDf$name %||% character())
    if (length(unknown)) {
      stop(
        "init/param_spec contains parameter names not found in model iniDf: ",
        paste(unknown, collapse = ", ")
      )
    }
    for (nm in names(init_vec)) {
      idx <- match(nm, iniDf$name)
      if (!is.na(idx)) iniDf$est[idx] <- init_vec[[nm]]
    }
    ui$iniDf <- iniDf
  }

  class(ui) <- unique(c("rxUi", setdiff(class(ui), "rxUi")))
  ui
}
