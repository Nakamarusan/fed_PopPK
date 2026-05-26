#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(jsonlite)
  library(mclust)
  library(rxode2)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
ROOT <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", unset = dirname(script_path))
dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

parse_numeric_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) return(default)
  vals <- suppressWarnings(as.numeric(strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(default)
  vals
}

parse_scalar_numeric_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) return(default)
  vals <- suppressWarnings(as.numeric(strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]))
  vals <- vals[is.finite(vals)]
  if (length(vals) != 1L) {
    stop(name, " must contain exactly one numeric value")
  }
  vals[[1L]]
}

parse_scalar_integer_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) return(as.integer(default))
  vals <- suppressWarnings(as.integer(strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]))
  vals <- vals[is.finite(vals)]
  if (length(vals) != 1L) {
    stop(name, " must contain exactly one integer value")
  }
  vals[[1L]]
}

parse_count_env <- function(default) {
  raw <- trimws(Sys.getenv("DESIGN_COUNTS", ""))
  if (!nzchar(raw)) return(default)
  vals <- suppressWarnings(as.integer(strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]))
  vals <- vals[is.finite(vals)]
  if (length(vals) != length(default)) {
    stop("DESIGN_COUNTS must contain exactly ", length(default), " values")
  }
  setNames(vals, names(default))
}

parse_named_count_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) return(default)
  vals <- suppressWarnings(as.integer(strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]))
  vals <- vals[is.finite(vals)]
  if (length(vals) != length(default)) {
    stop(name, " must contain exactly ", length(default), " values")
  }
  setNames(vals, names(default))
}

DESIGNS <- list(
  D1 = parse_numeric_env("D1_TIMES", c(0.25, 0.5, 1.0)),
  D2 = parse_numeric_env("D2_TIMES", c(0.5, 2.0, 5.0)),
  D3 = parse_numeric_env("D3_TIMES", c(2.0, 4.0, 6.0))
)
DESIGN_COUNTS <- parse_count_env(c(D1 = 30L, D2 = 40L, D3 = 30L))
SITE_MAPPING <- c(D1 = "Site1", D2 = "Site2", D3 = "Site3")
SITE_ORDER <- c("Site1", "Site2", "Site3")
SCENARIO2_SITE_COUNTS <- parse_named_count_env("SCENARIO2_SITE_COUNTS", c(Site1 = 30L, Site2 = 40L, Site3 = 30L))
SCENARIO2_ASSIGNMENT_METHOD <- tolower(trimws(Sys.getenv("SCENARIO2_ASSIGNMENT_METHOD", "cl_mclust_capacity")))
SCENARIO2_SOFT_TEMPERATURE <- parse_scalar_numeric_env("SCENARIO2_SOFT_TEMPERATURE", 2.0)
SCENARIO2_RANDOM_MIX <- parse_scalar_numeric_env("SCENARIO2_RANDOM_MIX", 0.0)
SCENARIO2_WT_SOFTNESS <- parse_scalar_numeric_env("SCENARIO2_WT_SOFTNESS", 1.0)
SCENARIO2_WT_SITE_CENTERS <- parse_numeric_env("SCENARIO2_WT_SITE_CENTERS", c(-0.8, 0, 0.8))
SCENARIO2_CL_SOFTNESS <- parse_scalar_numeric_env("SCENARIO2_CL_SOFTNESS", 1.5)
SCENARIO2_CL_SITE_CENTERS <- parse_numeric_env("SCENARIO2_CL_SITE_CENTERS", c(-0.45, 0, 0.45))
SCENARIO2_BALANCE_BINS <- parse_scalar_integer_env("SCENARIO2_BALANCE_BINS", 10L)
SCENARIO2_ASSIGN_SEED <- parse_scalar_integer_env("SCENARIO2_ASSIGN_SEED", 20260515L)
if (!is.finite(SCENARIO2_SOFT_TEMPERATURE) || SCENARIO2_SOFT_TEMPERATURE <= 0) {
  stop("SCENARIO2_SOFT_TEMPERATURE must be a positive finite value")
}
if (!is.finite(SCENARIO2_RANDOM_MIX) || SCENARIO2_RANDOM_MIX < 0 || SCENARIO2_RANDOM_MIX >= 1) {
  stop("SCENARIO2_RANDOM_MIX must be finite and in [0, 1)")
}
if (!is.finite(SCENARIO2_WT_SOFTNESS) || SCENARIO2_WT_SOFTNESS <= 0) {
  stop("SCENARIO2_WT_SOFTNESS must be a positive finite value")
}
if (length(SCENARIO2_WT_SITE_CENTERS) != length(SITE_ORDER)) {
  stop("SCENARIO2_WT_SITE_CENTERS must contain exactly ", length(SITE_ORDER), " values")
}
if (!is.finite(SCENARIO2_CL_SOFTNESS) || SCENARIO2_CL_SOFTNESS <= 0) {
  stop("SCENARIO2_CL_SOFTNESS must be a positive finite value")
}
if (length(SCENARIO2_CL_SITE_CENTERS) != length(SITE_ORDER)) {
  stop("SCENARIO2_CL_SITE_CENTERS must contain exactly ", length(SITE_ORDER), " values")
}
if (!is.finite(SCENARIO2_BALANCE_BINS) || SCENARIO2_BALANCE_BINS < 1L) {
  stop("SCENARIO2_BALANCE_BINS must be a positive integer")
}

SEEDS <- list(
  design = 20260511L,
  weight = 20260512L,
  eta = 20260513L,
  eps = 20260514L,
  assign = 20260515L
)

PROP_VAR <- parse_scalar_numeric_env("PROP_VAR", 0.0953)
OMEGA_CL_VAR <- parse_scalar_numeric_env("OMEGA_CL_VAR", 0.0644)
OMEGA_V_VAR <- parse_scalar_numeric_env("OMEGA_V_VAR", 0.0392)
OMEGA_CLV_COV <- parse_scalar_numeric_env("OMEGA_CLV_COV", 0.0310)
if (nzchar(trimws(Sys.getenv("OMEGA_CLV_CORR", "")))) {
  omega_corr <- parse_scalar_numeric_env("OMEGA_CLV_CORR", NA_real_)
  if (!is.finite(omega_corr) || abs(omega_corr) >= 1) {
    stop("OMEGA_CLV_CORR must be finite and between -1 and 1")
  }
  OMEGA_CLV_COV <- omega_corr * sqrt(OMEGA_CL_VAR * OMEGA_V_VAR)
}
OMEGA <- matrix(c(OMEGA_CL_VAR, OMEGA_CLV_COV, OMEGA_CLV_COV, OMEGA_V_VAR), nrow = 2, byrow = TRUE)
if (OMEGA_CL_VAR <= 0 || OMEGA_V_VAR <= 0 || det(OMEGA) <= 0) {
  stop("OMEGA must be positive definite")
}

MODEL <- list(
  dose_mgkg = parse_scalar_numeric_env("DOSE_MGKG", 25),
  wt_ref_kg = parse_scalar_numeric_env("WT_REF_KG", 16.08),
  wt_mean_kg = parse_scalar_numeric_env("WT_MEAN_KG", 16.08),
  wt_sd_kg = parse_scalar_numeric_env("WT_SD_KG", 3.18),
  wt_lower_kg = parse_scalar_numeric_env("WT_LOWER_KG", 11),
  wt_upper_kg = parse_scalar_numeric_env("WT_UPPER_KG", 22.5),
  tvcl_l_h = parse_scalar_numeric_env("TVCL_L_H", 8.3),
  tvv_l = parse_scalar_numeric_env("TVV_L", 18.7),
  tvka_h = parse_scalar_numeric_env("TVKA_H", 9.13),
  theta_wt_cl = parse_scalar_numeric_env("THETA_WT_CL", 0.75),
  theta_wt_v = parse_scalar_numeric_env("THETA_WT_V", 1.00),
  omega = OMEGA,
  prop_var = PROP_VAR,
  prop_sd = sqrt(PROP_VAR),
  mw_g_mol = parse_scalar_numeric_env("MW_G_MOL", 139.15)
)

sample_truncnorm <- function(n, mean, sd, lower, upper, seed) {
  set.seed(as.integer(seed))
  out <- numeric(n)
  filled <- 0L
  while (filled < n) {
    draw <- rnorm(n - filled, mean = mean, sd = sd)
    keep <- draw[draw >= lower & draw <= upper]
    if (length(keep)) {
      take <- min(length(keep), n - filled)
      out[(filled + 1L):(filled + take)] <- keep[seq_len(take)]
      filled <- filled + take
    }
  }
  out
}

sample_eta <- function(n, omega, seed) {
  set.seed(as.integer(seed))
  z <- matrix(rnorm(n * nrow(omega)), nrow = n)
  eta <- z %*% chol(omega)
  colnames(eta) <- c("eta_cl", "eta_v")
  eta
}

make_rxode_model <- function() {
  rxode2::rxode2({
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - (cl / v) * central
    cp = central / v
  })
}

solve_rxode_predictions <- function(model, subjects, obs_grid) {
  dose_events <- subjects[, .(
    id,
    time = 0,
    evid = 1L,
    cmt = "depot",
    amt = dose_mg
  )]
  obs_events <- obs_grid[, .(
    id,
    time,
    evid = 0L,
    cmt = "central",
    amt = NA_real_
  )]
  events <- rbindlist(list(dose_events, obs_events), use.names = TRUE)
  setorder(events, id, time, -evid)

  params <- subjects[, .(id, cl, v, ka)]
  sim <- as.data.table(rxode2::rxSolve(
    model,
    params = as.data.frame(params),
    events = as.data.frame(events),
    returnType = "data.table",
    cores = 1
  ))
  if (!("id" %in% names(sim))) sim[, id := unique(params$id)]

  sim[, .(
    id = as.integer(id),
    time = as.numeric(time),
    pred_mg_l = cp
  )]
}

make_subjects <- function() {
  n_subjects <- sum(DESIGN_COUNTS)
  design <- rep(names(DESIGN_COUNTS), times = as.integer(DESIGN_COUNTS))
  set.seed(as.integer(SEEDS$design))
  design <- sample(design, size = length(design), replace = FALSE)

  wt <- sample_truncnorm(
    n = n_subjects,
    mean = MODEL$wt_mean_kg,
    sd = MODEL$wt_sd_kg,
    lower = MODEL$wt_lower_kg,
    upper = MODEL$wt_upper_kg,
    seed = SEEDS$weight
  )
  eta <- sample_eta(n_subjects, MODEL$omega, SEEDS$eta)

  dt <- data.table(
    id = seq_len(n_subjects),
    design = design,
    Site = unname(SITE_MAPPING[design]),
    WT = wt,
    eta_cl = eta[, "eta_cl"],
    eta_v = eta[, "eta_v"]
  )
  dt[, dose_mg := MODEL$dose_mgkg * WT]
  dt[, cl := MODEL$tvcl_l_h * (WT / MODEL$wt_ref_kg)^MODEL$theta_wt_cl * exp(eta_cl)]
  dt[, v := MODEL$tvv_l * (WT / MODEL$wt_ref_kg)^MODEL$theta_wt_v * exp(eta_v)]
  dt[, ka := MODEL$tvka_h]
  setorder(dt, id)
  dt
}

make_dataset <- function(subjects) {
  ode_model <- make_rxode_model()

  dose_rows <- subjects[, .(
    id,
    time = 0,
    cmt = 1L,
    amt = dose_mg,
    ii = 0,
    addl = 0L,
    evid = 1L,
    dv = 0,
    dv_mg_l = 0,
    pred = 0,
    pred_mg_l = 0,
    cens = 0L,
    design,
    Site,
    WT,
    dose_mgkg = MODEL$dose_mgkg,
    dose_mg
  )]

  obs_grid <- rbindlist(lapply(names(DESIGNS), function(design_name) {
    ids <- subjects[design == design_name, id]
    CJ(id = ids, time = DESIGNS[[design_name]])
  }), use.names = TRUE)

  rx_pred <- solve_rxode_predictions(ode_model, subjects, obs_grid)

  obs <- obs_grid
  obs <- merge(
    obs,
    subjects[, .(id, design, Site, WT, dose_mg, cl, v, ka)],
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )

  obs <- merge(obs, rx_pred, by = c("id", "time"), all.x = TRUE, sort = FALSE)
  if (anyNA(obs$pred_mg_l)) stop("rxode2 prediction failed for one or more observations.")
  obs[, pred := pred_mg_l * 1000 / MODEL$mw_g_mol]

  set.seed(as.integer(SEEDS$eps))
  obs[, eps_prop := rnorm(.N, mean = 0, sd = MODEL$prop_sd)]
  obs[, dv_mg_l := pmax(pred_mg_l * (1 + eps_prop), 0)]
  obs[, dv := dv_mg_l * 1000 / MODEL$mw_g_mol]

  obs_rows <- obs[, .(
    id,
    time,
    cmt = 2L,
    amt = NA_real_,
    ii = NA_real_,
    addl = NA_integer_,
    evid = 0L,
    dv,
    dv_mg_l,
    pred,
    pred_mg_l,
    cens = 0L,
    design,
    Site,
    WT,
    dose_mgkg = MODEL$dose_mgkg,
    dose_mg
  )]

  out <- rbindlist(list(dose_rows, obs_rows), use.names = TRUE, fill = TRUE)
  setorder(out, id, time, evid, cmt)
  out
}

write_split_files <- function(dat) {
  base_dir <- ensure_dir(file.path(ROOT, "data", "base"))
  meta_dir <- ensure_dir(file.path(ROOT, "data", "meta"))
  for (site in SITE_ORDER) {
    split_id <- match(site, SITE_ORDER)
    one <- dat[Site == site]
    fwrite(one, file.path(base_dir, sprintf("data1_%d.csv", split_id)))
    fwrite(one, file.path(meta_dir, sprintf("data1_%d.csv", split_id)))
  }
}

soften_probability_matrix <- function(z_mat, site_counts) {
  z_mat <- as.matrix(z_mat)
  z_mat[!is.finite(z_mat)] <- 0
  z_mat[z_mat < 0] <- 0
  z_mat <- z_mat + .Machine$double.eps
  z_mat <- z_mat^(1 / SCENARIO2_SOFT_TEMPERATURE)
  z_mat <- z_mat / rowSums(z_mat)

  if (SCENARIO2_RANDOM_MIX > 0) {
    prior <- as.numeric(site_counts[colnames(z_mat)])
    prior <- prior / sum(prior)
    z_mat <- (1 - SCENARIO2_RANDOM_MIX) * z_mat +
      SCENARIO2_RANDOM_MIX * matrix(prior, nrow = nrow(z_mat), ncol = ncol(z_mat), byrow = TRUE)
    z_mat <- z_mat / rowSums(z_mat)
  }
  z_mat
}

make_scenario2_site_map_cl_mclust <- function(subjects) {
  out <- subjects[, .(id, design, cl)]
  setorder(out, cl)

  gmm <- mclust::Mclust(as.numeric(out$cl), G = length(SITE_ORDER))
  ord <- order(as.numeric(gmm$parameters$mean))
  z_mat <- gmm$z[, ord, drop = FALSE]
  colnames(z_mat) <- SITE_ORDER

  assign_tbl <- assign_sites_capacity(
    ids = out$id,
    z_mat = z_mat,
    site_levels = SITE_ORDER,
    site_counts = SCENARIO2_SITE_COUNTS,
    seed = SEEDS$assign
  )
  assigned <- merge(out, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
  if (anyNA(assigned$Site)) stop("scenario2 site assignment failed")

  result <- assigned[, .(id, Site, round, cl)]
  component_means <- as.numeric(gmm$parameters$mean)[ord]
  attr(result, "component_means_ordered") <- component_means
  attr(result, "assignment_meta") <- list(
    method = "Mclust posterior-probability assignment using true individual CL with fixed site capacities",
    assignment_variable = "cl",
    true_cl_column = "cl",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    component_means_ordered = as.list(component_means),
    seed = SEEDS$assign
  )
  result
}

make_scenario2_site_map_cl_mclust_soft <- function(subjects) {
  out <- subjects[, .(id, design, cl)]
  setorder(out, cl)

  gmm <- mclust::Mclust(as.numeric(out$cl), G = length(SITE_ORDER))
  ord <- order(as.numeric(gmm$parameters$mean))
  z_mat <- gmm$z[, ord, drop = FALSE]
  colnames(z_mat) <- SITE_ORDER
  z_soft <- soften_probability_matrix(z_mat, SCENARIO2_SITE_COUNTS)

  assign_tbl <- assign_sites_capacity(
    ids = out$id,
    z_mat = z_soft,
    site_levels = SITE_ORDER,
    site_counts = SCENARIO2_SITE_COUNTS,
    seed = SEEDS$assign
  )
  assigned <- merge(out, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
  if (anyNA(assigned$Site)) stop("scenario2 soft Mclust site assignment failed")

  result <- assigned[, .(id, Site, round, cl)]
  component_means <- as.numeric(gmm$parameters$mean)[ord]
  attr(result, "component_means_ordered") <- component_means
  attr(result, "assignment_meta") <- list(
    method = "Temperature-softened Mclust posterior assignment using true individual CL with fixed site capacities",
    assignment_variable = "cl",
    true_cl_column = "cl",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    component_means_ordered = as.list(component_means),
    soft_temperature = SCENARIO2_SOFT_TEMPERATURE,
    random_mix = SCENARIO2_RANDOM_MIX,
    seed = SEEDS$assign
  )
  result
}

make_scenario2_site_map_cl_mclust_soft_design_quota <- function(subjects) {
  quota <- make_design_site_quota()

  pieces <- lapply(rownames(quota), function(design_name) {
    one <- subjects[design == design_name, .(id, design, cl)]
    setorder(one, cl)
    gmm <- mclust::Mclust(as.numeric(one$cl), G = length(SITE_ORDER))
    ord <- order(as.numeric(gmm$parameters$mean))
    z_mat <- gmm$z[, ord, drop = FALSE]
    colnames(z_mat) <- SITE_ORDER
    site_counts <- setNames(as.integer(quota[design_name, SITE_ORDER]), SITE_ORDER)
    z_soft <- soften_probability_matrix(z_mat, site_counts)
    assign_tbl <- assign_sites_capacity(
      ids = one$id,
      z_mat = z_soft,
      site_levels = SITE_ORDER,
      site_counts = site_counts,
      seed = SEEDS$assign + match(design_name, rownames(quota))
    )
    assigned <- merge(one, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
    if (anyNA(assigned$Site)) stop("scenario2 soft Mclust design-quota assignment failed for ", design_name)
    assigned
  })
  result <- rbindlist(pieces, use.names = TRUE)
  setorder(result, id)
  result <- result[, .(id, Site, round, cl)]

  quota_dt <- as.data.table(as.table(quota))
  setnames(quota_dt, c("design", "Site", "n_subjects"))
  attr(result, "assignment_meta") <- list(
    method = "Design-stratified temperature-softened Mclust posterior assignment using true individual CL with exact design-by-site quotas",
    assignment_variable = "cl",
    true_cl_column = "cl",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    design_counts = as.list(DESIGN_COUNTS),
    design_site_quota = split(quota_dt, quota_dt$Site),
    soft_temperature = SCENARIO2_SOFT_TEMPERATURE,
    random_mix = SCENARIO2_RANDOM_MIX,
    seed = SEEDS$assign
  )
  result
}

make_scenario2_site_map_mclust_global_soft_design_quota <- function(subjects, variable) {
  if (!variable %in% c("cl", "WT")) stop("variable must be cl or WT")
  quota <- make_design_site_quota()

  out <- subjects[, .(id, design, WT, cl)]
  setorder(out, id)

  x <- as.numeric(out[[variable]])
  gmm <- mclust::Mclust(x, G = length(SITE_ORDER))
  ord <- order(as.numeric(gmm$parameters$mean))
  z_mat <- gmm$z[, ord, drop = FALSE]
  colnames(z_mat) <- SITE_ORDER
  z_soft <- soften_probability_matrix(z_mat, SCENARIO2_SITE_COUNTS)
  rownames(z_soft) <- as.character(out$id)

  pieces <- lapply(rownames(quota), function(design_name) {
    one <- out[design == design_name]
    site_counts <- setNames(as.integer(quota[design_name, SITE_ORDER]), SITE_ORDER)
    z_one <- z_soft[as.character(one$id), SITE_ORDER, drop = FALSE]
    assign_tbl <- assign_sites_capacity(
      ids = one$id,
      z_mat = z_one,
      site_levels = SITE_ORDER,
      site_counts = site_counts,
      seed = SEEDS$assign + match(design_name, rownames(quota))
    )
    assigned <- merge(one, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
    if (anyNA(assigned$Site)) {
      stop("scenario2 global soft Mclust design-quota assignment failed for ", design_name)
    }
    assigned
  })
  result <- rbindlist(pieces, use.names = TRUE)
  setorder(result, id)
  result <- result[, .(id, Site, round, cl, WT)]

  quota_dt <- as.data.table(as.table(quota))
  setnames(quota_dt, c("design", "Site", "n_subjects"))
  component_means <- as.numeric(gmm$parameters$mean)[ord]
  attr(result, "component_means_ordered") <- component_means
  attr(result, "assignment_meta") <- list(
    method = paste0(
      "Global temperature-softened Mclust posterior assignment using generated individual ",
      variable,
      " with exact design-by-site quotas"
    ),
    assignment_variable = variable,
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    design_counts = as.list(DESIGN_COUNTS),
    design_site_quota = split(quota_dt, quota_dt$Site),
    component_means_ordered = as.list(component_means),
    soft_temperature = SCENARIO2_SOFT_TEMPERATURE,
    random_mix = SCENARIO2_RANDOM_MIX,
    seed = SEEDS$assign
  )
  result
}

make_scenario2_site_map_wt_mclust_soft <- function(subjects) {
  out <- subjects[, .(id, design, WT, cl)]
  setorder(out, WT, id)

  gmm <- mclust::Mclust(as.numeric(out$WT), G = length(SITE_ORDER))
  ord <- order(as.numeric(gmm$parameters$mean))
  z_mat <- gmm$z[, ord, drop = FALSE]
  colnames(z_mat) <- SITE_ORDER
  z_soft <- soften_probability_matrix(z_mat, SCENARIO2_SITE_COUNTS)

  assign_tbl <- assign_sites_capacity(
    ids = out$id,
    z_mat = z_soft,
    site_levels = SITE_ORDER,
    site_counts = SCENARIO2_SITE_COUNTS,
    seed = SEEDS$assign
  )
  assigned <- merge(out, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
  if (anyNA(assigned$Site)) stop("scenario2 soft Mclust WT site assignment failed")

  result <- assigned[, .(id, Site, round, cl, WT)]
  component_means <- as.numeric(gmm$parameters$mean)[ord]
  attr(result, "component_means_ordered") <- component_means
  attr(result, "assignment_meta") <- list(
    method = "Temperature-softened Mclust posterior assignment using generated individual WT with fixed site capacities",
    assignment_variable = "WT",
    true_wt_column = "WT",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    component_means_ordered = as.list(component_means),
    soft_temperature = SCENARIO2_SOFT_TEMPERATURE,
    random_mix = SCENARIO2_RANDOM_MIX,
    seed = SEEDS$assign
  )
  result
}

make_scenario2_site_map_wt_mclust_soft_design_quota <- function(subjects) {
  quota <- make_design_site_quota()

  pieces <- lapply(rownames(quota), function(design_name) {
    one <- subjects[design == design_name, .(id, design, WT, cl)]
    setorder(one, WT, id)
    gmm <- mclust::Mclust(as.numeric(one$WT), G = length(SITE_ORDER))
    ord <- order(as.numeric(gmm$parameters$mean))
    z_mat <- gmm$z[, ord, drop = FALSE]
    colnames(z_mat) <- SITE_ORDER
    site_counts <- setNames(as.integer(quota[design_name, SITE_ORDER]), SITE_ORDER)
    z_soft <- soften_probability_matrix(z_mat, site_counts)
    assign_tbl <- assign_sites_capacity(
      ids = one$id,
      z_mat = z_soft,
      site_levels = SITE_ORDER,
      site_counts = site_counts,
      seed = SEEDS$assign + match(design_name, rownames(quota))
    )
    assigned <- merge(one, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
    if (anyNA(assigned$Site)) stop("scenario2 soft Mclust WT design-quota assignment failed for ", design_name)
    assigned
  })
  result <- rbindlist(pieces, use.names = TRUE)
  setorder(result, id)
  result <- result[, .(id, Site, round, cl, WT)]

  quota_dt <- as.data.table(as.table(quota))
  setnames(quota_dt, c("design", "Site", "n_subjects"))
  attr(result, "assignment_meta") <- list(
    method = "Design-stratified temperature-softened Mclust posterior assignment using generated individual WT with exact design-by-site quotas",
    assignment_variable = "WT",
    true_wt_column = "WT",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    design_counts = as.list(DESIGN_COUNTS),
    design_site_quota = split(quota_dt, quota_dt$Site),
    soft_temperature = SCENARIO2_SOFT_TEMPERATURE,
    random_mix = SCENARIO2_RANDOM_MIX,
    seed = SEEDS$assign
  )
  result
}

make_scenario2_site_map_wt_quantile <- function(subjects) {
  out <- subjects[, .(id, design, WT, cl)]
  setorder(out, WT, id)

  site_counts <- as.integer(SCENARIO2_SITE_COUNTS[SITE_ORDER])
  names(site_counts) <- SITE_ORDER
  if (sum(site_counts) != nrow(out)) {
    stop("sum(SCENARIO2_SITE_COUNTS) must equal number of subjects")
  }

  out[, Site := rep(SITE_ORDER, times = site_counts)]
  out[, round := seq_len(.N)]
  result <- out[, .(id, Site, round, cl, WT)]

  cutpoints <- result[, .(
    min_wt_kg = min(WT),
    max_wt_kg = max(WT),
    n_subjects = .N
  ), by = Site][match(SITE_ORDER, Site)]
  attr(result, "assignment_meta") <- list(
    method = "Rank-based assignment using generated individual WT with fixed site capacities; Site1=low WT, Site2=middle WT, Site3=high WT",
    assignment_variable = "WT",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    weight_ranges_kg = split(cutpoints[, .(min_wt_kg, max_wt_kg, n_subjects)], cutpoints$Site)
  )
  result
}

make_design_site_quota <- function() {
  total <- sum(DESIGN_COUNTS)
  quota_raw <- outer(
    as.numeric(DESIGN_COUNTS),
    as.numeric(SCENARIO2_SITE_COUNTS[SITE_ORDER]),
    FUN = function(d, s) d * s / total
  )
  rownames(quota_raw) <- names(DESIGN_COUNTS)
  colnames(quota_raw) <- SITE_ORDER
  if (any(abs(quota_raw - round(quota_raw)) > 1e-8)) {
    stop(
      "DESIGN_COUNTS and SCENARIO2_SITE_COUNTS do not yield integer design-by-site quotas. ",
      "Use counts where design_count * site_count / total_n is integer."
    )
  }
  quota <- round(quota_raw)
  if (any(rowSums(quota) != as.integer(DESIGN_COUNTS))) stop("design quota row sums are inconsistent")
  if (any(colSums(quota) != as.integer(SCENARIO2_SITE_COUNTS[SITE_ORDER]))) stop("site quota column sums are inconsistent")
  quota
}

make_scenario2_site_map_wt_design_quota <- function(subjects) {
  quota <- make_design_site_quota()

  pieces <- lapply(rownames(quota), function(design_name) {
    one <- subjects[design == design_name, .(id, design, WT, cl)]
    setorder(one, WT, id)
    one[, Site := rep(SITE_ORDER, times = as.integer(quota[design_name, SITE_ORDER]))]
    one[, round := seq_len(.N)]
    one
  })
  result <- rbindlist(pieces, use.names = TRUE)
  setorder(result, WT, id)
  result <- result[, .(id, Site, round, cl, WT)]

  quota_dt <- as.data.table(as.table(quota))
  setnames(quota_dt, c("design", "Site", "n_subjects"))
  attr(result, "assignment_meta") <- list(
    method = "Design-stratified rank-based assignment using generated individual WT with exact design-by-site quotas",
    assignment_variable = "WT",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    design_counts = as.list(DESIGN_COUNTS),
    design_site_quota = split(quota_dt, quota_dt$Site)
  )
  result
}

make_wt_soft_score_matrix <- function(wt) {
  z <- as.numeric(scale(wt))
  if (any(!is.finite(z))) z <- rep(0, length(wt))
  centers <- setNames(as.numeric(SCENARIO2_WT_SITE_CENTERS), SITE_ORDER)
  scores <- vapply(centers, function(center) {
    exp(-0.5 * ((z - center) / SCENARIO2_WT_SOFTNESS)^2)
  }, numeric(length(z)))
  scores <- as.matrix(scores)
  colnames(scores) <- SITE_ORDER
  scores
}

make_scenario2_site_map_wt_soft <- function(subjects) {
  out <- subjects[, .(id, design, WT, cl)]
  scores <- make_wt_soft_score_matrix(out$WT)
  assign_tbl <- assign_sites_capacity(
    ids = out$id,
    z_mat = scores,
    site_levels = SITE_ORDER,
    site_counts = SCENARIO2_SITE_COUNTS,
    seed = SEEDS$assign
  )
  assigned <- merge(out, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
  if (anyNA(assigned$Site)) stop("scenario2 soft WT site assignment failed")
  result <- assigned[, .(id, Site, round, cl, WT)]
  setorder(result, id)

  wt_summary <- result[, .(
    n_subjects = .N,
    min_wt_kg = min(WT),
    q25_wt_kg = as.numeric(quantile(WT, 0.25)),
    median_wt_kg = median(WT),
    q75_wt_kg = as.numeric(quantile(WT, 0.75)),
    max_wt_kg = max(WT)
  ), by = Site][match(SITE_ORDER, Site)]
  attr(result, "assignment_meta") <- list(
    method = "Soft probability assignment using generated individual WT with fixed site capacities; Site1 tends lower WT, Site2 middle WT, Site3 higher WT",
    assignment_variable = "WT",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    wt_softness = SCENARIO2_WT_SOFTNESS,
    wt_site_centers = as.list(setNames(as.numeric(SCENARIO2_WT_SITE_CENTERS), SITE_ORDER)),
    weight_summary_kg = split(wt_summary, wt_summary$Site),
    seed = SEEDS$assign
  )
  result
}

make_scenario2_site_map_wt_soft_design_quota <- function(subjects) {
  quota <- make_design_site_quota()

  pieces <- lapply(rownames(quota), function(design_name) {
    one <- subjects[design == design_name, .(id, design, WT, cl)]
    scores <- make_wt_soft_score_matrix(one$WT)
    assign_tbl <- assign_sites_capacity(
      ids = one$id,
      z_mat = scores,
      site_levels = SITE_ORDER,
      site_counts = setNames(as.integer(quota[design_name, SITE_ORDER]), SITE_ORDER),
      seed = SEEDS$assign + match(design_name, rownames(quota))
    )
    assigned <- merge(one, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
    if (anyNA(assigned$Site)) stop("scenario2 soft WT design-quota assignment failed for ", design_name)
    assigned
  })
  result <- rbindlist(pieces, use.names = TRUE)
  setorder(result, id)
  result <- result[, .(id, Site, round, cl, WT)]

  quota_dt <- as.data.table(as.table(quota))
  setnames(quota_dt, c("design", "Site", "n_subjects"))
  wt_summary <- result[, .(
    n_subjects = .N,
    min_wt_kg = min(WT),
    q25_wt_kg = as.numeric(quantile(WT, 0.25)),
    median_wt_kg = median(WT),
    q75_wt_kg = as.numeric(quantile(WT, 0.75)),
    max_wt_kg = max(WT)
  ), by = Site][match(SITE_ORDER, Site)]
  attr(result, "assignment_meta") <- list(
    method = "Design-stratified soft probability assignment using generated individual WT with exact design-by-site quotas",
    assignment_variable = "WT",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    design_counts = as.list(DESIGN_COUNTS),
    design_site_quota = split(quota_dt, quota_dt$Site),
    wt_softness = SCENARIO2_WT_SOFTNESS,
    wt_site_centers = as.list(setNames(as.numeric(SCENARIO2_WT_SITE_CENTERS), SITE_ORDER)),
    weight_summary_kg = split(wt_summary, wt_summary$Site),
    seed = SEEDS$assign
  )
  result
}

make_soft_score_matrix <- function(x, centers, softness) {
  z <- as.numeric(scale(x))
  if (any(!is.finite(z))) z <- rep(0, length(x))
  centers <- setNames(as.numeric(centers), SITE_ORDER)
  scores <- vapply(centers, function(center) {
    exp(-0.5 * ((z - center) / softness)^2)
  }, numeric(length(z)))
  scores <- as.matrix(scores)
  colnames(scores) <- SITE_ORDER
  scores
}

make_balance_block_site_quota <- function(block_counts, site_counts) {
  block_counts <- as.integer(block_counts)
  site_counts <- as.integer(site_counts[SITE_ORDER])
  if (sum(block_counts) != sum(site_counts)) {
    stop("block counts and site counts must sum to the same total")
  }
  n_block <- length(block_counts)
  raw <- outer(block_counts, site_counts, FUN = function(b, s) b * s / sum(block_counts))
  quota <- matrix(0L, nrow = n_block, ncol = length(SITE_ORDER))
  row_remaining <- block_counts
  col_remaining <- site_counts

  while (sum(row_remaining) > 0L) {
    score <- raw - quota
    score[row_remaining <= 0L, ] <- -Inf
    score[, col_remaining <= 0L] <- -Inf
    pick <- which(score == max(score), arr.ind = TRUE)[1, ]
    i <- pick[[1L]]
    j <- pick[[2L]]
    quota[i, j] <- quota[i, j] + 1L
    row_remaining[[i]] <- row_remaining[[i]] - 1L
    col_remaining[[j]] <- col_remaining[[j]] - 1L
  }

  colnames(quota) <- SITE_ORDER
  quota
}

make_scenario2_site_map_cl_soft_bwbalanced_design_quota <- function(subjects) {
  quota <- make_design_site_quota()
  out <- subjects[, .(id, design, WT, cl)]
  setorder(out, id)
  scores <- make_soft_score_matrix(
    out$cl,
    centers = SCENARIO2_CL_SITE_CENTERS,
    softness = SCENARIO2_CL_SOFTNESS
  )
  rownames(scores) <- as.character(out$id)

  pieces <- lapply(rownames(quota), function(design_name) {
    one <- out[design == design_name]
    setorder(one, WT, id)
    one[, balance_block := pmin(
      SCENARIO2_BALANCE_BINS,
      floor((seq_len(.N) - 1L) * SCENARIO2_BALANCE_BINS / .N) + 1L
    )]
    block_counts <- one[, .N, by = balance_block][order(balance_block)]
    site_counts <- setNames(as.integer(quota[design_name, SITE_ORDER]), SITE_ORDER)
    block_quota <- make_balance_block_site_quota(block_counts$N, site_counts)

    block_pieces <- lapply(seq_len(nrow(block_counts)), function(block_i) {
      block <- one[balance_block == block_counts$balance_block[[block_i]]]
      z_block <- scores[as.character(block$id), SITE_ORDER, drop = FALSE]
      assign_tbl <- assign_sites_capacity(
        ids = block$id,
        z_mat = z_block,
        site_levels = SITE_ORDER,
        site_counts = setNames(as.integer(block_quota[block_i, SITE_ORDER]), SITE_ORDER),
        seed = SCENARIO2_ASSIGN_SEED +
          1000L * match(design_name, rownames(quota)) +
          as.integer(block_counts$balance_block[[block_i]])
      )
      assigned <- merge(block, assign_tbl, by = "id", all.x = TRUE, sort = FALSE)
      if (anyNA(assigned$Site)) {
        stop("scenario2 CL soft BW-balanced design-quota assignment failed for ", design_name)
      }
      assigned
    })
    rbindlist(block_pieces, use.names = TRUE, fill = TRUE)
  })
  result <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
  setorder(result, id)
  result <- result[, .(id, Site, round, cl, WT)]

  quota_dt <- as.data.table(as.table(quota))
  setnames(quota_dt, c("design", "Site", "n_subjects"))
  cl_summary <- result[, .(
    n_subjects = .N,
    min_cl = min(cl),
    q25_cl = as.numeric(quantile(cl, 0.25)),
    median_cl = median(cl),
    q75_cl = as.numeric(quantile(cl, 0.75)),
    max_cl = max(cl)
  ), by = Site][match(SITE_ORDER, Site)]
  wt_summary <- result[, .(
    n_subjects = .N,
    min_wt_kg = min(WT),
    q25_wt_kg = as.numeric(quantile(WT, 0.25)),
    median_wt_kg = median(WT),
    q75_wt_kg = as.numeric(quantile(WT, 0.75)),
    max_wt_kg = max(WT)
  ), by = Site][match(SITE_ORDER, Site)]
  attr(result, "assignment_meta") <- list(
    method = "Design-stratified Gaussian soft allocation using generated individual CL with exact design-by-site quotas and WT-quantile balance blocks",
    assignment_variable = "cl",
    balance_variable = "WT",
    site_levels = as.list(SITE_ORDER),
    site_counts = as.list(SCENARIO2_SITE_COUNTS),
    design_counts = as.list(DESIGN_COUNTS),
    design_site_quota = split(quota_dt, quota_dt$Site),
    cl_softness = SCENARIO2_CL_SOFTNESS,
    cl_site_centers = as.list(setNames(as.numeric(SCENARIO2_CL_SITE_CENTERS), SITE_ORDER)),
    balance_bins = SCENARIO2_BALANCE_BINS,
    assign_seed = SCENARIO2_ASSIGN_SEED,
    cl_summary = split(cl_summary, cl_summary$Site),
    weight_summary_kg = split(wt_summary, wt_summary$Site)
  )
  result
}

make_scenario2_site_map <- function(subjects) {
  method <- SCENARIO2_ASSIGNMENT_METHOD
  if (method %in% c("cl_mclust_capacity", "mclust", "cl_mclust")) {
    return(make_scenario2_site_map_cl_mclust(subjects))
  }
  if (method %in% c("cl_mclust_soft_capacity", "cl_soft_mclust_capacity", "mclust_soft")) {
    return(make_scenario2_site_map_cl_mclust_soft(subjects))
  }
  if (method %in% c("cl_mclust_soft_design_quota", "cl_soft_mclust_design_quota", "mclust_soft_design_quota")) {
    return(make_scenario2_site_map_cl_mclust_soft_design_quota(subjects))
  }
  if (method %in% c("cl_mclust_global_soft_design_quota", "cl_global_mclust_soft_design_quota")) {
    return(make_scenario2_site_map_mclust_global_soft_design_quota(subjects, "cl"))
  }
  if (method %in% c(
    "cl_soft_bwbalanced_design_quota",
    "cl_soft_wtbalanced_design_quota",
    "cl_target_bwbalanced_design_quota",
    "cl_gaussian_soft_bwbalanced_design_quota"
  )) {
    return(make_scenario2_site_map_cl_soft_bwbalanced_design_quota(subjects))
  }
  if (method %in% c("wt_quantile_capacity", "weight_quantile_capacity", "wt_low_mid_high", "weight_low_mid_high")) {
    return(make_scenario2_site_map_wt_quantile(subjects))
  }
  if (method %in% c("wt_mclust_soft_capacity", "wt_soft_mclust_capacity", "weight_mclust_soft_capacity")) {
    return(make_scenario2_site_map_wt_mclust_soft(subjects))
  }
  if (method %in% c("wt_mclust_soft_design_quota", "wt_soft_mclust_design_quota", "weight_mclust_soft_design_quota")) {
    return(make_scenario2_site_map_wt_mclust_soft_design_quota(subjects))
  }
  if (method %in% c("wt_mclust_global_soft_design_quota", "wt_global_mclust_soft_design_quota", "weight_mclust_global_soft_design_quota")) {
    return(make_scenario2_site_map_mclust_global_soft_design_quota(subjects, "WT"))
  }
  if (method %in% c("wt_design_stratified_capacity", "wt_design_quota", "weight_design_quota", "wt_stratified_design_quota")) {
    return(make_scenario2_site_map_wt_design_quota(subjects))
  }
  if (method %in% c("wt_soft_capacity", "weight_soft_capacity", "wt_prob_capacity", "weight_prob_capacity")) {
    return(make_scenario2_site_map_wt_soft(subjects))
  }
  if (method %in% c("wt_soft_design_quota", "weight_soft_design_quota", "wt_prob_design_quota", "weight_prob_design_quota")) {
    return(make_scenario2_site_map_wt_soft_design_quota(subjects))
  }
  stop("Unsupported SCENARIO2_ASSIGNMENT_METHOD: ", method)
}

assign_sites_capacity <- function(ids, z_mat, site_levels, site_counts, seed) {
  n <- length(ids)
  k <- length(site_levels)
  site_counts <- as.integer(site_counts[site_levels])
  names(site_counts) <- site_levels
  if (ncol(z_mat) != k) stop("z_mat column count must equal number of sites")
  if (any(is.na(site_counts)) || any(site_counts < 0L)) stop("invalid site_counts")
  if (sum(site_counts) != n) stop("sum(site_counts) must equal number of subjects")

  remaining_idx <- seq_len(n)
  remaining_counts <- site_counts
  assign_list <- vector("list", n)
  step <- 1L
  round_idx <- 1L
  set.seed(as.integer(seed))

  while (sum(remaining_counts) > 0L) {
    for (site in site_levels[remaining_counts > 0L]) {
      site_idx <- match(site, site_levels)
      cand_idx <- remaining_idx
      prob_raw <- z_mat[cand_idx, site_idx]
      prob_raw[is.na(prob_raw)] <- 0
      prob_raw[prob_raw < 0] <- 0
      s <- sum(prob_raw)
      prob <- if (s <= 0) rep(1 / length(cand_idx), length(cand_idx)) else prob_raw / s
      pick_local <- sample.int(length(cand_idx), size = 1L, prob = prob)
      pick_idx <- cand_idx[[pick_local]]
      assign_list[[step]] <- data.table(id = ids[[pick_idx]], Site = site, round = round_idx)
      remaining_idx <- setdiff(remaining_idx, pick_idx)
      remaining_counts[[site]] <- remaining_counts[[site]] - 1L
      step <- step + 1L
    }
    round_idx <- round_idx + 1L
  }

  assigned <- rbindlist(assign_list, fill = TRUE)
  chk <- assigned[, .N, by = Site][order(Site)]
  expected <- data.table(Site = site_levels, expected_N = as.integer(site_counts))
  chk <- merge(expected, chk, by = "Site", all.x = TRUE)
  chk[is.na(N), N := 0L]
  if (any(chk$N != chk$expected_N)) stop("capacity assignment failed")
  assigned
}

write_scenario_split_files <- function(dat, subjects) {
  scenario1_base <- ensure_dir(file.path(ROOT, "data", "scenario1", "base"))
  scenario1_meta <- ensure_dir(file.path(ROOT, "data", "scenario1", "meta"))
  scenario2_base <- ensure_dir(file.path(ROOT, "data", "scenario2", "base"))
  scenario2_meta <- ensure_dir(file.path(ROOT, "data", "scenario2", "meta"))

  id_params_base <- subjects[, .(
    id, design, Site, WT,
    dose_mgkg = MODEL$dose_mgkg,
    dose_mg, cl, v, ka, eta_cl, eta_v
  )]

  for (site in SITE_ORDER) {
    split_id <- match(site, SITE_ORDER)
    one <- dat[Site == site]
    fwrite(one, file.path(scenario1_base, sprintf("data1_%d.csv", split_id)))
    fwrite(one, file.path(scenario1_meta, sprintf("data1_%d.csv", split_id)))
  }
  fwrite(id_params_base, file.path(scenario1_meta, "id_params.csv"))

  site2_map <- make_scenario2_site_map(subjects)
  assignment_meta <- attr(site2_map, "assignment_meta")
  dat2 <- copy(dat)
  dat2[, Site := NULL]
  dat2 <- merge(dat2, site2_map[, .(id, Site)], by = "id", all.x = TRUE, sort = FALSE)
  setorder(dat2, id, time, evid, cmt)
  subjects2 <- copy(subjects)
  subjects2[, Site := NULL]
  subjects2 <- merge(subjects2, site2_map[, .(id, Site, round)], by = "id", all.x = TRUE, sort = FALSE)
  setorder(subjects2, id)

  for (site in SITE_ORDER) {
    split_id <- match(site, SITE_ORDER)
    one <- dat2[Site == site]
    fwrite(one, file.path(scenario2_base, sprintf("data1_%d.csv", split_id)))
    fwrite(one, file.path(scenario2_meta, sprintf("data1_%d.csv", split_id)))
  }
  id_params2 <- subjects2[, .(
    id, design, Site, WT,
    dose_mgkg = MODEL$dose_mgkg,
    dose_mg, cl, v, ka, eta_cl, eta_v
  )]
  fwrite(id_params2, file.path(scenario2_meta, "id_params.csv"))
  fwrite(id_params2, file.path(scenario2_meta, "id_params_assigned.csv"))

  assignment_audit <- merge(
    subjects[, .(id, design, source_site = Site, WT, cl, v, eta_cl, eta_v)],
    site2_map[, .(id, site_assigned = Site, assignment_round = round)],
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )
  assignment_audit[, site_changed := source_site != site_assigned]
  setorder(assignment_audit, id)
  fwrite(assignment_audit, file.path(scenario2_meta, "site_assignment_audit.csv"))

  write_json(
    list(
      scenario = "scenario2",
      assignment = assignment_meta
    ),
    file.path(scenario2_meta, "generation_meta.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = 10
  )
}

make_summaries <- function(dat, subjects) {
  table_dir <- ensure_dir(file.path(ROOT, "reports", "tables"))
  meta_dir <- ensure_dir(file.path(ROOT, "data", "meta"))
  check_dir <- ensure_dir(file.path(ROOT, "checks"))

  obs <- dat[evid == 0L]
  summary <- obs[, .(
    n = .N,
    median_dv_umol_l = as.numeric(median(dv)),
    q05_dv_umol_l = as.numeric(quantile(dv, 0.05)),
    q95_dv_umol_l = as.numeric(quantile(dv, 0.95)),
    median_pred_umol_l = as.numeric(median(pred)),
    q05_pred_umol_l = as.numeric(quantile(pred, 0.05)),
    q95_pred_umol_l = as.numeric(quantile(pred, 0.95))
  ), by = .(design, time)]
  setorder(summary, design, time)
  fwrite(summary, file.path(table_dir, "dv_time_summary.csv"))

  design_check <- obs[, .(
    n_subjects = uniqueN(id),
    observed_times = paste(sort(unique(time)), collapse = ","),
    expected_times = paste(DESIGNS[[unique(design)]], collapse = ",")
  ), by = .(design, Site)]
  setorder(design_check, design)
  fwrite(design_check, file.path(check_dir, "design_time_check.csv"))

  id_params <- subjects[, .(
    id,
    design,
    Site,
    WT,
    dose_mgkg = MODEL$dose_mgkg,
    dose_mg,
    cl,
    v,
    ka,
    eta_cl,
    eta_v
  )]
  fwrite(id_params, file.path(meta_dir, "id_params.csv"))

  list(summary = summary, design_check = design_check)
}

plot_outputs <- function(dat, summary) {
  fig_dir <- ensure_dir(file.path(ROOT, "reports", "figures"))
  obs <- dat[evid == 0L]
  obs[, Site := factor(as.character(Site), levels = c("Site1", "Site2", "Site3"))]
  obs[, design := factor(as.character(design), levels = c("D1", "D2", "D3"))]
  design_cols <- c(D1 = "#F8766D", D2 = "#00BA38", D3 = "#619CFF")

  typical_subject <- data.table(
    id = 1L,
    dose_mg = MODEL$dose_mgkg * MODEL$wt_ref_kg,
    cl = MODEL$tvcl_l_h,
    v = MODEL$tvv_l,
    ka = MODEL$tvka_h
  )
  typical_grid <- data.table(id = 1L, time = seq(0, 6, by = 0.02))
  typical_curve <- solve_rxode_predictions(make_rxode_model(), typical_subject, typical_grid)
  typical_curve[, pred := pred_mg_l * 1000 / MODEL$mw_g_mol]

  dv_time_theme <- function(strip = FALSE) {
    theme_bw(base_family = "serif") +
      theme(
        panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.25),
        panel.grid.major.y = element_line(colour = "#eeeeee", linewidth = 0.25),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        plot.margin = margin(8, 12, 8, 8),
        strip.background = if (strip) element_rect(fill = "#f3f3f3", colour = "#222222") else element_blank(),
        strip.text = if (strip) element_text(face = "bold", size = 17) else element_blank()
      )
  }

  p1 <- ggplot(obs, aes(x = time, y = dv, group = id, color = design)) +
    geom_line(alpha = 0.16, linewidth = 0.25) +
    geom_point(alpha = 0.58, size = 1.8) +
    geom_line(
      data = typical_curve,
      aes(x = time, y = pred),
      inherit.aes = FALSE,
      linewidth = 0.7,
      color = "#202020"
    ) +
    facet_wrap(~ Site, nrow = 1) +
    scale_x_continuous(breaks = 0:6) +
    scale_color_manual(values = design_cols, drop = FALSE) +
    coord_cartesian(xlim = c(0, 6)) +
    labs(
      x = "Time after dose (h)",
      y = "Observed concentration (umol/L)",
      color = "Sampling design"
    ) +
    dv_time_theme(strip = TRUE)

  ggsave(
    filename = file.path(fig_dir, "dv_time_by_design.png"),
    plot = p1,
    width = 13,
    height = 6.4,
    dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "dv_time_by_design.pdf"),
    plot = p1,
    width = 13,
    height = 6.4
  )
  ggsave(
    filename = file.path(fig_dir, "dv_time_by_design.svg"),
    plot = p1,
    width = 13,
    height = 6.4,
    device = grDevices::svg
  )

  summary_design <- obs[, .(median_dv_umol_l = median(dv, na.rm = TRUE)), by = .(design, time)]
  summary_design[, design := factor(as.character(design), levels = c("D1", "D2", "D3"))]
  p2 <- ggplot(obs, aes(x = time, y = dv, color = design)) +
    geom_point(alpha = 0.42, size = 1.7, position = position_jitter(width = 0.025, height = 0)) +
    geom_line(
      data = summary_design,
      aes(x = time, y = median_dv_umol_l, color = design, group = design),
      inherit.aes = FALSE,
      linewidth = 0.85
    ) +
    geom_line(
      data = typical_curve,
      aes(x = time, y = pred),
      inherit.aes = FALSE,
      linewidth = 0.7,
      color = "#202020"
    ) +
    scale_x_continuous(breaks = 0:6) +
    coord_cartesian(xlim = c(0, 6)) +
    scale_color_manual(values = design_cols, drop = FALSE) +
    labs(
      x = "Time after dose (h)",
      y = "Observed concentration (umol/L)",
      color = "Sampling design"
    ) +
    dv_time_theme(strip = FALSE)

  ggsave(
    filename = file.path(fig_dir, "dv_time_all_designs.png"),
    plot = p2,
    width = 11,
    height = 6.4,
    dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "dv_time_all_designs.pdf"),
    plot = p2,
    width = 11,
    height = 6.4
  )
  ggsave(
    filename = file.path(fig_dir, "dv_time_all_designs.svg"),
    plot = p2,
    width = 11,
    height = 6.4,
    device = grDevices::svg
  )
}

write_manifest <- function(dat, subjects, summaries) {
  manifest_dir <- ensure_dir(file.path(ROOT, "manifests"))
  manifest <- list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    source = list(
      pmcid = "PMC5306498",
      doi = "10.1111/bcp.13134",
      title = "Population pharmacokinetics and dosing recommendations for the use of deferiprone in children younger than 6 years of age",
      accepted_manuscript_url = "https://discovery.ucl.ac.uk/1516573/1/pasqua_bcp13134.pdf"
    ),
    design = list(
      dose_mgkg = MODEL$dose_mgkg,
      single_oral_dose_time_h = 0,
      sample_times_h = DESIGNS,
      design_counts = as.list(DESIGN_COUNTS),
      n_subjects = nrow(subjects)
    ),
    model = list(
      structure = "one-compartment oral, first-order absorption, first-order elimination",
      solver = list(
        package = "rxode2",
        model_builder = "rxode2::rxode2",
        ode_solver = "rxode2::rxSolve"
      ),
      units = list(dose = "mg", concentration_dv = "umol/L", concentration_dv_mg_l = "mg/L"),
      wt_ref_kg = MODEL$wt_ref_kg,
      weight_distribution = list(
        mean_kg = MODEL$wt_mean_kg,
        sd_kg = MODEL$wt_sd_kg,
        lower_kg = MODEL$wt_lower_kg,
        upper_kg = MODEL$wt_upper_kg
      ),
      tvcl_l_h = MODEL$tvcl_l_h,
      tvv_l = MODEL$tvv_l,
      tvka_h = MODEL$tvka_h,
      theta_wt_cl = MODEL$theta_wt_cl,
      theta_wt_v = MODEL$theta_wt_v,
      omega = unname(MODEL$omega),
      omega_rho_cl_v = MODEL$omega[1, 2] / sqrt(MODEL$omega[1, 1] * MODEL$omega[2, 2]),
      prop_var = MODEL$prop_var,
      prop_sd = MODEL$prop_sd,
      molecular_weight_g_mol = MODEL$mw_g_mol
    ),
    seeds = SEEDS,
    outputs = list(
      all_data = file.path(ROOT, "data", "all", "deferiprone_single_dose_25mgkg.csv"),
      split_base_dir = file.path(ROOT, "data", "base"),
      id_params = file.path(ROOT, "data", "meta", "id_params.csv"),
      design_time_check = file.path(ROOT, "checks", "design_time_check.csv"),
      dv_time_summary = file.path(ROOT, "reports", "tables", "dv_time_summary.csv"),
      figures = list(
        file.path(ROOT, "reports", "figures", "dv_time_by_design.png"),
        file.path(ROOT, "reports", "figures", "dv_time_all_designs.png")
      )
    )
  )
  write_json(
    manifest,
    file.path(manifest_dir, "generation_meta.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = 10
  )
}

main <- function() {
  ensure_dir(file.path(ROOT, "data", "all"))
  ensure_dir(file.path(ROOT, "data", "base"))
  ensure_dir(file.path(ROOT, "data", "meta"))
  ensure_dir(file.path(ROOT, "data", "scenario1", "base"))
  ensure_dir(file.path(ROOT, "data", "scenario1", "meta"))
  ensure_dir(file.path(ROOT, "data", "scenario2", "base"))
  ensure_dir(file.path(ROOT, "data", "scenario2", "meta"))
  ensure_dir(file.path(ROOT, "reports", "figures"))
  ensure_dir(file.path(ROOT, "reports", "tables"))
  ensure_dir(file.path(ROOT, "checks"))
  ensure_dir(file.path(ROOT, "manifests"))

  subjects <- make_subjects()
  dat <- make_dataset(subjects)

  expected_times <- unlist(DESIGNS, use.names = FALSE)
  actual_times <- dat[evid == 0L, sort(unique(time))]
  if (!all(sort(unique(expected_times)) == actual_times)) {
    stop("Observation time grid does not match the requested design.")
  }

  all_path <- file.path(ROOT, "data", "all", "deferiprone_single_dose_25mgkg.csv")
  fwrite(dat, all_path)
  write_split_files(dat)
  write_scenario_split_files(dat, subjects)
  summaries <- make_summaries(dat, subjects)
  plot_outputs(dat, summaries$summary)
  write_manifest(dat, subjects, summaries)

  message("Wrote: ", all_path)
  message("Wrote figures under: ", file.path(ROOT, "reports", "figures"))
}

main()
