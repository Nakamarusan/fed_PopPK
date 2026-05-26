#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(scales)
  library(jsonlite)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
SCRIPT_ROOT <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
ROOT <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", unset = SCRIPT_ROOT)
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)
repo_candidates <- c(Sys.getenv("REPO_ROOT", ""), "/project", normalizePath(dirname(dirname(SCRIPT_ROOT)), winslash = "/", mustWork = FALSE))
repo_candidates <- repo_candidates[nzchar(repo_candidates)]
REPO_ROOT <- repo_candidates[file.exists(file.path(repo_candidates, "R")) & file.exists(file.path(repo_candidates, "temp"))][[1L]]
REPO_HOST <- normalizePath("/project", winslash = "/", mustWork = FALSE)
REPO_CONTAINER <- "/project"
REPORTS_FIGURES <- file.path(ROOT, "reports", "figures")
dir.create(REPORTS_FIGURES, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x
parse_scenarios <- function(default = c("scenario1", "scenario2")) {
  raw <- trimws(Sys.getenv("SCENARIOS", ""))
  if (!nzchar(raw)) return(default)
  vals <- strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]
  vals[nzchar(vals)]
}
SCENARIOS <- parse_scenarios()

map_path <- function(path) {
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  if (startsWith(path, REPO_HOST)) {
    mapped <- sub(paste0("^", REPO_HOST), REPO_CONTAINER, path)
    if (file.exists(mapped)) return(normalizePath(mapped, winslash = "/", mustWork = FALSE))
  }
  if (startsWith(path, REPO_CONTAINER)) {
    mapped <- sub(paste0("^", REPO_CONTAINER), REPO_HOST, path)
    if (file.exists(mapped)) return(normalizePath(mapped, winslash = "/", mustWork = FALSE))
  }
  path
}

to_site_label <- function(x) {
  x <- as.character(x)
  out <- x
  out[grepl("client1|site1", x, ignore.case = TRUE)] <- "Site1"
  out[grepl("client2|site2", x, ignore.case = TRUE)] <- "Site2"
  out[grepl("client3|site3", x, ignore.case = TRUE)] <- "Site3"
  out
}

parse_bound_scalar <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- x[[1L]]
  if (is.character(x)) {
    s <- tolower(trimws(x))
    if (s %in% c("inf", "+inf", "infinity", "+infinity")) return(Inf)
    if (s %in% c("-inf", "-infinity")) return(-Inf)
  }
  val <- suppressWarnings(as.numeric(x))
  if (!length(val) || is.na(val)) return(default)
  val[[1L]]
}

resolve_transform <- function(lower, upper) {
  if (is.finite(lower) && is.finite(upper)) return("logit")
  if (is.finite(lower) && !is.finite(upper)) return("lower_exp")
  if (!is.finite(lower) && !is.finite(upper)) return("exp")
  "identity"
}

resolve_transform_for_name <- function(nm, lower, upper) {
  if (nm %in% c("lka", "lcl", "lvc")) return("identity")
  if (identical(nm, "CcPropSd")) {
    if (is.finite(lower) && !is.finite(upper)) return("lower_exp")
    return("exp")
  }
  if (startsWith(nm, "chol__")) {
    parts <- strsplit(nm, "__", fixed = TRUE)[[1L]]
    if (length(parts) >= 4L && identical(parts[[3L]], parts[[4L]])) return("exp")
    return("identity")
  }
  resolve_transform(lower, upper)
}

z_to_nat <- function(z, lower, upper, transform) {
  if (identical(transform, "lower_exp")) return(lower + exp(z))
  if (identical(transform, "exp")) return(exp(z))
  if (identical(transform, "logit")) return(lower + (upper - lower) * plogis(z))
  z
}

dzdp <- function(p, lower, upper, transform) {
  if (identical(transform, "lower_exp")) return(1 / pmax(p - lower, .Machine$double.eps))
  if (identical(transform, "exp")) return(1 / pmax(p, .Machine$double.eps))
  if (identical(transform, "logit")) {
    u <- (p - lower) / (upper - lower)
    u <- pmax(pmin(u, 1 - 1e-12), 1e-12)
    return(1 / ((upper - lower) * u * (1 - u)))
  }
  rep(1, length(p))
}

signed_log10 <- function(x) sign(x) * log10(1 + abs(x))
signed_log10_inv <- function(x) sign(x) * (10^abs(x) - 1)
signed_log10_trans <- function() scales::trans_new(name = "signed_log10", transform = signed_log10, inverse = signed_log10_inv, domain = c(-Inf, Inf))
signed_log10_breaks <- function(raw_limit) {
  lim <- suppressWarnings(as.numeric(raw_limit[[1L]]))
  if (!is.finite(lim) || lim <= 0) return(0)
  max_pow <- max(0L, ceiling(log10(lim)))
  step <- if (max_pow >= 4L) 2L else 1L
  start_pow <- if (max_pow >= 1L) 2L else 0L
  exponents <- unique(sort(c(seq.int(start_pow, max_pow, by = step), max_pow)))
  pos <- unique(c(0, 10^exponents))
  pos <- pos[pos <= lim]
  sort(unique(c(-rev(pos[pos > 0]), 0, pos[pos > 0])))
}

build_dzdp_long <- function(iter_log_csv, result_json, param_names) {
  iter_log <- fread(iter_log_csv)
  meta <- fromJSON(result_json, simplifyVector = FALSE)$meta %||% list()
  lower_raw <- meta$lower %||% list()
  upper_raw <- meta$upper %||% list()
  out <- data.table(iter = as.integer(iter_log$iter))
  for (nm in param_names) {
    z <- as.numeric(iter_log[[nm]])
    lo <- if (!is.null(lower_raw[[nm]])) parse_bound_scalar(lower_raw[[nm]], default = -Inf) else -Inf
    hi <- if (!is.null(upper_raw[[nm]])) parse_bound_scalar(upper_raw[[nm]], default = Inf) else Inf
    tr <- resolve_transform_for_name(nm, lo, hi)
    p <- z_to_nat(z, lo, hi, tr)
    jac <- dzdp(p, lo, hi, tr)
    if (identical(nm, "CcPropSd")) {
      jac <- jac / (2 * pmax(p, .Machine$double.eps))
    }
    out[[paste0("dzdp_", nm)]] <- jac
  }
  long <- melt(out, id.vars = "iter", measure.vars = setdiff(names(out), "iter"), variable.name = "grad_param", value.name = "dzdp")
  long[, grad_param := sub("^dzdp_", "", grad_param)]
  long[]
}

parse_chol_name <- function(nm) {
  parts <- strsplit(nm, "__", fixed = TRUE)[[1L]]
  if (length(parts) < 4L || !identical(parts[[1L]], "chol")) return(NULL)
  list(block = parts[[2L]], row = parts[[3L]], col = parts[[4L]])
}

chol_outputs_from_z <- function(z_vec, parsed) {
  rows <- vapply(parsed, `[[`, character(1), "row")
  cols <- vapply(parsed, `[[`, character(1), "col")
  etas <- unique(c(cols, rows))
  diag_rows <- rows[rows == cols]
  if (length(diag_rows)) etas <- unique(c(diag_rows, etas))
  k <- length(etas)
  L <- matrix(0, nrow = k, ncol = k, dimnames = list(etas, etas))
  for (nm in names(z_vec)) {
    p <- parsed[[nm]]
    val <- as.numeric(z_vec[[nm]])
    L[p$row, p$col] <- if (identical(p$row, p$col)) exp(val) else val
  }
  omega <- L %*% t(L)
  vals <- numeric()
  nms <- character()
  for (eta in etas) {
    vals <- c(vals, omega[eta, eta])
    nms <- c(nms, eta)
  }
  if (k >= 2L) {
    for (i in 2:k) {
      for (j in seq_len(i - 1L)) {
        vals <- c(vals, omega[etas[[i]], etas[[j]]])
        nms <- c(nms, sprintf("(%s,%s)", etas[[j]], etas[[i]]))
      }
    }
  }
  names(vals) <- nms
  vals
}

chol_grad_to_omega <- function(z_vec, gz_vec, parsed) {
  y0 <- chol_outputs_from_z(z_vec, parsed)
  J <- matrix(NA_real_, nrow = length(y0), ncol = length(z_vec), dimnames = list(names(y0), names(z_vec)))
  for (nm in names(z_vec)) {
    eps <- 1e-6 * max(1, abs(as.numeric(z_vec[[nm]])))
    z_plus <- z_vec
    z_minus <- z_vec
    z_plus[[nm]] <- z_plus[[nm]] + eps
    z_minus[[nm]] <- z_minus[[nm]] - eps
    J[, nm] <- (chol_outputs_from_z(z_plus, parsed) - chol_outputs_from_z(z_minus, parsed)) / (2 * eps)
  }
  out <- tryCatch(
    solve(t(J), as.numeric(gz_vec)),
    error = function(e) rep(NA_real_, nrow(J))
  )
  names(out) <- rownames(J)
  out
}

transform_chol_gradients <- function(g_chol, iter_log) {
  if (!nrow(g_chol)) return(data.table(iter = integer(), grad_param = character(), site = character(), grad_value = numeric()))
  parsed_all <- lapply(unique(g_chol$grad_param), parse_chol_name)
  names(parsed_all) <- unique(g_chol$grad_param)
  parsed_all <- parsed_all[!vapply(parsed_all, is.null, logical(1))]
  if (!length(parsed_all)) return(data.table(iter = integer(), grad_param = character(), site = character(), grad_value = numeric()))
  block_by_param <- vapply(parsed_all, `[[`, character(1), "block")
  blocks <- split(names(block_by_param), block_by_param)

  out <- list()
  for (block_name in names(blocks)) {
    pnames <- blocks[[block_name]]
    parsed <- parsed_all[pnames]
    rows <- split(g_chol[grad_param %in% pnames], by = c("iter", "site"), keep.by = TRUE, drop = TRUE)
    for (one in rows) {
      iter_val <- one$iter[[1L]]
      z_row <- iter_log[iter == iter_val]
      if (!nrow(z_row) || !all(pnames %in% names(z_row))) next
      gz <- setNames(rep(0, length(pnames)), pnames)
      gz[one$grad_param] <- as.numeric(one$grad_value_z)
      z_vec <- setNames(as.numeric(z_row[1, ..pnames]), pnames)
      gy <- chol_grad_to_omega(z_vec, gz, parsed)
      out[[length(out) + 1L]] <- data.table(
        iter = iter_val,
        grad_param = names(gy),
        site = one$site[[1L]],
        grad_value = as.numeric(gy)
      )
    }
  }
  if (!length(out)) return(data.table(iter = integer(), grad_param = character(), site = character(), grad_value = numeric()))
  rbindlist(out, use.names = TRUE)
}

read_grad <- function(log_csv, iter_log_csv, result_json) {
  dt <- fread(log_csv)
  iter_log <- fread(iter_log_csv)
  dt <- dt[is.finite(iter) & eval_type == "fn"]
  dt[, site := to_site_label(client)]
  grad_cols <- grep("^grad_", names(dt), value = TRUE)
  g <- melt(dt, id.vars = c("iter", "site"), measure.vars = grad_cols, variable.name = "grad_param", value.name = "grad_value_z")
  g <- g[is.finite(grad_value_z)]
  g[, grad_param := sub("^grad_", "", grad_param)]
  direct <- g[!startsWith(as.character(grad_param), "chol__")]
  omega <- transform_chol_gradients(g[startsWith(as.character(grad_param), "chol__")], iter_log)
  if (nrow(direct)) {
    dz <- build_dzdp_long(iter_log_csv, result_json, unique(direct$grad_param))
    direct <- merge(direct, dz, by = c("iter", "grad_param"), all.x = TRUE, sort = FALSE)
    direct[is.na(dzdp), dzdp := 1]
    direct[, grad_value := as.numeric(grad_value_z) * as.numeric(dzdp)]
    direct <- direct[, .(iter, grad_param, site, grad_value)]
  } else {
    direct <- data.table(iter = integer(), grad_param = character(), site = character(), grad_value = numeric())
  }
  g_report <- rbindlist(list(direct, omega), use.names = TRUE, fill = TRUE)
  agg <- g_report[, .(grad_value = sum(as.numeric(grad_value), na.rm = TRUE)), by = .(iter, grad_param)]
  agg[, site := "Aggregated"]
  out <- rbindlist(list(g_report, agg[, .(iter, grad_param, site, grad_value)]), use.names = TRUE)
  param_order <- c("lka", "lcl", "lvc", "etaLcl", "(etaLcl,etaLvc)", "etaLvc", "CcPropSd")
  out[, grad_param := factor(grad_param, levels = param_order)]
  out[]
}

read_grad_opt_scale <- function(log_csv) {
  dt <- fread(log_csv)
  dt <- dt[is.finite(iter) & eval_type == "fn"]
  dt[, site := to_site_label(client)]
  grad_cols <- grep("^grad_", names(dt), value = TRUE)
  g <- melt(dt, id.vars = c("iter", "site"), measure.vars = grad_cols, variable.name = "grad_param", value.name = "grad_value")
  g <- g[is.finite(grad_value)]
  g[, grad_param := sub("^grad_", "", grad_param)]
  agg <- g[, .(grad_value = sum(as.numeric(grad_value), na.rm = TRUE)), by = .(iter, grad_param)]
  agg[, site := "Aggregated"]
  out <- rbindlist(list(g[, .(iter, grad_param, site, grad_value)], agg[, .(iter, grad_param, site, grad_value)]), use.names = TRUE)
  omega_params <- sort(unique(as.character(out$grad_param)[
    startsWith(as.character(out$grad_param), "chol__") |
      startsWith(as.character(out$grad_param), "pchol__")
  ]))
  out[, grad_param := factor(grad_param, levels = c("lka", "lcl", "lvc", omega_params, "CcPropSd"))]
  out[]
}

save_figure1 <- function() {
  src_dir <- file.path(REPO_ROOT, "temp", "reanalysis_infusion_3point_peakmidtrough_rxsolvefixed_init0p3_pgtol0_20260423", "reports", "figures")
  file.copy(file.path(src_dir, "figure1_federated_framework_sequence_diagram.pdf"), file.path(REPORTS_FIGURES, "figure1_federated_framework_sequence_diagram.pdf"), overwrite = TRUE)
  file.copy(file.path(src_dir, "figure1_federated_framework_sequence_diagram.svg"), file.path(REPORTS_FIGURES, "figure1_federated_framework_sequence_diagram.svg"), overwrite = TRUE)
  file.copy(file.path(src_dir, "figure1_federated_framework_sequence_diagram_notes.md"), file.path(REPORTS_FIGURES, "figure1_federated_framework_sequence_diagram_notes.md"), overwrite = TRUE)
}

save_figure2 <- function() {
  read_ids <- function(scenario) {
    path <- file.path(ROOT, "data", scenario, "meta", "id_params.csv")
    if (!file.exists(path)) path <- file.path(ROOT, "data", "meta", "id_params.csv")
    dt <- fread(path)
    dt[, Scenario := sub("^scenario", "Scenario ", scenario)]
    dt
  }
  ids <- rbindlist(lapply(SCENARIOS, read_ids), use.names = TRUE)
  plot_dt <- melt(ids[, .(Scenario, Site, CL = cl, V = v)], id.vars = c("Scenario", "Site"), variable.name = "Parameter", value.name = "Value")
  plot_dt[, Scenario := factor(Scenario, levels = c("Scenario 1", "Scenario 2"))]
  plot_dt[, Site := factor(Site, levels = c("Site1", "Site2", "Site3"))]
  plot_dt[, Parameter := factor(Parameter, levels = c("CL", "V"))]
  p <- ggplot(plot_dt, aes(x = Site, y = Value, fill = Site)) +
    geom_boxplot(width = 0.62, outlier.shape = NA, alpha = 0.72, linewidth = 0.45) +
    geom_jitter(aes(color = Site), width = 0.12, height = 0, size = 1.2, alpha = 0.45, show.legend = FALSE) +
    facet_grid(Scenario ~ Parameter, scales = "free_y") +
    scale_fill_manual(values = c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF"), guide = "none") +
    scale_color_manual(values = c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF"), guide = "none") +
    labs(x = "Site", y = "Individual parameter value") +
    theme_bw(base_family = "serif") +
    theme(
      strip.background = element_rect(fill = "#f3f3f3", colour = "#222222"),
      strip.text = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 12, 8, 8)
    )
  ggsave(file.path(REPORTS_FIGURES, "figure2_sitewise_cl_v_distributions.png"), p, width = 11, height = 8.2, dpi = 300)
  ggsave(file.path(REPORTS_FIGURES, "figure2_sitewise_cl_v_distributions.pdf"), p, width = 11, height = 8.2, device = "pdf")
  ggsave(file.path(REPORTS_FIGURES, "figure2_sitewise_cl_v_distributions.svg"), p, width = 11, height = 8.2, device = grDevices::svg)
}

save_figure2b_weight <- function() {
  read_ids <- function(scenario) {
    path <- file.path(ROOT, "data", scenario, "meta", "id_params.csv")
    if (!file.exists(path)) path <- file.path(ROOT, "data", "meta", "id_params.csv")
    dt <- fread(path)
    dt[, Scenario := sub("^scenario", "Scenario ", scenario)]
    dt
  }
  ids <- rbindlist(lapply(SCENARIOS, read_ids), use.names = TRUE)
  ids[, Scenario := factor(Scenario, levels = c("Scenario 1", "Scenario 2"))]
  ids[, Site := factor(Site, levels = c("Site1", "Site2", "Site3"))]

  wt_summary <- ids[, .(
    n_subjects = .N,
    min_wt_kg = min(WT, na.rm = TRUE),
    median_wt_kg = median(WT, na.rm = TRUE),
    max_wt_kg = max(WT, na.rm = TRUE)
  ), by = .(Scenario, Site)][order(Scenario, Site)]
  dir.create(file.path(ROOT, "reports", "tables"), recursive = TRUE, showWarnings = FALSE)
  fwrite(wt_summary, file.path(ROOT, "reports", "tables", "sitewise_weight_distribution_summary.csv"))

  p <- ggplot(ids, aes(x = Site, y = WT, fill = Site)) +
    geom_boxplot(width = 0.62, outlier.shape = NA, alpha = 0.72, linewidth = 0.45) +
    geom_jitter(aes(color = Site), width = 0.12, height = 0, size = 1.2, alpha = 0.45, show.legend = FALSE) +
    facet_grid(Scenario ~ ., scales = "fixed") +
    scale_fill_manual(values = c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF"), guide = "none") +
    scale_color_manual(values = c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF"), guide = "none") +
    labs(x = "Site", y = "Body weight (kg)") +
    theme_bw(base_family = "serif") +
    theme(
      strip.background = element_rect(fill = "#f3f3f3", colour = "#222222"),
      strip.text = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 12, 8, 8)
    )
  ggsave(file.path(REPORTS_FIGURES, "figure2b_sitewise_wt_distributions.png"), p, width = 8.2, height = 8.2, dpi = 300)
  ggsave(file.path(REPORTS_FIGURES, "figure2b_sitewise_wt_distributions.pdf"), p, width = 8.2, height = 8.2, device = "pdf")
  ggsave(file.path(REPORTS_FIGURES, "figure2b_sitewise_wt_distributions.svg"), p, width = 8.2, height = 8.2, device = grDevices::svg)
}

save_figure2_cl_v_wt <- function() {
  read_ids <- function(scenario) {
    path <- file.path(ROOT, "data", scenario, "meta", "id_params.csv")
    if (!file.exists(path)) path <- file.path(ROOT, "data", "meta", "id_params.csv")
    dt <- fread(path)
    dt[, Scenario := sub("^scenario", "Scenario ", scenario)]
    dt
  }
  ids <- rbindlist(lapply(SCENARIOS, read_ids), use.names = TRUE)
  plot_dt <- melt(
    ids[, .(`CL (L/h)` = cl, `V (L)` = v, `Body weight (kg)` = WT, Scenario, Site)],
    id.vars = c("Scenario", "Site"),
    variable.name = "Parameter",
    value.name = "Value"
  )
  plot_dt[, Scenario := factor(Scenario, levels = c("Scenario 1", "Scenario 2"))]
  plot_dt[, Site := factor(Site, levels = c("Site1", "Site2", "Site3"))]
  plot_dt[, Parameter := factor(Parameter, levels = c("CL (L/h)", "V (L)", "Body weight (kg)"))]
  p <- ggplot(plot_dt, aes(x = Site, y = Value, fill = Site)) +
    geom_boxplot(width = 0.62, outlier.shape = NA, alpha = 0.72, linewidth = 0.45) +
    geom_jitter(aes(color = Site), width = 0.12, height = 0, size = 1.2, alpha = 0.45, show.legend = FALSE) +
    facet_grid(Scenario ~ Parameter, scales = "free_y") +
    scale_fill_manual(values = c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF"), guide = "none") +
    scale_color_manual(values = c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF"), guide = "none") +
    labs(x = "Site", y = "Individual value") +
    theme_bw(base_family = "serif") +
    theme(
      strip.background = element_rect(fill = "#f3f3f3", colour = "#222222"),
      strip.text = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 12, 8, 8)
    )
  ggsave(file.path(REPORTS_FIGURES, "figure2_sitewise_cl_v_wt_distributions.png"), p, width = 13.2, height = 8.2, dpi = 300)
  ggsave(file.path(REPORTS_FIGURES, "figure2_sitewise_cl_v_wt_distributions.pdf"), p, width = 13.2, height = 8.2, device = "pdf")
  ggsave(file.path(REPORTS_FIGURES, "figure2_sitewise_cl_v_wt_distributions.svg"), p, width = 13.2, height = 8.2, device = grDevices::svg)
}

latest_run_dir <- function(scenario = SCENARIOS[[1L]]) {
  latest_path <- file.path(ROOT, "runs", scenario, "standard", "federated", "latest_run.txt")
  if (!file.exists(latest_path)) return(NA_character_)
  map_path(trimws(readLines(latest_path, warn = FALSE)[1L]))
}

has_federated_latest <- function() {
  any(vapply(SCENARIOS, function(scenario) {
    run_dir <- latest_run_dir(scenario)
    is.character(run_dir) && length(run_dir) == 1L && nzchar(run_dir) && !is.na(run_dir) && dir.exists(run_dir)
  }, logical(1)))
}

read_grad_scenarios <- function(scale = c("reporting", "optimization")) {
  scale <- match.arg(scale)
  parts <- lapply(SCENARIOS, function(scenario) {
    run_dir <- latest_run_dir(scenario)
    if (!is.character(run_dir) || length(run_dir) != 1L || !nzchar(run_dir) || is.na(run_dir) || !dir.exists(run_dir)) return(NULL)
    dt <- if (identical(scale, "reporting")) {
      read_grad(file.path(run_dir, "iter_client_log.csv"), file.path(run_dir, "iter_log.csv"), file.path(run_dir, "result.json"))
    } else {
      read_grad_opt_scale(file.path(run_dir, "iter_client_log.csv"))
    }
    dt[, Scenario := sub("^scenario", "Scenario ", scenario)]
    dt
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(data.table())
  out <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  out[, Scenario := factor(Scenario, levels = sub("^scenario", "Scenario ", SCENARIOS))]
  out[]
}

gradient_panel <- function(plot_dt, facet_lab, title = NULL, y_lab = "Gradient") {
  site_levels <- c("Site1", "Site2", "Site3", "Aggregated")
  site_colors <- c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF", Aggregated = "black")
  site_shapes <- c(Site1 = 16, Site2 = 17, Site3 = 15, Aggregated = NA)
  plot_dt <- copy(plot_dt)
  plot_dt[, site := factor(site, levels = site_levels)]
  plot_dt[, draw_order := factor(as.character(site), levels = c("Aggregated", "Site3", "Site2", "Site1"))]
  setorder(plot_dt, draw_order, iter)
  raw_limit <- max(abs(plot_dt$grad_value), na.rm = TRUE)
  if (!is.finite(raw_limit) || raw_limit <= 0) raw_limit <- 1
  facet_layer <- if ("Scenario" %in% names(plot_dt)) {
    facet_grid(Scenario ~ grad_param, scales = "fixed", labeller = labeller(grad_param = as_labeller(facet_lab, label_parsed)))
  } else {
    facet_wrap(~grad_param, ncol = 3, scales = "fixed", labeller = as_labeller(facet_lab, label_parsed))
  }
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", color = "gray40") +
    geom_line(data = plot_dt, aes(x = iter, y = grad_value, color = site, group = site), linewidth = 0.65, alpha = 0.92) +
    geom_point(data = plot_dt[site != "Aggregated"], aes(x = iter, y = grad_value, color = site, shape = site), size = 1.7, alpha = 0.88) +
    facet_layer +
    scale_color_manual(values = site_colors, breaks = site_levels, drop = FALSE, name = "Site") +
    scale_shape_manual(values = site_shapes, breaks = site_levels, drop = FALSE, guide = "none") +
    guides(color = guide_legend(
      override.aes = list(
        shape = unname(site_shapes[site_levels]),
        linetype = rep(1, length(site_levels)),
        linewidth = rep(0.8, length(site_levels)),
        alpha = rep(1, length(site_levels))
      )
    )) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 7), expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(
      trans = signed_log10_trans(),
      breaks = signed_log10_breaks(raw_limit),
      labels = scales::label_number(big.mark = ",", accuracy = 1),
      limits = c(-raw_limit, raw_limit)
    ) +
    labs(x = "Iteration", y = y_lab, title = title) +
    theme_bw(base_family = "serif") +
    theme(
      strip.text = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 13, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 13, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.margin = margin(8, 12, 8, 8)
    )
}

save_figure3 <- function() {
  dt <- read_grad_scenarios("optimization")
  if (!nrow(dt)) return(invisible(NULL))
  facet_lab <- c(
    lka = "bold(theta[KA])",
    lcl = "bold(theta[CL])",
    lvc = "bold(theta[V])",
    chol__omega_cl_v__etaLcl__etaLcl = "bold(omega[CL]^2)",
    chol__omega_cl_v__etaLvc__etaLcl = "bold(omega[CL*','*V])",
    chol__omega_cl_v__etaLvc__etaLvc = "bold(omega[V]^2)",
    pchol__omega_cl_v__etaLcl__etaLcl = "bold(omega[CL]^2)",
    pchol__omega_cl_v__etaLcl__etaLvc = "bold(omega[CL*','*V])",
    pchol__omega_cl_v__etaLvc__etaLvc = "bold(omega[V]^2)",
    CcPropSd = "bold(sigma[prop]^2)"
  )
  p <- gradient_panel(dt, facet_lab, NULL, "Gradient")
  ggsave(file.path(REPORTS_FIGURES, "figure3_gradient_trajectories.png"), p, width = 22, height = 9.6, dpi = 300)
  ggsave(file.path(REPORTS_FIGURES, "figure3_gradient_trajectories.pdf"), p, width = 22, height = 9.6, device = "pdf")
  ggsave(file.path(REPORTS_FIGURES, "figure3_gradient_trajectories.svg"), p, width = 22, height = 9.6, device = grDevices::svg)
}

save_figure3_theta_v_detail <- function() {
  run_dir <- latest_run_dir()
  plot_dt <- read_grad(file.path(run_dir, "iter_client_log.csv"), file.path(run_dir, "iter_log.csv"), file.path(run_dir, "result.json"))
  plot_dt <- plot_dt[grad_param == "lvc"]
  plot_dt[, site := factor(site, levels = c("Site1", "Site2", "Site3", "Aggregated"))]
  setorder(plot_dt, site, iter)
  p <- ggplot(plot_dt, aes(x = iter, y = grad_value, color = site, group = site)) +
    geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", color = "gray40") +
    geom_line(linewidth = 0.8, alpha = 0.95) +
    geom_point(data = plot_dt[site != "Aggregated"], aes(shape = site), size = 2.0, alpha = 0.9) +
    scale_color_manual(values = c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF", Aggregated = "black"), name = "Site") +
    scale_shape_manual(values = c(Site1 = 16, Site2 = 17, Site3 = 15, Aggregated = NA), guide = "none") +
    guides(color = guide_legend(
      override.aes = list(
        shape = c(16, 17, 15, NA),
        linetype = rep(1, 4),
        linewidth = rep(0.8, 4),
        alpha = rep(1, 4)
      )
    )) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 7), expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
    labs(x = "Iteration", y = expression(paste("Gradient for ", theta[V]))) +
    theme_bw(base_family = "serif") +
    theme(
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 14, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 12, 10, 10)
    )
  ggsave(file.path(REPORTS_FIGURES, "figure3_theta_v_gradient_detail.png"), p, width = 12, height = 7, dpi = 300)
  ggsave(file.path(REPORTS_FIGURES, "figure3_theta_v_gradient_detail.pdf"), p, width = 12, height = 7, device = "pdf")
  ggsave(file.path(REPORTS_FIGURES, "figure3_theta_v_gradient_detail.svg"), p, width = 12, height = 7, device = grDevices::svg)
}

save_supplement3_optimization_scale <- function() {
  dt <- read_grad_scenarios("optimization")
  if (!nrow(dt)) return(invisible(NULL))
  facet_lab <- c(
    lka = "bold(theta[KA])",
    lcl = "bold(theta[CL])",
    lvc = "bold(theta[V])",
    chol__omega_cl_v__etaLcl__etaLcl = "bold(omega[CL]^2)",
    chol__omega_cl_v__etaLvc__etaLcl = "bold(omega[CL*','*V])",
    chol__omega_cl_v__etaLvc__etaLvc = "bold(omega[V]^2)",
    pchol__omega_cl_v__etaLcl__etaLcl = "bold(omega[CL]^2)",
    pchol__omega_cl_v__etaLcl__etaLvc = "bold(omega[CL*','*V])",
    pchol__omega_cl_v__etaLvc__etaLvc = "bold(omega[V]^2)",
    CcPropSd = "bold(sigma[prop]^2)"
  )
  p <- gradient_panel(dt, facet_lab, NULL, "Gradient")
  ggsave(file.path(REPORTS_FIGURES, "supplement3_optimization_scale_gradient_trajectories.png"), p, width = 22, height = 9.6, dpi = 300)
  ggsave(file.path(REPORTS_FIGURES, "supplement3_optimization_scale_gradient_trajectories.pdf"), p, width = 22, height = 9.6, device = "pdf")
  ggsave(file.path(REPORTS_FIGURES, "supplement3_optimization_scale_gradient_trajectories.svg"), p, width = 22, height = 9.6, device = grDevices::svg)
}

read_base_obs <- function() {
  parts <- lapply(1:3, function(i) {
    path <- file.path(ROOT, "data", "base", sprintf("data1_%d.csv", i))
    dt <- fread(path)
    dt[, time := as.numeric(time)]
    dt[, dv := as.numeric(dv)]
    dt[, evid := as.integer(evid)]
    dt[evid == 0L & is.finite(time) & is.finite(dv), .(Site, design, id, time, dv)]
  })
  out <- rbindlist(parts, use.names = TRUE)
  out[, Site := factor(as.character(Site), levels = c("Site1", "Site2", "Site3"))]
  out[, design := factor(as.character(design), levels = c("D1", "D2", "D3"))]
  out[]
}

save_concentration_profile <- function(stem, x_label = "Time after dose (h)") {
  plot_dt <- read_base_obs()
  design_cols <- c(D1 = "#F8766D", D2 = "#00BA38", D3 = "#619CFF")
  p <- ggplot(plot_dt, aes(x = time, y = dv, color = design)) +
    geom_line(aes(group = interaction(Site, id)), linewidth = 0.4, alpha = 0.35) +
    geom_point(position = position_jitter(width = 0.01, height = 0), size = 1.8, alpha = 0.75) +
    facet_grid(. ~ Site, scales = "fixed") +
    scale_color_manual(values = design_cols, name = "Design", drop = FALSE) +
    scale_x_continuous(breaks = sort(unique(plot_dt$time)), expand = expansion(mult = c(0.03, 0.05)), guide = guide_axis(angle = 45)) +
    labs(x = x_label, y = "Observed concentration (umol/L)") +
    theme_bw(base_family = "serif") +
    theme(
      strip.background = element_rect(fill = "#f3f3f3", colour = "#222222"),
      strip.text = element_text(face = "bold", size = 17),
      panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13, face = "bold"),
      axis.title = element_text(size = 17, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
      plot.margin = margin(8, 12, 8, 8)
    )
  ggsave(file.path(REPORTS_FIGURES, paste0(stem, ".png")), p, width = 13, height = 6.4, dpi = 300)
  ggsave(file.path(REPORTS_FIGURES, paste0(stem, ".pdf")), p, width = 13, height = 6.4, device = "pdf")
  ggsave(file.path(REPORTS_FIGURES, paste0(stem, ".svg")), p, width = 13, height = 6.4, device = grDevices::svg)
}

save_figure1()
save_figure2()
save_figure2b_weight()
save_figure2_cl_v_wt()
if (has_federated_latest()) {
  save_figure3()
  save_figure3_theta_v_detail()
  save_supplement3_optimization_scale()
} else {
  message("Skipping federated gradient figures because no standard federated latest_run.txt was found.")
}
save_concentration_profile("supplement1")
save_concentration_profile("time_concentration_profiles")
