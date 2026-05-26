#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grid)
})

REPO_ROOT <- normalizePath(getwd(), mustWork = TRUE)
ROOT <- Sys.getenv(
  "DEFERIPRONE_OUTPUT_ROOT",
  file.path(
    REPO_ROOT,
    "temp",
    "deferiprone_single_oral_25mgkg_designcheck_20260511",
    "validation",
    "design_change_n403030_wtmclustT10_nomix_site403030_nondesignquota_precisionchol_basefitbootinit_preboot_20260521"
  )
)
SCENARIOS <- strsplit(Sys.getenv("SCENARIOS", "scenario1 scenario2"), "\\s+")[[1]]
SCENARIOS <- SCENARIOS[nzchar(SCENARIOS)]
REVISION_NAME <- Sys.getenv("FIGURE3_REVISION_NAME", "scenario_rows_theta_other_v1")

FIG3_DIR <- file.path(ROOT, "reports", "main", "figure3")
OUT_DIR <- file.path(FIG3_DIR, "revisions", REVISION_NAME)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

map_path <- function(path) {
  if (!nzchar(path) || is.na(path)) return(path)
  if (dir.exists(path) || file.exists(path)) return(path)
  if (startsWith(path, "/project/")) {
    alt <- file.path(REPO_ROOT, sub("^/project/", "", path))
    if (dir.exists(alt) || file.exists(alt)) return(alt)
  }
  repo_rel <- sub("^.*/fed_PopPK/", "", path)
  if (!identical(repo_rel, path)) {
    alt <- file.path(REPO_ROOT, repo_rel)
    if (dir.exists(alt) || file.exists(alt)) return(alt)
  }
  path
}

latest_run_dir <- function(scenario) {
  latest_path <- file.path(ROOT, "runs", scenario, "standard", "federated", "latest_run.txt")
  if (!file.exists(latest_path)) stop("Missing latest_run.txt for ", scenario, call. = FALSE)
  run_dir <- map_path(trimws(readLines(latest_path, warn = FALSE)[1L]))
  if (!dir.exists(run_dir)) stop("Latest run directory does not exist: ", run_dir, call. = FALSE)
  run_dir
}

to_site_label <- function(client) {
  x <- as.character(client)
  out <- x
  out[grepl("client1|site1", x, ignore.case = TRUE)] <- "Site1"
  out[grepl("client2|site2", x, ignore.case = TRUE)] <- "Site2"
  out[grepl("client3|site3", x, ignore.case = TRUE)] <- "Site3"
  out
}

signed_log10 <- function(x) sign(x) * log10(1 + abs(x))
signed_log10_inv <- function(x) sign(x) * (10^abs(x) - 1)
signed_log10_trans <- function() {
  scales::trans_new(
    name = "signed_log10",
    transform = signed_log10,
    inverse = signed_log10_inv,
    domain = c(-Inf, Inf)
  )
}
signed_log10_breaks <- function(raw_limit) {
  if (!is.finite(raw_limit) || raw_limit <= 0) return(0)
  raw_powers <- 10^(0:ceiling(log10(raw_limit)))
  out <- unique(c(-rev(raw_powers), 0, raw_powers))
  out[out >= -raw_limit & out <= raw_limit]
}

read_grad_opt_scale <- function(scenario) {
  run_dir <- latest_run_dir(scenario)
  dt <- fread(file.path(run_dir, "iter_client_log.csv"))
  dt <- dt[is.finite(iter) & eval_type == "fn"]
  dt[, site := to_site_label(client)]

  grad_cols <- grep("^grad_", names(dt), value = TRUE)
  g <- melt(
    dt,
    id.vars = c("iter", "site"),
    measure.vars = grad_cols,
    variable.name = "grad_param",
    value.name = "grad_value"
  )
  g <- g[is.finite(grad_value)]
  g[, grad_param := sub("^grad_", "", as.character(grad_param))]

  agg <- g[, .(grad_value = sum(as.numeric(grad_value), na.rm = TRUE)), by = .(iter, grad_param)]
  agg[, site := "Aggregated"]
  out <- rbindlist(
    list(
      g[, .(iter, grad_param, site, grad_value)],
      agg[, .(iter, grad_param, site, grad_value)]
    ),
    use.names = TRUE
  )
  out[, Scenario := sub("^scenario", "Scenario ", scenario)]
  out[]
}

dt <- rbindlist(lapply(SCENARIOS, read_grad_opt_scale), use.names = TRUE)
dt[, Scenario := factor(Scenario, levels = sub("^scenario", "Scenario ", SCENARIOS))]
dt[, site := factor(site, levels = c("Site1", "Site2", "Site3", "Aggregated"))]

theta_params <- c("lka", "lcl", "lvc")
other_params <- c(
  "pchol__omega_cl_v__etaLcl__etaLcl",
  "pchol__omega_cl_v__etaLcl__etaLvc",
  "pchol__omega_cl_v__etaLvc__etaLvc",
  "chol__omega_cl_v__etaLcl__etaLcl",
  "chol__omega_cl_v__etaLvc__etaLcl",
  "chol__omega_cl_v__etaLvc__etaLvc",
  "CcPropSd"
)
theta_params <- theta_params[theta_params %in% unique(dt$grad_param)]
other_params <- other_params[other_params %in% unique(dt$grad_param)]

param_labels <- list(
  lka = expression(bold(theta[KA])),
  lcl = expression(bold(theta[CL])),
  lvc = expression(bold(theta[V])),
  pchol__omega_cl_v__etaLcl__etaLcl = expression(bold(omega[CL]^2)),
  pchol__omega_cl_v__etaLcl__etaLvc = expression(bold(omega[CL*","*V])),
  pchol__omega_cl_v__etaLvc__etaLvc = expression(bold(omega[V]^2)),
  chol__omega_cl_v__etaLcl__etaLcl = expression(bold(omega[CL]^2)),
  chol__omega_cl_v__etaLvc__etaLcl = expression(bold(omega[CL*","*V])),
  chol__omega_cl_v__etaLvc__etaLvc = expression(bold(omega[V]^2)),
  CcPropSd = expression(bold(sigma[prop]^2))
)

site_colors <- c(Site1 = "#F8766D", Site2 = "#00BA38", Site3 = "#619CFF", Aggregated = "black")
site_shapes <- c(Site1 = 16, Site2 = 17, Site3 = 15, Aggregated = NA)
site_widths <- c(Site1 = 0.62, Site2 = 0.62, Site3 = 0.62, Aggregated = 1.02)

plot_param <- function(scenario, param, show_y = TRUE, show_x = FALSE, show_legend = FALSE) {
  pdt <- copy(dt[Scenario == scenario & grad_param == param])
  raw_limit <- max(abs(pdt$grad_value), na.rm = TRUE)
  if (!is.finite(raw_limit) || raw_limit <= 0) raw_limit <- 1
  y_lab <- if (show_y) "Gradient" else NULL

  ggplot(pdt, aes(x = iter, y = grad_value, color = site, group = site)) +
    geom_hline(yintercept = 0, linewidth = 0.32, linetype = "dashed", color = "gray45") +
    geom_line(aes(linewidth = site), alpha = 0.94) +
    geom_point(
      data = pdt[site != "Aggregated"],
      aes(shape = site),
      size = 1.05,
      alpha = 0.84,
      stroke = 0.18
    ) +
    scale_color_manual(values = site_colors, breaks = names(site_colors), drop = FALSE, name = "Site") +
    scale_shape_manual(values = site_shapes, breaks = names(site_colors), drop = FALSE, guide = "none") +
    scale_linewidth_manual(values = site_widths, guide = "none") +
    guides(
      color = guide_legend(
        nrow = 1,
        override.aes = list(
          shape = unname(site_shapes[names(site_colors)]),
          linewidth = unname(site_widths[names(site_colors)]),
          alpha = rep(1, length(site_colors))
        )
      )
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 5),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      trans = signed_log10_trans(),
      breaks = signed_log10_breaks(raw_limit),
      labels = scales::label_number(big.mark = ",", accuracy = 1),
      limits = c(-raw_limit, raw_limit)
    ) +
    labs(x = if (show_x) "Iteration" else NULL, y = y_lab, title = param_labels[[param]]) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(size = 14.2, face = "bold", hjust = 0.5, margin = margin(b = 4)),
      axis.title = element_text(size = 12.5, face = "bold"),
      axis.text.x = element_text(size = if (show_x) 9.5 else 0, face = "bold"),
      axis.text.y = element_text(size = 9.5, face = "bold"),
      axis.ticks.x = element_line(linewidth = if (show_x) 0.4 else 0),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = 12.5, face = "bold"),
      legend.text = element_text(size = 11.5, face = "bold"),
      legend.key.width = grid::unit(1.4, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.23, colour = "#e5e5e5"),
      plot.margin = margin(3, 4, 3, 4)
    )
}

extract_legend <- function(p) {
  gt <- ggplotGrob(p + theme(legend.position = "bottom"))
  idx <- which(vapply(gt$grobs, function(x) x$name, character(1)) == "guide-box")
  if (!length(idx)) return(nullGrob())
  gt$grobs[[idx[1L]]]
}

place_grob <- function(grob, row, col_start, col_end) {
  pushViewport(viewport(layout.pos.row = row, layout.pos.col = col_start:col_end))
  grid.draw(grob)
  popViewport()
}

draw_onepage <- function() {
  scenario_labels <- levels(dt$Scenario)
  if (length(scenario_labels) != 2L) {
    stop("This layout expects exactly two scenarios.", call. = FALSE)
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(
    nrow = 6,
    ncol = 13,
    widths = grid::unit(c(0.25, rep(1, 12)), c("null", rep("null", 12))),
    heights = grid::unit(c(1, 1, 0.08, 1, 1, 0.14), c("null", "null", "null", "null", "null", "null"))
  )))

  row_specs <- list(
    list(row = 1L, scenario = scenario_labels[[1L]], params = theta_params, cols = list(c(2, 5), c(6, 9), c(10, 13)), show_x = FALSE),
    list(row = 2L, scenario = scenario_labels[[2L]], params = theta_params, cols = list(c(2, 5), c(6, 9), c(10, 13)), show_x = FALSE),
    list(row = 4L, scenario = scenario_labels[[1L]], params = other_params, cols = list(c(2, 4), c(5, 7), c(8, 10), c(11, 13)), show_x = FALSE),
    list(row = 5L, scenario = scenario_labels[[2L]], params = other_params, cols = list(c(2, 4), c(5, 7), c(8, 10), c(11, 13)), show_x = TRUE)
  )

  for (spec in row_specs) {
    pushViewport(viewport(layout.pos.row = spec$row, layout.pos.col = 1))
    grid.text(spec$scenario, rot = 90, gp = gpar(fontfamily = "serif", fontsize = 12.5, fontface = "bold"))
    popViewport()
    for (i in seq_along(spec$params)) {
      p <- plot_param(
        scenario = spec$scenario,
        param = spec$params[[i]],
        show_y = i == 1L,
        show_x = isTRUE(spec$show_x),
        show_legend = FALSE
      )
      place_grob(ggplotGrob(p), spec$row, spec$cols[[i]][1], spec$cols[[i]][2])
    }
  }

  legend <- extract_legend(plot_param(scenario_labels[[1L]], theta_params[[1L]], show_y = FALSE, show_x = TRUE, show_legend = TRUE))
  place_grob(legend, 6, 2, 13)
  popViewport()
}

stem <- "figure3_gradient_trajectories_scenario_rows_v1"
png(file.path(OUT_DIR, paste0(stem, ".png")), width = 17.5, height = 12.2, units = "in", res = 300)
draw_onepage()
dev.off()
pdf(file.path(OUT_DIR, paste0(stem, ".pdf")), width = 17.5, height = 12.2, useDingbats = FALSE)
draw_onepage()
dev.off()
svg(file.path(OUT_DIR, paste0(stem, ".svg")), width = 17.5, height = 12.2)
draw_onepage()
dev.off()

message("Wrote scenario-row Figure 3 revision to: ", OUT_DIR)
