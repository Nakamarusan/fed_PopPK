#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(jsonlite)
  library(rxode2)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
SCRIPT_ROOT <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
ROOT <- Sys.getenv("DEFERIPRONE_OUTPUT_ROOT", unset = SCRIPT_ROOT)
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)
FIG_DIR <- file.path(ROOT, "reports", "figures")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
DESIGN_COLORS <- c(D1 = "#F8766D", D2 = "#00BA38", D3 = "#619CFF")

`%||%` <- function(x, y) if (is.null(x)) y else x

parse_scenarios <- function(default = c("scenario1", "scenario2")) {
  raw <- trimws(Sys.getenv("SCENARIOS", ""))
  if (!nzchar(raw)) return(default)
  vals <- strsplit(raw, "[,[:space:]]+", perl = TRUE)[[1L]]
  vals[nzchar(vals)]
}

scalar <- function(x, default = NA_real_) {
  if (is.null(x)) return(default)
  val <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)[[1L]]))
  if (!is.finite(val)) default else val
}

read_manifest <- function() {
  path <- file.path(ROOT, "manifests", "generation_meta.json")
  if (!file.exists(path)) stop("Missing manifest: ", path)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

read_scenario_data <- function(scenario) {
  base_dir <- file.path(ROOT, "data", scenario, "base")
  files <- file.path(base_dir, sprintf("data1_%d.csv", 1:3))
  if (!all(file.exists(files))) {
    stop("Missing scenario data files under: ", base_dir)
  }
  dat <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)
  dat[, scenario := scenario]
  dat[]
}

make_rxode_model <- function() {
  rxode2::rxode2({
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - (cl / v) * central
    cp = central / v
  })
}

solve_typical_curve <- function(model_meta, xmax) {
  wt_ref_kg <- scalar(model_meta$wt_ref_kg, 15.8)
  dose_mgkg <- scalar(model_meta$dose_mgkg, 25)
  subject <- data.frame(
    id = 1L,
    cl = scalar(model_meta$tvcl_l_h, 8.3),
    v = scalar(model_meta$tvv_l, 18.7),
    ka = scalar(model_meta$tvka_h, 9.13)
  )
  events <- rbind(
    data.frame(id = 1L, time = 0, evid = 1L, cmt = "depot", amt = dose_mgkg * wt_ref_kg),
    data.frame(id = 1L, time = seq(0, xmax, by = 0.02), evid = 0L, cmt = "central", amt = NA_real_)
  )
  sim <- as.data.table(rxode2::rxSolve(
    make_rxode_model(),
    params = subject,
    events = events,
    returnType = "data.table",
    cores = 1
  ))
  mw <- scalar(model_meta$molecular_weight_g_mol, 139.15)
  sim[, .(time = as.numeric(time), pred = as.numeric(cp) * 1000 / mw)]
}

save_plot_formats <- function(plot, stem, width, height) {
  ggsave(paste0(stem, ".png"), plot = plot, width = width, height = height, dpi = 300)
  ggsave(paste0(stem, ".pdf"), plot = plot, width = width, height = height)
  ggsave(paste0(stem, ".svg"), plot = plot, width = width, height = height, device = grDevices::svg)
}

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

plot_all_designs <- function(dat, manifest) {
  obs <- dat[evid == 0L]
  if (!nrow(obs)) stop("No observation rows found for all-design DV-time plot")
  obs[, design := factor(as.character(design), levels = c("D1", "D2", "D3"))]
  obs[, Site := factor(as.character(Site), levels = c("Site1", "Site2", "Site3"))]

  xmax <- max(6, ceiling(max(obs$time, na.rm = TRUE)))
  typical_curve <- solve_typical_curve(manifest$model %||% list(), xmax)
  summary_dt <- obs[, .(median_dv = median(dv, na.rm = TRUE)), by = .(time, design)]

  p <- ggplot(obs, aes(x = time, y = dv, color = design)) +
    geom_point(alpha = 0.42, size = 1.7, position = position_jitter(width = 0.025, height = 0)) +
    geom_line(
      data = summary_dt,
      aes(x = time, y = median_dv, color = design, group = design),
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
    scale_x_continuous(breaks = seq(0, xmax, by = 1)) +
    coord_cartesian(xlim = c(0, xmax)) +
    scale_color_manual(values = DESIGN_COLORS, drop = FALSE) +
    labs(
      x = "Time after dose (h)",
      y = "Observed concentration (umol/L)",
      color = "Sampling design"
    ) +
    dv_time_theme(strip = FALSE)

  stems <- c(
    file.path(FIG_DIR, "dv_time_all_designs"),
    file.path(FIG_DIR, "figure4_dv_time_all_designs")
  )
  for (stem in stems) {
    save_plot_formats(p, stem, width = 11, height = 6.4)
  }
  data.table(
    figure = "Figure 4",
    plot = "dv_time_all_designs",
    png = paste0(stems[[1L]], ".png"),
    svg = paste0(stems[[1L]], ".svg")
  )
}

plot_one_scenario <- function(dat, manifest, scenario) {
  obs <- dat[evid == 0L]
  if (!nrow(obs)) stop("No observation rows found for ", scenario)
  obs[, design := factor(as.character(design), levels = c("D1", "D2", "D3"))]
  obs[, Site := factor(as.character(Site), levels = c("Site1", "Site2", "Site3"))]

  xmax <- max(6, ceiling(max(obs$time, na.rm = TRUE)))
  typical_curve <- solve_typical_curve(manifest$model %||% list(), xmax)

  facet_var <- "Site"
  color_var <- "design"
  legend_title <- "Sampling design"

  p <- ggplot(obs, aes(x = time, y = dv, group = id, color = .data[[color_var]])) +
    geom_line(alpha = 0.16, linewidth = 0.25) +
    geom_point(alpha = 0.62, size = 1.7) +
    geom_line(
      data = typical_curve,
      aes(x = time, y = pred),
      inherit.aes = FALSE,
      linewidth = 0.7,
      color = "#202020"
    ) +
    facet_wrap(as.formula(paste("~", facet_var)), nrow = 1) +
    scale_x_continuous(breaks = seq(0, xmax, by = 1)) +
    scale_color_manual(values = DESIGN_COLORS, drop = FALSE) +
    coord_cartesian(xlim = c(0, xmax)) +
    labs(
      x = "Time after dose (h)",
      y = "Observed concentration (umol/L)",
      color = legend_title
    ) +
    dv_time_theme(strip = TRUE)

  scenario_dir <- file.path(FIG_DIR, scenario)
  dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)

  out_stems <- c(
    file.path(FIG_DIR, sprintf("%s_dv_time_by_design", scenario)),
    file.path(scenario_dir, "dv_time_by_design")
  )
  figure_stem <- if (identical(scenario, "scenario1")) {
    file.path(FIG_DIR, "figure5_scenario1_dv_time_by_design")
  } else if (identical(scenario, "scenario2")) {
    file.path(FIG_DIR, "figure6_scenario2_dv_time_by_design")
  } else {
    NA_character_
  }
  if (is.finite(match(scenario, c("scenario1", "scenario2")))) out_stems <- c(out_stems, figure_stem)
  for (stem in out_stems) {
    save_plot_formats(p, stem, width = 13, height = 6.4)
  }

  data.table(
    scenario = scenario,
    png = paste0(out_stems[[1L]], ".png"),
    svg = paste0(out_stems[[1L]], ".svg"),
    nested_png = paste0(out_stems[[2L]], ".png"),
    nested_svg = paste0(out_stems[[2L]], ".svg")
  )
}

main <- function() {
  manifest <- read_manifest()
  scenarios <- parse_scenarios()
  all_designs <- plot_all_designs(read_scenario_data(scenarios[[1L]]), manifest)
  out <- rbindlist(lapply(scenarios, function(scenario) {
    plot_one_scenario(read_scenario_data(scenario), manifest, scenario)
  }), use.names = TRUE)
  fwrite(out, file.path(FIG_DIR, "scenario_dv_time_plot_index.csv"))
  fwrite(all_designs, file.path(FIG_DIR, "figure4_dv_time_all_designs_index.csv"))
  print(out)
}

main()
