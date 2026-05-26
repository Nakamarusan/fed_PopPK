# bootstrap_main.R
# Thin bootstrap entrypoint: config/init orchestration + delegated trial execution.

local({
  candidates <- c("/project/R/common/source_utils.R", "R/common/source_utils.R")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("R/common/source_utils.R not found")
  source(hit[[1L]], chdir = TRUE)
})
fedpoppk_source("R/comm/server_comm.R")
fedpoppk_source("R/bootstrap/server_comm_boot.R")
fedpoppk_source("R/objective/server_objective.R")
fedpoppk_source("R/optimize/server_optimize.R")
fedpoppk_source("R/config/server_config.R")
fedpoppk_source("R/server/runtime_common.R")
fedpoppk_source("R/server/bootstrap/trial_runner.R")
fedpoppk_source("R/server/bootstrap/summary_writer.R")
fedpoppk_source("R/common/param_transform.R")
suppressPackageStartupMessages(library(jsonlite))
started_at <- Sys.time()

cli <- runtime_parse_cli(
  args = commandArgs(trailingOnly = TRUE),
  default_config = "/project/configs/bootstrap.json",
  allow_seed = FALSE
)
cfg <- runtime_prepare_cfg(server_config(cli$config))
param_names <- cfg$param_spec$names

grad_cores <- runtime_detect_grad_cores(cfg$objective)
Sys.sleep(5)  # wait for client startup
comm_metrics_reset()

init_responses <- runtime_send_init(cfg = cfg, grad_cores = grad_cores, bootstrap = cfg$bootstrap)
if (any(vapply(init_responses, function(resp) {
  !is.list(resp) || !(resp$status %||% "") %in% c("initialized", "ready")
}, logical(1)))) {
  stop("One or more clients failed to prepare bootstrap datasets")
}

init_par_nat <- runtime_require_named_numeric(cfg$init_par, param_names, "init_par")
lower_vec <- runtime_require_named_numeric(cfg$lower, param_names, "lower")
upper_vec <- runtime_require_named_numeric(cfg$upper, param_names, "upper")
opt_space <- param_optimizer_space(init_par_nat, cfg$param_spec, lower = lower_vec, upper = upper_vec)

optim_ctrl <- runtime_strip_nonoptim_fields(cfg$optim_control)
comm_timeout <- cfg$comm$run_timeout %||% fedpoppk_const("comm_boot.run_timeout", 1200)
comm_max_tries <- cfg$comm$run_max_tries %||% fedpoppk_const("comm_boot.run_max_tries", 3L)
comm_pause <- cfg$comm$run_pause %||% fedpoppk_const("comm_boot.run_pause", 1)

B <- as.integer(cfg$bootstrap$B_per_client %||% fedpoppk_const("bootstrap.B_per_client", 500L))
seed_base <- as.integer(cfg$bootstrap$seed_base %||% fedpoppk_const("bootstrap.seed_base", 4869L))
schema_meta <- runtime_schema_meta(
  config_path = cli$config,
  seed = seed_base,
  config_hash = cfg$config_hash_resolved %||% NA_character_
)

run <- runtime_make_run(
  logging = cfg$logging,
  default_base = "/project/result/logs_boot",
  default_scenario = "bootstrap"
)
run_id <- run$run_id
run_dir <- run$run_dir

trial_start <- runtime_read_int_env("BOOT_TRIAL_START", default = 1L)
trial_count <- runtime_read_int_env("BOOT_TRIAL_COUNT", default = B)
trials <- seq.int(from = trial_start, length.out = trial_count)

exec <- bootstrap_execute_trials(
  trials = trials,
  cfg = cfg,
  init_par_opt = opt_space$init_z,
  lower_opt = opt_space$lower_z,
  upper_opt = opt_space$upper_z,
  optim_ctrl = optim_ctrl,
  run_id = run_id,
  out_root = run_dir,
  seed_base = seed_base,
  comm_timeout = comm_timeout,
  comm_max_tries = comm_max_tries,
  comm_pause = comm_pause,
  param_names = param_names,
  schema_meta = schema_meta
)
comm_out <- comm_metrics_write(run_dir)

summary <- bootstrap_write_summary(
  out_root = run_dir,
  trials = trials,
  trial_records = exec$trial_records,
  par_storage = exec$par_storage,
  run_info = list(
    run_id = run_id,
    log_base = run$base_dir,
    log_scenario = run$scenario,
    seed_base = seed_base,
    B_per_client = B
  ),
  schema_meta = schema_meta,
  run_meta = list(
    duration_sec = as.numeric(difftime(Sys.time(), started_at, units = "secs")),
    host = Sys.info()[["nodename"]] %||% NA_character_,
    image_tag = Sys.getenv("FEDPOPPK_IMAGE_TAG", unset = NA_character_),
    communication = comm_out$summary,
    communication_files = list(
      csv = comm_out$csv,
      json = comm_out$json
    )
  )
)

cat(toJSON(summary, auto_unbox = TRUE, digits = 10))
