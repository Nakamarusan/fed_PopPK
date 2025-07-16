# tests/testthat/test-server_config.R
library(testthat)
library(jsonlite)
library(withr)

#── 実装モジュールをロード ───────────────────────────────────────
source("//root/test/server/R/server_config.R")

make_temp_json <- function(obj) {
  f <- tempfile(fileext = ".json")
  write_json(obj, f, auto_unbox = TRUE)
  f
}

test_that("load_server_config() reads valid config without schema", {
  local({
    assign("SCHEMA_FILE", "nonexistent_schema.json",
           envir = environment(load_server_config))

    cfg_obj <- list(
      clients      = c("http://c1/run", "http://c2/run"),
      initPar      = list(a = 1, b = 2),
      optimControl = list(maxit = 5)
    )
    cfg_path <- make_temp_json(cfg_obj)

    # No schema file: should not error
     expect_silent(
        cfg <- load_server_config(cfg_path)
        )

    expect_equal(cfg$clients,      cfg_obj$clients)
    expect_equal(cfg$initPar,      cfg_obj$initPar)
    expect_equal(cfg$optimControl, cfg_obj$optimControl)
  })
})

test_that("load_server_config() errors if file not found", {
  expect_error(
    load_server_config("no_such_file.json"),
    "Configuration file not found"
  )
})

test_that("load_server_config() errors on missing required fields", {
  cfg_obj <- list(initPar = list(x = 1))  # clients missing
  cfg_path <- make_temp_json(cfg_obj)
  expect_error(
    load_server_config(cfg_path),
    "Missing required fields"
  )
})

test_that("load_server_config() enforces schema when present", {
  schema <- list(
    type       = "object",
    required   = c("clients", "initPar"),
    properties = list(
      clients = list(type = "array", items = list(type = "string")),
      initPar = list(type = "object")
    )
  )
  schema_path <- tempfile(fileext = ".json")
  write_json(schema, schema_path, auto_unbox = TRUE)

  local({
    assign("SCHEMA_FILE", schema_path,
           envir = environment(load_server_config))

    bad_cfg <- list(clients = c(1,2,3), initPar = list(x = 1))
    bad_path <- make_temp_json(bad_cfg)
    expect_error(
      load_server_config(bad_path),
      "Configuration JSON failed schema validation"
    )

    good_cfg <- list(clients = c("a","b"), initPar = list(x = 1))
    good_path <- make_temp_json(good_cfg)
    # log_info goes to stdout; just ensure no error
    expect_silent(cfg2 <- load_server_config(good_path))
    expect_equal(cfg2$clients, good_cfg$clients)
  })
})

test_that("parse_and_load_config() integrates CLI and JSON", {
  local({
    # Prepare a real temp JSON and point parse_server_args to it
    cfg_obj <- list(
      clients      = c("u1","u2"),
      initPar      = list(alpha = 0.5),
      optimControl = list(maxit = 3)
    )
    cfg_path <- make_temp_json(cfg_obj)

    fake_args <- list(config_path = cfg_path, max_iter = 7)
    assign("parse_server_args",
           function() fake_args,
           envir = environment(parse_and_load_config))

    res <- parse_and_load_config()
    expect_equal(res$clients,      cfg_obj$clients)
    expect_equal(res$initPar,      cfg_obj$initPar)
    expect_equal(res$optimControl, cfg_obj$optimControl)
    expect_equal(res$max_iter,     fake_args$max_iter)
  })
})
