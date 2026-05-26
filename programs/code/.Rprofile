if (!nzchar(Sys.getenv("RENV_CONFIG_SYNCHRONIZED_CHECK", ""))) {
  Sys.setenv(RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE")
}
autoload_enabled <- tolower(Sys.getenv("RENV_CONFIG_AUTOLOADER_ENABLED", "TRUE")) %in% c("true", "t", "1")
if (isTRUE(autoload_enabled)) {
  source("/project/renv/activate.R")
}
