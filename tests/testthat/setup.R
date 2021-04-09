
# This is for devtools::test() and similar runs, which do not have an
# embedded library.

lib <- private_lib_dir()
if (!identical(names(lib), "embedded")) {
  create_dev_lib()
} else {
  cli::cli_alert_info("Using embedded library")
}
