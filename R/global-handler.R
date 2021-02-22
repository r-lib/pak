
global_handler <- function(err) {
  # TODO: is this what we want? Or refine this? E.g. testthat, knitr?
  if (!is_interactive()) return()

  # TODO: what if message output is redirected? we ignore for now
  if (sink.number() != 0) return()

  pkg <- err$package
  lib <- err$lib.loc

  cat0("\nFailed to load package: '", pkg, "'. Do you want to install it?")
  cat0(
    "\n\n  1. Install '", pkg, "' from the configured repositories into\n",
    "    '", lib[1], "'.\n")
  cat(
    "  2. No, not this time.\n\n")

  ans <- get_answer(c("1", "2"))

  if (ans == "2") return()

  pak::pkg_install(pkg, lib = lib[1])

  if (!is.null(findRestart("retry_loadNamespace"))) {
    cfm <- get_confirmation2("? Do you want to continue (Y/n) ")
    if (cfm) invokeRestart("retry_loadNamespace")
  }
}
