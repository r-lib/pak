
setup_global_handler <- function() {
  if (getRversion() < "4.0") return()
  if (Sys.getenv("R_PKG_NO_GLOBAL_HANDLER", "") != "") return()
  packageStartupMessage("Registering pak global handler.")
  globalCallingHandlers(packageNotFoundError = global_handler)
}

remove_global_handler <- function(...) {
  hdrs <- globalCallingHandlers()
  mine <- vapply(
    hdrs,
    function(x) environmentName(environment(x)) == "pak",
    logical(1)
  )

  ## Does not seem to work with an empty named list, handle that specially.
  if (all(mine)) {
    globalCallingHandlers(NULL)
  } else if (any(mine)) {
    globalCallingHandlers(hdrs[!mine])
  }

  invisible(FALSE)
}

if (getRversion() < "4.0") {
  globalVariables("globalCallingHandlers")
}

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
