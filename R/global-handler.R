
#' Install missing packages on the fly
#'
#' Use this function to set up a global error handler, that is called
#' if R fails to load a package. This handler will offer yout the choice
#' of installing the missing package (and all its dependencies),
#' and in some cases it can also remedy the error and restart the code.
#'
#' You are not supposed to call this function directly. Instead, set it
#' up as a global error handler, possibly in your `.Rprofile` file:
#'
#' ```r
#' if (interactive() && getRversion() >= "4.0.0") {
#'   globalCallingHandlers(
#'     packageNotFoundError = function(err) {
#'       try(pak::handle_package_not_found(err))
#'     }
#'   )
#' }
#' ```
#'
#' Global error handlers are only supported in R 4.0.0 and later.
#'
#' In some cases it is possible to remedy the original computation that
#' tried to load the missing package, and pak will offer you to do so
#' after a successful installation. Currently, in R 4.0.4, it is not
#' possible to continue a failed `library()` call.
#'
#' @param err The error object, of class `packageNotFoundError`.
#' @return Nothing.
#'
#' @export

handle_package_not_found <- function(err) {
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

  pkg_install(pkg, lib = lib[1])

  if (!is.null(findRestart("retry_loadNamespace"))) {
    cfm <- get_confirmation2("? Do you want to continue (Y/n) ")
    if (cfm) invokeRestart("retry_loadNamespace")
  }
}
