
#' Install missing packages on the fly
#'
#' Use this function to set up a global error handler, that is called
#' if R fails to load a package. This handler will offer you the choice
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
#' Currently `handle_package_not_found()` does not do anything in
#' non-interactive mode (including in knitr, testthat and RStudio
#' notebooks), this might change in the future.
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
  # TODO: is this what we want? Or refine this? E.g. testthat, knitr?
  if (!is_interactive()) return()

  # TODO: what if message output is redirected? we ignore for now
  if (sink.number() != 0) return()

  pkg <- err$package
  lib <- err$lib.loc %||% .libPaths()[1]

  can_cont <- !is.null(findRestart("retry_loadNamespace"))

  cli <- load_private_cli()
  cli$cli_text()
  cli$cli_alert_danger(
    c("Failed to load package {.pkg {pkg}}. Do you want to install it ",
      "into the default library at {.path {lib}}?"),
    wrap = TRUE
    )
  cli$cli_text()

  dv <- cli$cli_div(theme = list(ol = list("margin-left" = 2)))
  cli$cli_ol()
  if (can_cont) {
    cli$cli_li("Yes, install it, and continue the original computation.")
  } else {
    cli$cli_li("Yes, install it.")
  }
  cli$cli_li("No, stop now, and I'll handle it myself.")
  cli$cli_text()
  cli$cli_end(dv)

  ans <- get_answer(c("1", "2"), "  ? Your choice [1]: ")

  cat("\n")

  if (ans == "2") return()

  cli$cli_rule("start installation")
  pkg_install(pkg, lib = lib[1])
  cli$cli_rule("end installation")
  cli$cli_text()

  if (can_cont) invokeRestart("retry_loadNamespace")
}
