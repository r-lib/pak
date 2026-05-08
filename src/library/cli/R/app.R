cliappenv <- new.env()
cliappenv$stack <- list()
cliappenv$pid <- Sys.getpid()

#' Start, stop, query the default cli application
#'
#' `start_app` creates an app, and places it on the top of the app stack.
#'
#' `stop_app` removes the top app, or multiple apps from the app stack.
#'
#' `default_app` returns the default app, the one on the top of the stack.
#'
#' @param theme Theme to use.
#' @param output How to print the output.
#' @param .auto_close Whether to stop the app, when the calling frame
#'   is destroyed.
#' @param .envir The environment to use, instead of the calling frame,
#'   to trigger the stop of the app.
#' @param app App to stop. If `NULL`, the current default app is stopped.
#'   Otherwise we find the supplied app in the app stack, and remote it,
#'   together with all the apps above it.
#' @return
#'   `start_app` returns the new app, `default_app` returns the default app.
#'   `stop_app` does not return anything.
#'
#' @export

start_app <- function(
  theme = getOption("cli.theme"),
  output = c("auto", "message", "stdout", "stderr"),
  .auto_close = TRUE,
  .envir = parent.frame()
) {
  if (!inherits(output, "connection")) output <- match.arg(output)

  app <- cliapp(
    theme = theme,
    user_theme = getOption("cli.user_theme"),
    output = output
  )
  cliappenv$stack[[length(cliappenv$stack) + 1]] <- app

  if (.auto_close && !identical(.envir, globalenv())) {
    defer(stop_app(app = app), envir = .envir, priority = "first")
  }

  invisible(app)
}

#' @export
#' @name start_app

stop_app <- function(app = NULL) {
  if (is.null(app)) {
    cliappenv$stack <- utils::head(cliappenv$stack, -1)
  } else {
    if (!inherits(app, "cliapp")) {
      throw(cli_error(
        "{.arg app} must be a CLI app",
        "i" = "{.arg app} is {.type {app}}"
      ))
    }
    ndl <- format.default(app)
    nms <- vapply(cliappenv$stack, format.default, character(1))
    if (!ndl %in% nms) {
      warning("No app to end")
      return()
    }
    wh <- which(nms == ndl)[1]
    cliappenv$stack <- utils::head(cliappenv$stack, wh - 1)
  }

  invisible()
}

#' @export
#' @name start_app

default_app <- function() {
  top <- utils::tail(cliappenv$stack, 1)
  if (length(top)) top[[1]] else NULL
}
