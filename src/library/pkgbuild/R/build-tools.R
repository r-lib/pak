#' Are build tools are available?
#'
#' `has_build_tools` returns a logical, `check_build_tools` throws
#' an error. `with_build_tools` checks that build tools are available,
#' then runs `code` in an correctly staged environment.
#' If run interactively from RStudio, and the build tools are not
#' available these functions will trigger an automated install.
#'
#' Errors like `running command
#' '"C:/PROGRA~1/R/R-34~1.2/bin/x64/R" CMD config CC' had status 127`
#' indicate the code expected Rtools to be on the system PATH. You can
#' then verify you have rtools installed with `has_build_tools()` and
#' temporarily add Rtools to the PATH `with_build_tools({ code })`.
#'
#' It is possible to add Rtools to your system PATH manually; you can use
#' [rtools_path()] to show the installed location. However because this
#' requires manual updating when a new version of Rtools is installed and the
#' binaries in Rtools may conflict with existing binaries elsewhere on the PATH it
#' is better practice to use `with_build_tools()` as needed.
#' @inheritParams has_rtools
#' @param quiet if `TRUE` suppresses output from this function.
#' @export
#' @seealso has_rtools
#' @examples
#' has_build_tools(debug = TRUE)
#' check_build_tools()
has_build_tools <- function(debug = FALSE) {
  check <- getOption("buildtools.check", NULL)
  # we do this from R 4.3.0, because some people might still use
  # Rtools40 for R 4.2.x, and we don't want to break their config
  has <- if (is_windows() && getRversion() >= "4.3.0") {
    has_compiler(debug = debug)
  } else if (is_windows()) {
    has_rtools(debug = debug)
  } else {
    has_compiler(debug = debug)
  }

  if (!has && !is.null(check)) {
    return(check("Building R package from source"))
  }

  if (!has && is_windows() && getRversion() >= "4.3.0") {
    message(
      "WARNING: Rtools is required to build R packages, but is not ",
      "currently installed.\n\n",
      "Please download and install the appropriate version of Rtools for ",
      getRversion(), " from\n", rtools_url(), "."
    )
  }

  has
}

#' @export
#' @rdname has_build_tools
check_build_tools <- function(debug = FALSE, quiet = FALSE) {
  if (!has_build_tools(debug = debug)) {
    stop(
      "Could not find tools necessary to compile a package\n",
      "Call `pkgbuild::check_build_tools(debug = TRUE)` to diagnose the problem.",
      call. = FALSE
    )
  } else if (!isTRUE(quiet)) {
    message("Your system is ready to build packages!")
  }

  invisible(TRUE)
}

#' @export
#' @rdname has_build_tools
#' @param code Code to rerun in environment where build tools are guaranteed to
#'   exist.
#' @param required If `TRUE`, and build tools are not available,
#'   will throw an error. Otherwise will attempt to run `code` without
#'   them.
with_build_tools <- function(code, debug = FALSE, required = TRUE) {
  if (required) {
    check_build_tools(debug = debug, quiet = TRUE)
  }

  if (has_rtools()) {
    withr_with_path(rtools_path(), code)
  } else {
    code
  }
}

#' @rdname has_build_tools
#' @param .local_envir The environment to use for scoping.
#' @export
local_build_tools <- function(debug = FALSE, required = TRUE, .local_envir = parent.frame()) {
  if (required) {
    check_build_tools(debug = debug, quiet = TRUE)
  }

  if (has_rtools()) {
    withr_local_path(rtools_path(), .local_envir = .local_envir)
  }
}
