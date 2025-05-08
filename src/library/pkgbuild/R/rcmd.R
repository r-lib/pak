#' Call R CMD 'command' with build tools active
#'
#' This is a wrapper around `callr::rcmd_safe()` that checks
#' that you have build tools available, and on Windows, automatically sets
#' the path to include Rtools.
#'
#' @param ... Parameters passed on to `rcmd_safe`.
#' @param env Additional environment variables to set. The defaults from
#'   `callr::rcmd_safe_env()` are always set.
#' @inheritParams with_build_tools
#' @inheritParams build
#' @export
#' @examples
#' # These env vars are always set
#' callr::rcmd_safe_env()
#'
#' if (has_build_tools()) {
#'   rcmd_build_tools("CONFIG", "CC")$stdout
#'   rcmd_build_tools("CC", "--version")$stdout
#' }
rcmd_build_tools <- function(
  ...,
  env = character(),
  required = TRUE,
  quiet = FALSE
) {
  env <- c(callr::rcmd_safe_env(), env)

  if (!quiet) {
    cli::cat_rule(paste0("R CMD ", ..1), col = "cyan")
  }

  warn_for_potential_errors()

  callback <- if (cli::is_dynamic_tty()) {
    block_callback(quiet)
  } else {
    simple_callback(quiet)
  }

  res <- with_build_tools(
    {
      withCallingHandlers(
        callr::rcmd_safe(
          ...,
          env = env,
          spinner = FALSE,
          show = FALSE,
          echo = FALSE,
          block_callback = callback,
          stderr = "2>&1"
        ),
        error = function(e) {
          if (!quiet) e$echo <- TRUE
          asNamespace("callr")$err$throw(e)
        }
      )
    },
    required = required
  )

  msg_for_long_paths(res)

  invisible(res)
}

msg_for_long_paths <- function(output) {
  if (
    is_windows() &&
      any(grepl("over-long path length", output$stdout))
  ) {
    message(
      "\nIt seems that this package contains files with very long paths.\n",
      "This is not supported on most Windows versions. Please contact the\n",
      "package authors and tell them about this. See this GitHub issue\n",
      "for more details: https://github.com/r-lib/remotes/issues/84\n"
    )
  }
}

warn_for_potential_errors <- function() {
  if (is_windows() && grepl(" ", R.home()) && getRversion() <= "3.4.2") {
    warning(
      immediate. = TRUE,
      "\n!!! Building will probably fail!\n",
      "This version of R has trouble with building packages if\n",
      "the R HOME directory (currently '",
      R.home(),
      "')\n",
      "has space characters. Possible workarounds include:\n",
      "- installing R to the C: drive,\n",
      "- installing it into a path without a space, or\n",
      "- creating a drive letter for R HOME via the `subst` windows command, and\n",
      "  starting R from the new drive.\n",
      "See also https://github.com/r-lib/remotes/issues/98\n"
    )
  }
}
