

#' Terminal Hyperlinks
#'
#' @details
#' hyperlink()` creates an ANSI hyperlink.
#'
#' `has_hyperlink()` checks if the current `stdout()` supports hyperlinks.
#' terminal links.
#'
#' See also
#' <https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda>.
#'
#' @param text Text to show. `text` and `url` are recycled to match their
#'   length, via a `paste0()` call.
#' @param url URL to link to.
#' @return Logical scalar, for `has_hyperlink()`.
#' 
#' @export
#' @examples
#' cat("This is an", hyperlink("R", "https://r-project.org"), "link.\n")

hyperlink <- function(text, url) {
  if (has_hyperlink()) {
    paste0("\u001B]8;;", url, "\u0007", text, "\u001B]8;;\u0007")
  } else {
    text
  }
}

#' @export
#' @name hyperlink
#' @examples
#' has_hyperlink()

has_hyperlink <- function() {

  ## Hyperlinks forced?
  enabled <- getOption("crayon.hyperlink")
  if (!is.null(enabled)) { return(isTRUE(enabled)) }

  ## Are we in a terminal? No?
  if (!isatty(stdout())) { return(FALSE) }

  ## Are we in a windows terminal?
  if (os_type() == "windows")  { return(TRUE) }

  ## Better to avoid it in CIs
  if (nzchar(Sys.getenv("CI")) ||
      nzchar(Sys.getenv("TEAMCITY_VERSION"))) { return(FALSE) }

  ## iTerm
  if (nzchar(TERM_PROGRAM <- Sys.getenv("TERM_PROGRAM"))) {
    version <- package_version(
      Sys.getenv("TERM_PROGRAM_VERSION"),
      strict = FALSE)

    if (TERM_PROGRAM == "iTerm.app") {
      if (!is.na(version) && version >= "3.1") return(TRUE)
    }
  }

  if (nzchar(VTE_VERSION <- Sys.getenv("VTE_VERSION"))) {
    if (package_version(VTE_VERSION) >= "0.50.1")  return(TRUE)
  }

  FALSE
}
