#' Path to user log directory
#'
#' Typical user cache directories are:
#'
#' \itemize{
#'   \item Mac OS X: \file{~/Library/Logs/<AppName>}
#'   \item Unix: \file{~/.cache/<AppName>/log}, or under
#'     \\env{$XDG_CACHE_HOME} if defined
#'   \item Win XP:  \file{C:\\Documents and Settings\\<username>\\Local Settings\\Application Data\\<AppAuthor>\\<AppName>\\Logs}
#'   \item Vista:
#'     \file{C:\\Users\\<username>\\AppData\\Local\\<AppAuthor>\\<AppName>\\Logs}
#' }
#'
#' On Windows the only suggestion in the MSDN docs is that local settings
#' go in the `CSIDL_LOCAL_APPDATA` directory.
#'
#' @section Opinion:
#' This function appends \file{Logs} to the `CSIDL_LOCAL_APPDATA`
#' value for Windows and appends \file{log} to the user cache dir for Unix.
#' This can be disabled with the `opinion = FALSE` option.
#'
#' @inheritParams user_data_dir
#' @param opinion (logical) can be `FALSE` to disable the appending of
#'   \file{Logs} to the base app data dir for Windows, and \file{log} to the
#'   base cache dir for Unix. See discussion below.
#' @examples
#' user_log_dir()
#' @export
user_log_dir <- function(appname = NULL, appauthor = appname, version = NULL,
                         opinion = TRUE, expand = TRUE, os = NULL) {
  version <- check_version(version, appname, expand)

  switch(check_os(os),
    win = file_path(
      win_path("local"), appauthor, appname, version,
      if (opinion) "Logs"
    ),
    mac = file_path("~/Library/Logs", appname, version),
    unix = file_path(
      Sys.getenv("XDG_CACHE_HOME", "~/.cache"),
      appname, version, if (opinion) "log"
    )
  )
}
