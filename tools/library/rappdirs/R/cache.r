#' Path to user cache directory
#'
#' @description
#' This functions uses `R_USER_CACHE_DIR` if set. Otherwise, they follow
#' platform conventions. Typical user cache directories are:
#'
#' * Mac OS X: `~/Library/Caches/<AppName>`
#' * Linux: `~/.cache/<AppName>`
#' * Win XP: `C:\\Documents and Settings\\<username>\\Local Settings\\Application Data\\<AppAuthor>\\<AppName>\\Cache`
#' * Vista: `C:\\Users\\<username>\\AppData\\Local\\<AppAuthor>\\<AppName>\\Cache`
#'
#' @section Opinion:
#' On Windows the only suggestion in the MSDN docs is that local settings go
#' in the `CSIDL_LOCAL_APPDATA` directory. This is identical to the
#' non-roaming app data dir (i.e. [user_data_dir()]). But apps typically put
#' cache data somewhere *under* this directory so `user_cache_dir()` appends
#' `Cache` to the `CSIDL_LOCAL_APPDATA` value, unless `opinion = FALSE`.
#'
#' @inheritParams user_data_dir
#' @param opinion (logical) Use `FALSE` to disable the appending of
#'   `Cache` on Windows. See discussion below.
#' @seealso [tempdir()] for a non-persistent temporary directory.
#' @export
#' @examples
#' user_cache_dir("rappdirs")
user_cache_dir <- function(appname = NULL, appauthor = appname, version = NULL,
                           opinion = TRUE, expand = TRUE, os = NULL) {
  version <- check_version(version, appname, expand)

  base <- base_path(os, "CACHE",
    win  = win_path("local"),
    mac  = "~/Library/Caches",
    unix = Sys.getenv("XDG_CACHE_HOME", "~/.cache")
  )

  switch(check_os(os),
    win = file_path(base, appauthor, appname, version, if (opinion) "Cache"),
    mac = file_path(base, appname, version),
    unix = file_path(base, appname, version)
  )
}
