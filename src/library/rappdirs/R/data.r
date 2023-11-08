#' Path to user config/data directories
#'
#' @description
#' `user_data_dir()` returns path to the user-specific data directory and
#' `user_config_dir()` returns full path to the user-specific configuration
#' directory. These are the same on Windows and Mac but different on Linux.
#'
#' These functions first use `R_USER_DATA_DIR` and `R_USER_CONFIG_DIR` if set.
#' Otherwise, they follow platform conventions. Typical user config and data
#' directories are:
#'
#' * Mac OS X: `~/Library/Application Support/<AppName>`
#' * Win XP (not roaming): `C:\\Documents and Settings\\<username>\\Data\\<AppAuthor>\\<AppName>`
#' * Win XP (roaming): `C:\\Documents and Settings\\<username>\\Local Settings\\Data\\<AppAuthor>\\<AppName>`
#' * Win 7 (not roaming): `C:\\Users\\<username>\\AppData\\Local\\<AppAuthor>\\<AppName>`
#' * Win 7 (roaming): `C:\\Users\\<username>\\AppData\\Roaming\\<AppAuthor>\\<AppName>`
#'
#' Only Linux makes the distinction between config and data:
#'
#' * Data: `~/.local/share/<AppName>`
#' * Config: `~/.config/<AppName>`
#'
#' @param appname is the name of application. If NULL, just the system
#'     directory is returned.
#' @param appauthor (only required and used on Windows) is the name of the
#'     appauthor or distributing body for this application. Typically
#'     it is the owning company name. This falls back to appname.
#' @param version is an optional version path element to append to the
#'     path. You might want to use this if you want multiple versions
#'     of your app to be able to run independently. If used, this
#'     would typically be `"<major>.<minor>"`. Only applied when appname
#'     is not NULL.
#' @param roaming (logical, default `FALSE`) can be set `TRUE` to
#'     use the Windows roaming appdata directory. That means that for users on
#'     a Windows network setup for roaming profiles, this user data will be
#'     sync'd on login. See
#'     <https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-vista/cc766489(v=ws.10)>
#'     for a discussion of issues.
#' @param os Operating system whose conventions are used to construct the
#'     requested directory. Possible values are "win", "mac", "unix". If `NULL`
#'     (the default) then the current OS will be used.
#' @param expand If TRUE (the default) will expand the `R_LIBS` specifiers with their equivalents.
#'      See [R_LIBS()] for list of all possibly specifiers.
#' @export
#' @examples
#' user_data_dir("rappdirs")
#'
#' user_config_dir("rappdirs", roaming = TRUE, os = "win")
#' user_config_dir("rappdirs", roaming = FALSE, os = "win")
#' user_config_dir("rappdirs", os = "unix")
#' user_config_dir("rappdirs", os = "mac")
#' user_config_dir("rappdirs", version = "%p-platform/%v")
user_data_dir <- function(appname = NULL, appauthor = appname, version = NULL,
                          roaming = FALSE, expand = TRUE, os = NULL) {
  version <- check_version(version, appname, expand)

  base <- base_path(os, "DATA",
    win  = win_path(ifelse(roaming, "roaming", "local")),
    mac  = "~/Library/Application Support",
    unix = Sys.getenv("XDG_DATA_HOME", "~/.local/share")
  )
  switch(check_os(os),
    win = file_path(base, appauthor, appname, version),
    mac = file_path(base, appname, version),
    unix = file_path(base, appname, version)
  )
}

#' @rdname user_data_dir
#' @export
user_config_dir <- function(appname = NULL, appauthor = appname, version = NULL,
                            roaming = TRUE, expand = TRUE, os = NULL) {
  version <- check_version(version, appname, expand)

  base <- base_path(os, "CONFIG",
    win  = win_path(ifelse(roaming, "roaming", "local")),
    mac  = "~/Library/Application Support",
    unix = Sys.getenv("XDG_CONFIG_HOME", "~/.config")
  )

  switch(check_os(os),
    win = file_path(base, appauthor, appname, version),
    mac = file_path(base, appname, version),
    unix = file_path(base, appname, version)
  )
}


#' Path to shared data/config directories
#'
#' `site_data_dir` returns full path to the user-shared data dir for this application.
#' `site_config_dir` returns full path to the user-specific configuration directory for this application
#' which returns the same path as site data directory in Windows and Mac but a different one for Unix.
#' Typical user-shared data directories are:
#'
#' * Mac OS X:  `/Library/Application Support/<AppName>`
#' * Unix:      `/usr/local/share:/usr/share/`
#' * Win XP:    `C:\\Documents and Settings\\All Users\\Application Data\\<AppAuthor>\\<AppName>`
#' * Vista:     (Fail! `C:\\ProgramData` is a hidden *system* directory on Vista.)
#' * Win 7:     `C:\\ProgramData\\<AppAuthor>\\<AppName>`. Hidden, but writeable on Win 7.
#'
#' Unix also specifies a separate location for user-shared configuration data in \env{$XDG_CONFIG_DIRS}.
#'
#' * Unix: `/etc/xdg/<AppName>`, in \env{$XDG_CONFIG_HOME} if defined
#'
#' For Unix, this returns the first default.  Set the `multipath=TRUE` to guarantee returning all directories.
#'
#' @inheritParams user_data_dir
#' @param multipath is an optional parameter only applicable to *nix
#'       which indicates that the entire list of data dirs should be returned
#'       By default, the first directory is returned
#' @section Warning:
#' Do not use this on Windows. See the note above for why.
#' @export
site_data_dir <- function(appname = NULL, appauthor = appname, version = NULL,
                          multipath = FALSE, expand = TRUE, os = NULL) {
  version <- check_version(version, appname, expand)

  switch(check_os(os),
    win = file_path(win_path("common"), appauthor, appname, version),
    mac = file_path("/Library/Application Support", appname, version),
    unix = file_path_site_unix(
      Sys.getenv("XDG_DATA_DIRS", "/usr/local/share:/usr/share"),
      appname, version, multipath
    )
  )
}

#' @rdname site_data_dir
#' @export
site_config_dir <- function(appname = NULL, appauthor = appname, version = NULL,
                            multipath = FALSE, expand = TRUE, os = NULL) {
  version <- check_version(version, appname, expand)

  switch(check_os(os),
    win = file_path(win_path("common"), appauthor, appname, version),
    mac = file_path("/Library/Application Support", appname, version),
    unix = file_path_site_unix(
      Sys.getenv("XDG_CONFIG_DIRS", "/etc/xdg"),
      appname, version, multipath
    )
  )
}

# wrapper with `multipath` and use `parse_path_string` for cleaner switch statement
file_path_site_unix <- function(sys_getenv, appname, version, multipath = FALSE) {
  paths <- parse_path_string(sys_getenv)
  if (multipath) {
    vapply(paths, file_path, appname, version,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
  } else {
    file_path(paths[[1]], appname, version)
  }
}

parse_path_string <- function(path, sep = ":") {
  unique(strsplit(path, sep)[[1]])
}
