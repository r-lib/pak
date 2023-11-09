
#' Run an R child process, with no configuration
#'
#' It tries to mimic a fresh R installation. In particular:
#' * No library path setting.
#' * No CRAN(-like) repository is set.
#' * The system and user profiles are not run.
#'
#' @param ... Additional arguments are passed to [r()].
#' @inheritParams r
#' @inheritSection r Security considerations
#'
#' @family callr functions
#' @export
#' @examplesIf FALSE
#' # Compare to r()
#' r(function() .libPaths())
#' r_vanilla(function() .libPaths())
#'
#' r(function() getOption("repos"))
#' r_vanilla(function() getOption("repos"))

r_vanilla <- function(func, args = list(), libpath = character(),
                      repos = c(CRAN = "@CRAN@"), cmdargs = "--slave",
                      system_profile = FALSE, user_profile = FALSE,
                      env = character(), ...) {

  r(func, args = args, libpath = libpath, repos = repos,
    cmdargs = cmdargs, system_profile = system_profile,
    user_profile = user_profile, env = env, ...)
}

#' @rdname r
#' @export

r_safe <- r

#' Run an R process that mimics the current R process
#'
#' Differences to [r()]:
#' * No extra repositories are set up.
#' * The `--no-save`, `--no-restore`
#'   command line arguments are not used. (But `--slave` still is.)
#' * The system profile and the user profile are loaded.
#' * No extra environment variables are set up.
#'
#' @inheritSection r Security considerations
#' @inheritParams r
#' @param ... Additional arguments are passed to [r()].
#'
#' @family callr functions
#' @export

r_copycat <- function(func, args = list(), libpath = .libPaths(),
                      repos = getOption("repos"), cmdargs = "--slave",
                      system_profile = TRUE, user_profile = TRUE,
                      env = character(), ...) {

  r(func, args = args, libpath = libpath, repos = repos, cmdargs = cmdargs,
    system_profile = system_profile, user_profile = user_profile,
    env = env, ...)
}
