
#' Convert and check function arguments
#'
#' This function is used for all variants of `r` and `rcmd`. An argument
#' name is only used to refer to one kind of object, to make this possible.
#'
#' The benefit of having a single `options` object is to avoid passing
#' around a lot of arguments all the time.
#'
#' The benefit of making this object internal (i.e. that the `r`, etc.
#' functions have multiple arguments instead of a single `options` list),
#' is that documentation and usage is more user friendly (e.g. command-
#' completion works in the editor.
#'
#' @param options List of options.
#'
#' @keywords internal

convert_and_check_my_args <- function(options) {

  has <- function(x) x %in% names(options)
  no <- function(x) ! has(x)

  ## Conversions
  options <- within(options, {
    if (has("libpath")) libpath <- as.character(libpath)
    if (has("repos")) repos <- repos
    if (has("stdout") && !is.null(stdout)) {
      stdout <- as.character(stdout)
    }
    if (has("stderr") && !is.null(stderr)) {
      stderr <- as.character(stderr)
    }
    if (has("error")) error <- error[1]
    if (has("cmdargs")) cmdargs <- as.character(cmdargs)
    if (has("timeout") && !inherits(timeout, "difftime")) {
      timeout <- as.difftime(
        as.double(timeout),
        units = "secs"
      )
    }
    if (no("wd")) wd <- "."
    if (no("echo")) echo <- FALSE
    if (no("fail_on_status")) fail_on_status <- FALSE
    if (no("tmp_files")) tmp_files <- character()
    if (no("package")) package <- FALSE
    if (no("arch")) arch <- "same"
  })

  ## Checks
  with(options, stopifnot(
    no("func") || is.function(func),
    no("func") || is.list(args),
    is.character(libpath),
    no("stdout") || is.null(stdout) || is_string(stdout),
    no("stderr") || is.null(stderr) || is_string(stderr),
    no("error") || is_string(error),
    is.character(cmdargs),
    no("echo") || is_flag(echo),
    no("show") || is_flag(show),
    no("callback") || is.null(callback) || is.function(callback),
    no("block_callback") || is.null(block_callback) ||
      is.function(block_callback),
    no("spinner") || is_flag(spinner),
    is_flag(system_profile),
    is_flag(user_profile) || identical(user_profile, "project"),
    is.character(env),
    no("timeout") || (length(timeout) == 1 && !is.na(timeout)),
    no("wd") || is_string(wd),
    no("fail_on_status") || is_flag(fail_on_status),
    is_string(package) || is_flag(package),
    is_string(arch)
  ))

  options
}
