
#' Create options for an [r_process] object
#'
#' @param ... Options to override, named arguments.
#' @return A list of options.
#'
#' `r_process_options()` creates a set of options to initialize a new
#' object from the `r_process` class. Its arguments must be named, the
#' names are used as option names. The options correspond to (some of)
#' the arguments of the [r()] function. At least the `func` option must be
#' specified, this is the R function to run in the background.
#'
#' @export
#' @examples
#' ## List all options and their default values:
#' r_process_options()

r_process_options <- function(...) {
  update_options(r_process_options_default(), ...)
}

#' Create options for an [rcmd_process] object
#'
#' @param ... Options to override, named arguments.
#' @return A list of options.
#'
#' `rcmd_process_options()` creates a set of options to initialize a new
#' object from the `rcmd_process` class. Its arguments must be named, the
#' names are used as option names. The options correspond to (some of)
#' the arguments of the [rcmd()] function. At least the `cmd` option must
#' be specified, to select the `R CMD` subcommand to run. Typically
#' `cmdargs` is specified as well, to supply more arguments to `R CMD`.
#'
#' @export
#' @examples
#' ## List all options and their default values:
#' rcmd_process_options()

rcmd_process_options <- function(...) {
  update_options(rcmd_process_options_default(), ...)
}

#' Create options for an [rscript_process] object
#'
#' @param ... Options to override, named arguments.
#' @return A list of options.
#'
#' `rscript_process_options()` creates a set of options to initialize a new
#' object from the `rscript_process` class. Its arguments must be named,
#' the names are used as option names. The options correspond to (some of)
#' the arguments of the [rscript()] function. At least the `script` option
#' must be specified, the script file to run.
#'
#' @export
#' @examples
#' ## List all options and their default values:
#' rscript_process_options()

rscript_process_options <- function(...) {
  update_options(rscript_process_options_default(), ...)
}

r_process_options_default <- function() {
  list(
    func = NULL,
    args = list(),
    libpath = .libPaths(),
    repos = default_repos(),
    stdout = "|",
    stderr = "|",
    poll_connection = TRUE,
    error = getOption("callr.error", "error"),
    cmdargs = c("--slave", "--no-save", "--no-restore"),
    system_profile = FALSE,
    user_profile = "project",
    env = character(),
    supervise = FALSE,
    load_hook = default_load_hook(),
    extra = list(),
    package = FALSE,
    arch = "same"
  )
}

rcmd_process_options_default <- function() {
  list(
    cmd = NULL,
    cmdargs = character(),
    libpath = .libPaths(),
    stdout = "|",
    stderr = "|",
    poll_connection = TRUE,
    repos = default_repos(),
    system_profile = FALSE,
    user_profile = "project",
    env = rcmd_safe_env(),
    wd = ".",
    supervise = FALSE,
    extra = list(),
    arch = "same"
  )
}

rscript_process_options_default <- function() {
  list(
    script = NULL,
    cmdargs = character(),
    libpath = .libPaths(),
    stdout = "|",
    stderr = "|",
    poll_connection = TRUE,
    repos = default_repos(),
    system_profile = FALSE,
    user_profile = "project",
    env = rcmd_safe_env(),
    wd = ".",
    color = FALSE,
    extra = list(),
    arch = "same"
  )
}

update_options <- function(old_opts, ...) {
  new_opts <- list(...)
  stopifnot(is.named(new_opts))
  check_for_option_names(old_opts, new_opts)
  utils::modifyList(old_opts, new_opts)
}

check_for_option_names <- function(old, new) {
  if (length(miss <- setdiff(names(new), names(old)))) {
    throw(new_error("Unknown option", if (length(miss) > 1) "s", ":",
                    enumerate(sQuote(miss))))
  }
}
