
#' Run an `R CMD` command
#'
#' Run an `R CMD` command form within R. This will usually start
#' another R process, from a shell script.
#'
#' Starting from `callr` 2.0.0, `rcmd()` has safer defaults, the same as
#' the `rcmd_safe()` default values. Use [rcmd_copycat()] for the old
#' defaults.
#'
#' @param cmd Command to run. See `R --help` from the command
#'   line for the various commands. In the current version of R (3.2.4)
#'   these are: `BATCH`, `COMPILE`, `SHLIB`, `INSTALL`, `REMOVE`, `build`,
#'   `check`, `LINK`, `Rprof`, `Rdconv`, `Rd2pdf`, `Rd2txt`, `Stangle`,
#'   `Sweave`, `Rdiff`, `config`, `javareconf`, `rtags`.
#' @param cmdargs Command line arguments.
#' @param stdout Optionally a file name to send the standard output to.
#' @param stderr Optionally a file name to send the standard error to.
#'   It may be the same as `stdout`, in which case standard error is
#'   redirected to standard output. It can also be the special string
#'   `"2>&1"`, in which case standard error will be redirected to standard
#'   output.
#' @param poll_connection Whether to have a control connection to
#'   the process. This is used to transmit messages from the subprocess
#'   to the parent.
#' @param echo Whether to echo the complete command run by `rcmd`.
#' @param wd Working directory to use for running the command. Defaults
#'   to the current working directory.
#' @param fail_on_status Whether to throw an R error if the command returns
#'   with a non-zero status code. By default no error is thrown.
#' @inheritParams r
#' @inheritSection r Security considerations
#' @return A list with the command line `$command`),
#'   standard output (`$stdout`), standard error (`stderr`),
#'   exit status (`$status`) of the external `R CMD` command, and
#'   whether a timeout was reached (`$timeout`).
#'
#' @family R CMD commands
#' @export
#'
#' @examplesIf FALSE
#' rcmd("config", "CC")

rcmd <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                 repos = default_repos(),
                 stdout = NULL, stderr = NULL, poll_connection = TRUE,
                 echo = FALSE, show = FALSE, callback = NULL,
                 block_callback = NULL, spinner = show && interactive(),
                 system_profile = FALSE, user_profile = "project",
                 env = rcmd_safe_env(), timeout = Inf, wd = ".",
                 fail_on_status = FALSE, ...) {

  ## This contains the context that we set up in steps
  options <- convert_and_check_my_args(as.list(environment()))
  options$extra <- list(...)

  options <- setup_context(options)
  options <- setup_callbacks(options)
  options <- setup_rcmd_binary_and_args(options)

  ## This cleans up everything...
  on.exit(unlink(options$tmp_files, recursive = TRUE), add = TRUE)

  run_r(options)
}

#' @rdname rcmd
#' @export

rcmd_safe <- rcmd

#' `rcmd_safe_env` returns a set of environment variables that are
#' more appropriate for [rcmd_safe()]. It is exported to allow manipulating
#' these variables (e.g. add an extra one), before passing them to the
#' [rcmd()] functions.
#'
#' It currently has the following variables:
#' * `CYGWIN="nodosfilewarning"`: On Windows, do not warn about MS-DOS
#'   style file names.
#' * `R_TESTS=""` This variable is set by `R CMD check`, and makes the
#'   child R process load a startup file at startup, from the current
#'   working directory, that is assumed to be the `/test` directory
#'   of the package being checked. If the current working directory is
#'   changed to something else (as it typically is by `testthat`, then R
#'   cannot start. Setting it to the empty string ensures that `callr` can
#'   be used from unit tests.
#' * `R_BROWSER="false"`: typically we don't want to start up a browser
#'   from the child R process.
#' * `R_PDFVIEWER="false"`: similarly for the PDF viewer.
#'
#' Note that `callr` also sets the `R_ENVIRON`, `R_ENVIRON_USER`,
#' `R_PROFILE` and `R_PROFILE_USER` environment variables
#' appropriately, unless these are set by the user in the `env` argument
#' of the `r`, etc. calls.
#'
#' @return A named character vector of environment variables.
#'
#' @export

rcmd_safe_env <- function() {

  vars <- c(
    CYGWIN = "nodosfilewarning",
    R_TESTS = "",
    R_BROWSER = "false",
    R_PDFVIEWER = "false"
  )

  vars
}

#' Call and `R CMD` command, while mimicking the current R session
#'
#' This function is similar to [rcmd()], but it has slightly different
#' defaults:
#' * The `repos` options is unchanged.
#' * No extra environment variables are defined.
#'
#' @inheritSection r Security considerations
#' @inheritParams rcmd
#' @param ... Additional arguments are passed to [rcmd()].
#'
#' @family R CMD commands
#' @export

rcmd_copycat <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                         repos = getOption("repos"), env = character(),
                         ...) {

  rcmd(cmd, cmdargs = cmdargs, libpath = libpath, repos = repos, env = env,
       ...)
}
