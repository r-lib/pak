
#' Evaluate an expression in another R session
#'
#' From `callr` version 2.0.0, `r()` is equivalent to `r_safe()`, and
#' tries to set up a less error prone execution environment. In particular:
#' * Ensures that at least one reasonable CRAN mirror is set up.
#' * Adds some command line arguments to avoid saving `.RData` files, etc.
#' * Ignores the system and user profiles (by default).
#' * Sets various environment variables: `CYGWIN` to avoid
#'   warnings about DOS-style paths, `R_TESTS` to avoid issues
#'   when `callr` is invoked from unit tests, `R_BROWSER`
#'   and `R_PDFVIEWER` to avoid starting a browser or a PDF viewer.
#'   See [rcmd_safe_env()].
#'
#' The `r()` function from before 2.0.0 is called [r_copycat()] now.
#'
#' @param func Function object to call in the new R process.
#'   The function should be self-contained and only refer to
#'   other functions and use variables explicitly from other packages
#'   using the `::` notation. By default the environment of the function
#'   is set to `.GlobalEnv` before passing it to the child process.
#'   (See the `package` option if you want to keep the environment.)
#'   Because of this, it is good practice to create an anonymous
#'   function and pass that to `callr`, instead of passing
#'   a function object from a (base or other) package. In particular
#'   ```
#'   r(.libPaths)
#'   ```
#'   does not work, because `.libPaths` is defined in a special
#'   environment, but
#'   ```
#'   r(function() .libPaths())
#'   ```
#'   works just fine.
#' @param args Arguments to pass to the function. Must be a list.
#' @param libpath The library path.
#' @param repos The `repos` option. If `NULL`, then no
#'   `repos` option is set. This options is only used if
#'   `user_profile` or `system_profile` is set `FALSE`,
#'   as it is set using the system or the user profile.
#' @param stdout The name of the file the standard output of
#'   the child R process will be written to.
#'   If the child process runs with the `--slave` option (the default),
#'   then the commands are not echoed and will not be shown
#'   in the standard output. Also note that you need to call `print()`
#'   explicitly to show the output of the command(s).
#'   IF `NULL` (the default), then standard output is not returned, but
#'   it is recorded and included in the error object if an error happens.
#' @param stderr The name of the file the standard error of
#'   the child R process will be written to.
#'   In particular `message()` sends output to the standard
#'   error. If nothing was sent to the standard error, then this file
#'   will be empty. This argument can be the same file as `stdout`,
#'   in which case they will be correctly interleaved. If this is the
#'   string `"2>&1"`, then standard error is redirected to standard output.
#'   IF `NULL` (the default), then standard output is not returned, but
#'   it is recorded and included in the error object if an error happens.
#' @param error What to do if the remote process throws an error.
#'   See details below.
#' @param poll_connection Whether to have a control connection to
#'   the process. This is used to transmit messages from the subprocess
#'   to the main process.
#' @param cmdargs Command line arguments to pass to the R process.
#'   Note that `c("-f", rscript)` is appended to this, `rscript`
#'   is the name of the script file to run. This contains a call to the
#'   supplied function and some error handling code.
#' @param show Logical, whether to show the standard output on the screen
#'   while the child process is running. Note that this is independent
#'   of the `stdout` and `stderr` arguments. The standard
#'   error is not shown currently.
#' @param callback A function to call for each line of the standard
#'   output and standard error from the child process. It works together
#'   with the `show` option; i.e. if `show = TRUE`, and a
#'   callback is provided, then the output is shown of the screen, and the
#'   callback is also called.
#' @param block_callback A function to call for each block of the standard
#'   output and standard error. This callback is not line oriented, i.e.
#'   multiple lines or half a line can be passed to the callback.
#' @param spinner Whether to show a calming spinner on the screen while
#'   the child R session is running. By default it is shown if
#'   `show = TRUE` and the R session is interactive.
#' @param system_profile Whether to use the system profile file.
#' @param user_profile Whether to use the user's profile file.
#'   If this is `"project"`, then only the profile from the working
#'   directory is used, but the `R_PROFILE_USER` environment variable
#'   and the user level profile are not. See also "Security considerations"
#'   below.
#' @param env Environment variables to set for the child process.
#' @param timeout Timeout for the function call to finish. It can be a
#'   [base::difftime] object, or a real number, meaning seconds.
#'   If the process does not finish before the timeout period expires,
#'   then a `system_command_timeout_error` error is thrown. `Inf`
#'   means no timeout.
#' @param package Whether to keep the environment of `func` when passing
#'   it to the other package. Possible values are:
#'   * `FALSE`: reset the environment to `.GlobalEnv`. This is the default.
#'   * `TRUE`: keep the environment as is.
#'   * `pkg`: set the environment to the `pkg` package namespace.
#' @param arch Architecture to use in the child process, for multi-arch
#'   builds of R. By default the same as the main process. See
#'   [supported_archs()]. If it contains a forward or backward slash
#'   character, then it is taken as the path to the R executable.
#'   Note that on Windows you need the path to `Rterm.exe`.
#' @param ... Extra arguments are passed to [processx::run()].
#' @return Value of the evaluated expression.
#'
#' @section Error handling:
#'
#' `callr` handles errors properly. If the child process throws an
#' error, then `callr` throws an error with the same error message
#' in the main process.
#'
#' The `error` expert argument may be used to specify a different
#' behavior on error. The following values are possible:
#' * `error` is the default behavior: throw an error in the main process,
#'   with a prefix and the same error message as in the subprocess.
#' * `stack` also throws an error in the main process, but the error
#'   is of a special kind, class `callr_error`, and it contains
#'   both the original error object, and the call stack of the child,
#'   as written out by [utils::dump.frames()]. This is now deprecated,
#'   because the error thrown for `"error"` has the same information.
#' * `debugger` is similar to `stack`, but in addition
#'   to returning the complete call stack, it also start up a debugger
#'   in the child call stack, via [utils::debugger()].
#'
#' The default error behavior can be also set using the `callr.error`
#' option. This is useful to debug code that uses `callr`.
#'
#' callr uses parent errors, to keep the stacks of the main process and the
#' subprocess(es) in the same error object.
#'
#' @section Security considerations:
#'
#' `callr` makes a copy of the user's `.Renviron` file and potentially of
#' the local or user `.Rprofile`, in the session temporary
#' directory. Avoid storing sensitive information such as passwords, in
#' your environment file or your profile, otherwise this information will
#' get scattered in various files, at least temporarily, until the
#' subprocess finishes. You can use the keyring package to avoid passwords
#' in plain files.
#'
#' @section Transporting objects:
#'
#' `func` and `args` are copied to the child process by first serializing them
#' into a temporary file using [saveRDS()] and then loading them back into the
#' child session using [readRDS()]. The same strategy is used to copy the result
#' of calling `func(args)` to the main session. Note that some objects, notably
#' those with `externalptr` type, won't work as expected after being
#' saved to a file and loaded back.
#'
#' For performance reasons `compress=FALSE` is used when serializing with
#' [saveRDS()], this can be disabled by setting
#' `options(callr.compress_transport = TRUE)`.
#'
#' @family callr functions
#' @examplesIf FALSE
#' # Workspace is empty
#' r(function() ls())
#'
#' # library path is the same by default
#' r(function() .libPaths())
#' .libPaths()
#'
#' @export

r <- function(func, args = list(), libpath = .libPaths(),
              repos = default_repos(),
              stdout = NULL, stderr = NULL,
              poll_connection = TRUE,
              error = getOption("callr.error", "error"),
              cmdargs = c("--slave", "--no-save", "--no-restore"),
              show = FALSE, callback = NULL,
              block_callback = NULL, spinner = show && interactive(),
              system_profile = FALSE, user_profile = "project",
              env = rcmd_safe_env(), timeout = Inf, package = FALSE,
              arch = "same", ...) {

  ## This contains the context that we set up in steps
  options <- convert_and_check_my_args(as.list(environment()))
  options$extra <- list(...)
  options$load_hook <- default_load_hook()

  ## This cleans up everything...
  on.exit(unlink(options$tmp_files, recursive = TRUE), add = TRUE)

  options <- setup_script_files(options)
  options <- setup_context(options)
  options <- setup_callbacks(options)
  options <- setup_r_binary_and_args(options)

  out <- run_r(options)

  get_result(output = out, options)
}
