
#' External R Process
#'
#' @description
#' An R process that runs in the background. This is an R6 class that
#' extends the [processx::process] class. The process starts in the
#' background, evaluates an R function call, and then quits.
#'
#' @examplesIf FALSE
#' ## List all options and their default values:
#' r_process_options()
#'
#' ## Start an R process in the background, wait for it, get result
#' opts <- r_process_options(func = function() 1 + 1)
#' rp <- r_process$new(opts)
#' rp$wait()
#' rp$get_result()
#' @export

r_process <- R6::R6Class(
  "r_process",
  inherit = processx::process,
  public = list(

    #' @description
    #' Start a new R process in the background.
    #' @param options A list of options created via [r_process_options()].
    #' @return A new `r_process` object.
    initialize = function(options)
      rp_init(self, private, super, options),

    #' @description
    #' Return the result, an R object, from a finished
    #' background R process. If the process has not finished yet, it throws
    #' an error. (You can use `wait()` method (see [processx::process]) to
    #' wait for the process to finish, optionally with a timeout.) You can
    #' also use [processx::poll()] to wait for the end of the process,
    #' together with other processes or events.
    #'
    #' @return The return value of the R expression evaluated in the R
    #' process.
    get_result = function()
      rp_get_result(self, private),

    #' @description
    #' Clean up temporary files once an R process has finished and its
    #' handle is garbage collected.
    finalize = function() {
      unlink(private$options$tmp_files, recursive = TRUE)
      if ("finalize" %in% ls(super)) super$finalize()
    }
  ),
  private = list(
    options = NULL
  )
)

rp_init <- function(self, private, super, options) {

  ## This contains the context that we set up in steps
  options <- convert_and_check_my_args(options)

  options <- setup_script_files(options)
  options <- setup_context(options)
  options <- setup_r_binary_and_args(options)

  private$options <- options

  with_envvar(
    options$env,
    do.call(super$initialize, c(list(
      options$bin, options$real_cmdargs, stdout = options$stdout,
      stderr = options$stderr, poll_connection = options$poll_connection,
      supervise = options$supervise),
      options$extra))
  )

  invisible(self)
}

rp_get_result <- function(self, private) {
  if (self$is_alive()) throw(new_error("Still alive"))

  ## This is artificial...
  out <- list(
    status = self$get_exit_status(),
    stdout = "",
    stderr = "",
    timeout = FALSE
  )

  get_result(out, private$options)
}
