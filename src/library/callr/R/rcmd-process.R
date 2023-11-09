
#' External `R CMD` Process
#'
#' @description
#' An `R CMD *` command that runs in the background. This is an R6 class
#' that extends the [processx::process] class.
#'
#' @examplesIf FALSE
#' options <- rcmd_process_options(cmd = "config", cmdargs = "CC")
#' rp <- rcmd_process$new(options)
#' rp$wait()
#' rp$read_output_lines()
#' @export

rcmd_process <- R6::R6Class(
  "rcmd_process",
  inherit = processx::process,
  public = list(
    #' @description
    #' Start an `R CMD` process.
    #' @param options A list of options created via
    #'  [rcmd_process_options()].
    #' @return A new `rcmd_process` object.
    initialize = function(options)
      rcmdp_init(self, private, super, options),
    #' @description
    #' Clean up the temporary files created for an `R CMD` process.
    finalize = function() {
      unlink(private$options$tmp_files, recursive = TRUE)
      if ("finalize" %in% ls(super)) super$finalize()
    }
  ),
  private = list(
    options = NULL
  )
)

rcmdp_init <- function(self, private, super, options) {

  ## This contains the context that we set up in steps
  options <- convert_and_check_my_args(options)

  options <- setup_context(options)
  options <- setup_rcmd_binary_and_args(options)

  private$options <- options

  oldwd <- getwd()
  setwd(options$wd)
  on.exit(setwd(oldwd), add = TRUE)

  with_envvar(
    options$env,
    do.call(super$initialize, c(list(options$bin, options$real_cmdargs,
      stdout = options$stdout, stderr = options$stderr,
      poll_connection = options$poll_connection),
      options$extra))
  )

  invisible(self)
}
