
#' Run an R script
#'
#' It uses the `Rscript` program corresponding to the current R version,
#' to run the script. It streams `stdout` and `stderr` of the process.
#'
#' @inheritSection r Security considerations
#' @inheritParams rcmd
#' @param script Path of the script to run.
#' @param color Whether to use terminal colors in the child process,
#'    assuming they are active in the parent process.
#'
#' @export

rscript <- function(script, cmdargs = character(), libpath = .libPaths(),
                    repos = default_repos(),
                    stdout = NULL, stderr = NULL,
                    poll_connection = TRUE, echo = FALSE, show = TRUE,
                    callback = NULL, block_callback = NULL, spinner = FALSE,
                    system_profile = FALSE, user_profile = "project",
                    env = rcmd_safe_env(), timeout = Inf, wd = ".",
                    fail_on_status = TRUE, color = TRUE, ...) {

  load_hook <- rscript_load_hook_color(color)

  options <- convert_and_check_my_args(as.list(environment()))
  options$extra <- list(...)

  options <- setup_context(options)
  options <- setup_callbacks(options)

  options <- setup_rscript_binary_and_args(options)

  ## This cleans up everything...
  on.exit(unlink(options$tmp_files, recursive = TRUE), add = TRUE)

  invisible(run_r(options))
}

rscript_load_hook_color <- function(color) {

  if (!color) return("")

  nc <- tryCatch(
    cli::num_ansi_colors(),
    error = function(e) 1L
  )
  if (nc == 1) return("")

  expr <- substitute(
    options(crayon.enabled = TRUE, crayon.colors = `_nc_`),
    list("_nc_" = nc)
  )
  paste0(deparse(expr), "\n")
}

#' External `Rscript` process
#'
#' @description
#' An `Rscript script.R` command that runs in the background. This is an
#' R6 class that extends the [processx::process] class.
#'
#' @name rscript_process
#' @examplesIf FALSE
#' options <- rscript_process_options(script = "script.R")
#' rp <- rscript_process$new(options)
#' rp$wait()
#' rp$read_output_lines()
#' @export

rscript_process <- R6::R6Class(
  "rscript_process",
  inherit = processx::process,
  public = list(
    #' @description Create a new `Rscript` process.
    #' @param options A list of options created via
    #'   [rscript_process_options()].
    initialize = function(options)
      rscript_init(self, private, super, options),
    #' @description Clean up after an `Rsctipt` process, remove
    #' temporary files.
    finalize = function() {
      unlink(private$options$tmp_files, recursive = TRUE)
      if ("finalize" %in% ls(super)) super$finalize()
    }
  ),
  private = list(
    options = NULL
  )
)

rscript_init <- function(self, private, super, options) {

  options$load_hook <- rscript_load_hook_color(options$color)
  options <- convert_and_check_my_args(options)
  options <- setup_context(options)
  options <- setup_rscript_binary_and_args(options)

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
