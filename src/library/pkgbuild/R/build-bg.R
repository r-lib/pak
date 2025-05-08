#' Build package in the background
#'
#' This R6 class is a counterpart of the [build()] function, and
#' represents a background process that builds an R package.
#'
#' @section Usage:
#' ```
#' bp <- pkgbuild_process$new(path = ".", dest_path = NULL,
#'          binary = FALSE, vignettes = TRUE, manual = FALSE, args = NULL)
#' bp$get_dest_path()
#' ```
#'
#' Other methods are inherited from [callr::rcmd_process] and
#' `processx::process`.
#'
#' @section Arguments:
#' See the corresponding arguments of [build()].
#'
#' @section Details:
#' Most methods are inherited from [callr::rcmd_process] and
#' `processx::process`.
#'
#' `bp$get_dest_path()` returns the path to the built package.
#'
#' @section Examples:
#' ```
#' ## Here we are just waiting, but in a more realistic example, you
#' ## would probably run some other code instead...
#' bp <- pkgbuild_process$new("mypackage", dest_path = tempdir())
#' bp$is_alive()
#' bp$get_pid()
#' bp$wait()
#' bp$read_all_output_lines()
#' bp$read_all_error_lines()
#' bp$get_exit_status()
#' bp$get_dest_path()
#' ```
#'
#' @importFrom R6 R6Class
#' @name pkgbuild_process
NULL

#' @export

pkgbuild_process <- R6Class(
  "pkgbuild_process",
  inherit = callr::rcmd_process,
  public = list(
    initialize = function(
      path = ".",
      dest_path = NULL,
      binary = FALSE,
      vignettes = TRUE,
      manual = FALSE,
      clean_doc = NULL,
      args = NULL,
      needs_compilation = pkg_has_src(path),
      compile_attributes = FALSE,
      register_routines = FALSE,
      quiet = FALSE
    ) {
      rcb_init(
        self,
        private,
        super,
        path,
        dest_path,
        binary,
        vignettes,
        manual,
        clean_doc,
        args,
        needs_compilation,
        compile_attributes,
        register_routines,
        quiet
      )
    },
    is_incomplete_error = function() FALSE,
    read_all_error = function() "",
    read_all_error_lines = function() character(),
    read_error = function(n = -1) "",
    read_error_lines = function(n = -1) character(),
    get_dest_path = function() private$dest_path,
    get_built_file = function() {
      if (self$is_alive()) stop("Still alive")
      if (self$get_exit_status() != 0) stop("Build process failed")

      ## Already copied?
      if (!is.null(private$out_file)) {
        return(private$out_file)
      }

      ## No, copy, and remove temp dir, order is important here!
      file_name <- dir(private$out_dir)
      tmp_file <- file.path(private$out_dir, file_name)
      file.copy(tmp_file, private$dest_path, overwrite = TRUE)
      private$out_file <- file.path(private$dest_path, file_name)
      unlink(private$out_dir, recursive = TRUE)
      private$out_file
    },
    kill = function(...) {
      unlink(private$makevars_file)
      super$kill(...)
    }
  ),
  private = list(
    finalize = function() {
      unlink(private$makevars_file)
      super$kill()
    },
    path = NULL,
    dest_path = NULL,
    out_dir = NULL,
    out_file = NULL,
    makevars_file = NULL
  )
)

rcb_init <- function(
  self,
  private,
  super,
  path,
  dest_path,
  binary,
  vignettes,
  manual,
  clean_doc,
  args,
  needs_compilation,
  compile_attributes,
  register_routines,
  quiet
) {
  options <- build_setup(
    path,
    dest_path,
    binary,
    vignettes,
    manual,
    clean_doc,
    args,
    needs_compilation,
    compile_attributes,
    register_routines,
    quiet
  )

  private$path <- options$path
  private$dest_path <- options$dest_path
  private$out_dir <- options$out_dir
  private$makevars_file <- tempfile()

  ## Build tools already checked in setup

  withr_set_makevars(
    compiler_flags(debug = FALSE),
    new_path = private$makevars_file
  )
  withr_with_envvar(
    c("R_MAKEVARS_USER" = private$makevars_file),
    {
      options <- callr::rcmd_process_options(
        cmd = options$cmd,
        cmdargs = c(options$path, options$args),
        wd = options$out_dir,
        stderr = "2>&1"
      )

      super$initialize(options)

      invisible(self)
    }
  )
}
