os_type <- function() {
  .Platform$OS.type
}

get_tool <- function(prog) {
  if (os_type() == "windows") prog <- paste0(prog, ".exe")

  exe <- system.file(package = "zip", "bin", .Platform$r_arch, prog)
  if (exe == "") {
    pkgpath <- system.file(package = "zip")
    if (basename(pkgpath) == "inst") pkgpath <- dirname(pkgpath)
    exe <- file.path(pkgpath, "src", "tools", prog)
    if (!file.exists(exe)) return("")
  }
  exe
}

unzip_exe <- function() {
  get_tool("cmdunzip")
}

zip_exe <- function() {
  get_tool("cmdzip")
}

zip_data <- new.env(parent = emptyenv())

## R CMD check fix
super <- ""

#' Class for an external unzip process
#'
#' `unzip_process()` returns an R6 class that represents an unzip process.
#' It is implemented as a subclass of [processx::process].
#'
#' @section Using the `unzip_process` class:
#'
#' ```
#' up <- unzip_process()$new(zipfile, exdir = ".", poll_connection = TRUE,
#'                            stderr = tempfile(), ...)
#' ```
#'
#' See [processx::process] for the class methods.
#'
#' Arguments:
#' * `zipfile`: Path to the zip file to uncompress.
#' * `exdir`: Directory to uncompress the archive to. If it does not
#'   exist, it will be created.
#' * `poll_connection`: passed to the `initialize` method of
#'   [processx::process], it allows using [processx::poll()] or the
#'   `poll_io()` method to poll for the completion of the process.
#' * `stderr`: passed to the `initialize` method of [processx::process],
#'   by default the standard error is written to a temporary file.
#'   This file can be used to diagnose errors if the process failed.
#' * `...` passed to the `initialize` method of [processx::process].
#'
#' @return An `unzip_process` R6 class object, a subclass of
#' [processx::process].
#'
#' @export
#' @examples
#' ex <- system.file("example.zip", package = "zip")
#' tmp <- tempfile()
#' up <- unzip_process()$new(ex, exdir = tmp)
#' up$wait()
#' up$get_exit_status()
#' dir(tmp)

unzip_process <- function() {
  need_packages(c("processx", "R6"), "creating unzip processes")
  zip_data$unzip_class <- zip_data$unzip_class %||%
    R6::R6Class(
      "unzip_process",
      inherit = processx::process,
      public = list(
        initialize = function(
          zipfile,
          exdir = ".",
          poll_connection = TRUE,
          stderr = tempfile(),
          ...
        ) {
          stopifnot(
            is_string(zipfile),
            is_string(exdir)
          )
          exdir <- normalizePath(exdir, winslash = "\\", mustWork = FALSE)
          super$initialize(
            unzip_exe(),
            enc2c(c(zipfile, exdir)),
            poll_connection = poll_connection,
            stderr = stderr,
            ...
          )
        }
      ),
      private = list()
    )

  zip_data$unzip_class
}

#' Class for an external zip process
#'
#' `zip_process()` returns an R6 class that represents a zip process.
#' It is implemented as a subclass of [processx::process].
#'
#' @section Using the `zip_process` class:
#'
#' ```
#' zp <- zip_process()$new(zipfile, files, recurse = TRUE,
#'                          poll_connection = TRUE,
#'                          stderr = tempfile(), ...)
#' ```
#'
#' See [processx::process] for the class methods.
#'
#' Arguments:
#' * `zipfile`: Path to the zip file to create.
#' * `files`: List of file to add to the archive. Each specified file
#'    or directory in is created as a top-level entry in the zip archive.
#' * `recurse`: Whether to add the contents of directories recursively.
#' * `include_directories`: Whether to explicitly include directories
#'   in the archive. Including directories might confuse MS Office when
#'   reading docx files, so set this to `FALSE` for creating them.
#' * `poll_connection`: passed to the `initialize` method of
#'   [processx::process], it allows using [processx::poll()] or the
#'   `poll_io()` method to poll for the completion of the process.
#' * `stderr`: passed to the `initialize` method of [processx::process],
#'   by default the standard error is written to a temporary file.
#'   This file can be used to diagnose errors if the process failed.
#' * `...` passed to the `initialize` method of [processx::process].
#'
#' @return A `zip_process` R6 class object, a subclass of
#' [processx::process].
#'
#' @export
#' @examples
#' dir.create(tmp <- tempfile())
#' write.table(iris, file = file.path(tmp, "iris.ssv"))
#' zipfile <- tempfile(fileext = ".zip")
#' zp <- zip_process()$new(zipfile, tmp)
#' zp$wait()
#' zp$get_exit_status()
#' zip_list(zipfile)

zip_process <- function() {
  need_packages(c("processx", "R6"), "creating zip processes")
  zip_data$zip_class <- zip_data$zip_class %||%
    R6::R6Class(
      "zip_process",
      inherit = processx::process,
      public = list(
        initialize = function(
          zipfile,
          files,
          recurse = TRUE,
          include_directories = TRUE,
          poll_connection = TRUE,
          stderr = tempfile(),
          ...
        ) {
          private$zipfile <- zipfile
          private$files <- files
          private$recurse <- recurse
          private$include_directories <- include_directories
          private$params_file <- tempfile()
          write_zip_params(
            files,
            recurse,
            include_directories,
            private$params_file
          )
          super$initialize(
            zip_exe(),
            enc2c(c(zipfile, private$params_file)),
            poll_connection = poll_connection,
            stderr = stderr,
            ...
          )
        }
      ),
      private = list(
        zipfile = NULL,
        files = NULL,
        recurse = NULL,
        include_directories = NULL,
        params_file = NULL
      )
    )

  zip_data$zip_class
}

write_zip_params <- function(files, recurse, include_directories, outfile) {
  data <- get_zip_data(
    files,
    recurse,
    keep_path = FALSE,
    include_directories = include_directories
  )
  mtime <- as.double(file.info(data$file)$mtime)

  con <- file(outfile, open = "wb")
  on.exit(close(con))

  ## Number of files
  writeBin(con = con, as.integer(nrow(data)))

  ## Key, first total length
  data$key <- data$key <- fix_absolute_paths(data$key)
  warn_for_colon(data$key)
  warn_for_dotdot(data$key)
  writeBin(con = con, as.integer(sum(nchar(data$key, type = "bytes") + 1L)))
  writeBin(con = con, data$key)

  ## Filenames
  writeBin(con = con, as.integer(sum(nchar(data$file, type = "bytes") + 1L)))
  writeBin(con = con, data$file)

  ## Is dir or not
  writeBin(con = con, as.integer(data$dir))

  ## mtime
  writeBin(con = con, as.double(mtime))
}
