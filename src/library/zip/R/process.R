os_type <- function() {
  .Platform$OS.type
}

get_tool <- function(prog) {
  if (os_type() == "windows") {
    prog <- paste0(prog, ".exe")
  }

  exe <- system.file(package = "zip", "bin", .Platform$r_arch, prog)
  if (exe == "") {
    pkgpath <- find.package("zip")
    if (basename(pkgpath) == "inst") {
      pkgpath <- dirname(pkgpath)
    }
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

can_run_unzip_exe <- function() {
  if (isTRUE(is_true_env_var("R_ZIP_PROCESS_FALLBACK"))) {
    return(FALSE)
  }
  if (!is.null(zip_data$unzip_exe_works)) {
    return(zip_data$unzip_exe_works)
  }
  if (.Platform$OS.type != "windows") {
    return(TRUE)
  }
  exe <- unzip_exe()
  zip_data$unzip_exe_works <- if (exe == "") {
    FALSE
  } else {
    tryCatch(
      {
        p <- processx::process$new(
          exe,
          "--test",
          stdout = NULL,
          stderr = NULL
        )
        p$wait(5000)
        p$kill()
        identical(p$get_exit_status(), 0L)
      },
      error = function(e) FALSE
    )
  }
  zip_data$unzip_exe_works
}

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
#' [processx::process], or a subclass of [callr::r_process] when the fallback
#' is active (see the Fallback section below).
#'
#' @section Fallback:
#' `unzip_process()` normally runs the bundled `cmdunzip` native executable
#' via [processx::process]. If the executable cannot be found or fails its
#' self-test it falls back to running [unzip()] in a background R process
#' via [callr::r_process]. This may happen when system policies do not
#' allow starting the `cmdunzip` executable., The fallback class has the
#' same interface but inherits from [callr::r_process] instead of
#' [processx::process].
#'
#' Set the environment variable `R_ZIP_PROCESS_FALLBACK=true` to force the
#' fallback unconditionally.
#'
#' @section Encoding:
#' The `unzip_process` class does not support the `encoding` argument of
#' [unzip()]. Non-UTF-8 filenames are decoded using the IBM CP437 fallback.
#' Use [unzip()] directly if you need to handle ZIP files with filenames in
#' other encodings (e.g. CP932).
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
    {
      if (can_run_unzip_exe()) {
        make_unzip_process_class()
      } else {
        need_packages("callr", "creating unzip processes (fallback)")
        make_unzip_process_fallback_class()
      }
    }
  zip_data$unzip_class
}

# Subclass of processx::process that runs the bundled `cmdunzip` executable.
make_unzip_process_class <- function() {
  R6::R6Class(
    "unzip_process",
    inherit = processx::process,
    public = list(
      initialize = function(
        zipfile,
        exdir = ".",
        password = NULL,
        poll_connection = TRUE,
        stderr = tempfile(),
        ...
      ) {
        stopifnot(
          is_string(zipfile),
          is_string(exdir)
        )
        exdir <- normalizePath(exdir, winslash = "\\", mustWork = FALSE)
        pw <- resolve_password(password)
        args <- enc2c(c(zipfile, exdir))
        if (!is.null(pw)) {
          args <- c(args, raw_to_hex(pw))
        }
        super$initialize(
          unzip_exe(),
          args,
          poll_connection = poll_connection,
          stderr = stderr,
          ...
        )
      }
    ),
    private = list()
  )
}

# Fallback subclass of callr::r_process that unzips in a background R process,
# used when the `cmdunzip` executable is not available or does not run.
make_unzip_process_fallback_class <- function() {
  R6::R6Class(
    "unzip_process",
    inherit = callr::r_process,
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
        zipfile <- enc2c(normalizePath(zipfile))
        opts <- callr::r_process_options(
          func = function(zipfile, exdir) zip::unzip(zipfile, exdir = exdir),
          args = list(zipfile = zipfile, exdir = exdir),
          poll_connection = poll_connection,
          stderr = stderr,
          env = c(Sys.getenv(), ZIP_PROGRESS = "false")
        )
        super$initialize(opts)
      }
    ),
    private = list()
  )
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
#' * `files`: Character vector of paths to files to add to the archive.
#'    Each specified file or directory in is created as a top-level entry
#'    in the zip archive.
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
          password = NULL,
          encryption = "aes256",
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
          pw <- resolve_password(password)
          enc <- if (is.null(pw)) NULL else encryption_code(encryption)
          args <- enc2c(c(zipfile, private$params_file))
          if (!is.null(pw)) {
            args <- c(args, raw_to_hex(pw), as.character(enc))
          }
          super$initialize(
            zip_exe(),
            args,
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
