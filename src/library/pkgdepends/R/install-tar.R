#' Create a tar background process
#'
#' Use an external tar program, if there is a working one, otherwise use
#' the internal implementation.
#'
#' When using the internal implementation, we need to start another R
#' process.
#'
#' @param tarfile Tar file.
#' @param files Files or regular expressions to set what to extract. if
#'   `NULL` then everything is extracted.
#' @param exdir Where to extract the archive. It must exist.
#' @param restore_times Whether to restore file modification times.
#' @param post_process Function to call after the extraction.
#' @param stdout Standard output of process.
#' @param stderr Standard error of process.
#' @param ... Extra arguments for the process constructor.
#' @return The [processx::process] object.
#' @noRd

make_untar_process <- function(
  tarfile,
  files = NULL,
  exdir = ".",
  restore_times = TRUE,
  post_process = NULL,
  stdout = "|",
  stderr = "2>&1",
  ...
) {
  internal <- need_internal_tar()
  if (internal) {
    r_untar_process$new(
      tarfile,
      files,
      exdir,
      restore_times,
      post_process = post_process,
      stdout = stdout,
      stderr = stderr,
      ...
    )
  } else {
    external_untar_process$new(
      tarfile,
      files,
      exdir,
      restore_times,
      post_process = post_process,
      stdout = stdout,
      stderr = stderr,
      ...
    )
  }
}

#' Check if we need to use R's internal tar implementation
#'
#' This is slow, because we need to start an R child process, and the
#' implementation is also very slow. So it is better to use an external tar
#' program, if we can. We test this by trying to uncompress a `.tar.gz`
#' archive using the external program. The name of the tar program is
#' taken from the `TAR` environment variable, if this is unset then `tar`
#' is used.
#'
#' @return Whether we need to use the internal tar implementation.
#' @noRd

need_internal_tar <- local({
  internal <- NULL
  function() {
    if (!is.null(internal)) return(internal)

    mkdirp(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    tarfile <- system.file(package = .packageName, "tools", "pkg_1.0.0.tgz")

    tryCatch(
      p <- external_untar_process$new(tarfile, exdir = tmp),
      error = function(e) {
        internal <<- TRUE
      }
    )
    if (!is.null(internal)) return(internal)

    p$wait(timeout = 2000)
    p$kill()
    internal <<- p$get_exit_status() != 0 ||
      !file.exists(file.path(tmp, "pkg", "DESCRIPTION"))
    internal
  }
})

#' R6 class for an external un-tar process
#'
#' @description
#' Uses the system's `tar` program, in a background process.
#'
#' @noRd

external_untar_process <- R6::R6Class(
  "external_untar_process",
  inherit = processx::process,

  public = list(
    #' @details
    #' Start running the background process that extracts the file.
    #'
    #' @param tarfile Path to the `.tar` or `.tar.gz`, etc. file to
    #' uncompress.
    #' @param files List of files to uncompress, see [utils::untar()].
    #' @param exdir Directory to extract the files to.
    #' @param restore_times Whether to restore modification files.
    #' @param tar Name of the external `tar` program. Defaults to
    #' `TAR` environment variable, or `tar` if unset.
    #' @param stdout Standard output of process.
    #' @param stderr Standard error of process.
    #' @param post_process Function to call, once the extraction is
    #' done, or `NULL`
    #' @param ... Extra arguments for the process constructor.
    #' @return New `r_untar_process` object.

    initialize = function(
      tarfile,
      files = NULL,
      exdir = ".",
      restore_times = TRUE,
      tar = Sys.getenv("TAR", "tar"),
      stdout = "|",
      stderr = "2>&1",
      post_process = NULL,
      ...
    ) {
      private$options <- list(
        tarfile = normalizePath(tarfile),
        files = files,
        exdir = exdir,
        restore_times = restore_times,
        tar = tar
      )

      private$options$args <- eup_get_args(private$options)
      super$initialize(
        tar,
        private$options$args,
        post_process = post_process,
        stdout = stdout,
        stderr = stderr,
        ...
      )
      invisible(self)
    }
  ),

  private = list(
    options = NULL
  )
)

#' R6 class for an R un-tar process
#'
#' @description
#' Uses [utils::untar()], in a background process.
#'
#' @noRd

r_untar_process <- R6::R6Class(
  "r_untar_process",
  inherit = callr::r_process,

  public = list(
    #' @details
    #' Start running the background R process that extracts the file.
    #'
    #' @param tarfile Path to the `.tar` or `.tar.gz`, etc. file to
    #' uncompress.
    #' @param files List of files to uncompress, see [utils::untar()].
    #' @param exdir Directory to extract the files to.
    #' @param restore_times Whether to restore modification files.
    #' @param post_process Function to call, once the extraction is
    #' done, or `NULL`
    #' @param stdout Standard output of process.
    #' @param stderr Standard error of process.
    #' @param ... Extra arguments for the process cosntructor.
    #' @return New `r_untar_process` object.

    initialize = function(
      tarfile,
      files = NULL,
      exdir = ".",
      restore_times = TRUE,
      post_process = NULL,
      stdout = "|",
      stderr = "2>&1",
      ...
    ) {
      options <- list(
        tarfile = normalizePath(tarfile),
        files = files,
        exdir = exdir,
        restore_times = restore_times,
        tar = tar,
        post_process = post_process
      )

      process_options <- callr::r_process_options(
        stdout = stdout,
        stderr = stderr
      )
      process_options$func <- function(options) {
        # nocov start
        ret <- utils::untar(
          tarfile = options$tarfile,
          files = options$files,
          list = FALSE,
          exdir = options$exdir,
          compressed = NA,
          restore_times = options$restore_times,
          tar = "internal"
        )

        if (!is.null(options$post_process)) options$post_process() else ret
        # nocov end
      }
      process_options$args <- list(options = options)
      super$initialize(process_options)
    }
  ),

  private = list(
    options = NULL
  )
)

eup_get_args <- function(options) {
  c(
    "-x",
    "-f",
    path_norm(options$tarfile),
    "-C",
    path_norm(options$exdir),
    # do not restore ownership, this is problematic on some mounts, e.f. sshfs
    "-o",
    get_untar_decompress_arg(options$tarfile),
    if (!options$restore_times) "-m",
    options$files
  )
}

get_untar_decompress_arg <- function(tarfile) {
  type <- detect_package_archive_type(tarfile)
  switch(
    type,
    "gzip" = "-z",
    "bzip2" = "-j",
    "xz" = "-J",
    "zip" = throw(pkg_error(
      "{.path {tarfile}} is not a tar file, it looks like a zip file"
    )),
    "unknown" = character()
  )
}

detect_package_archive_type <- function(file) {
  buf <- readBin(file, what = "raw", n = 6)
  if (is_gzip(buf)) {
    "gzip"
  } else if (is_zip(buf)) {
    "zip"
  } else if (is_bzip2(buf)) {
    "bzip2"
  } else if (is_xz(buf)) {
    "xz"
  } else {
    "unknown"
  }
}

is_gzip <- function(buf) {
  if (!is.raw(buf)) buf <- readBin(buf, what = "raw", n = 3)
  length(buf) >= 3 &&
    buf[1] == 0x1f &&
    buf[2] == 0x8b &&
    buf[3] == 0x08
}

is_bzip2 <- function(buf) {
  if (!is.raw(buf)) buf <- readBin(buf, what = "raw", n = 3)
  length(buf) >= 3 &&
    buf[1] == 0x42 &&
    buf[2] == 0x5a &&
    buf[3] == 0x68
}

is_xz <- function(buf) {
  if (!is.raw(buf)) buf <- readBin(buf, what = "raw", n = 6)
  length(buf) >= 6 &&
    buf[1] == 0xFD &&
    buf[2] == 0x37 &&
    buf[3] == 0x7A &&
    buf[4] == 0x58 &&
    buf[5] == 0x5A &&
    buf[6] == 0x00
}

is_zip <- function(buf) {
  if (!is.raw(buf)) buf <- readBin(buf, what = "raw", n = 4)
  length(buf) >= 4 &&
    buf[1] == 0x50 &&
    buf[2] == 0x4b &&
    (buf[3] == 0x03 || buf[3] == 0x05 || buf[5] == 0x07) &&
    (buf[4] == 0x04 || buf[4] == 0x06 || buf[4] == 0x08)
}

make_uncompress_process <- function(archive, exdir = ".", ...) {
  type <- detect_package_archive_type(archive)

  if (type == "unknown") {
    throw(pkg_error(
      "Cannot extract {.path {archive}}, unknown archive type.",
      .class = "install_input_error"
    ))
  }

  if (type == "zip") {
    make_unzip_process(archive, exdir = exdir)
  } else {
    make_untar_process(archive, exdir = exdir)
  }
}

# This is similar but uses the async framework

run_uncompress_process <- function(archive, exdir = ".", ...) {
  type <- detect_package_archive_type(archive)
  if (type == "unknown") {
    throw(pkg_error(
      "Cannot extract {.path {archive}}, unknown archive type.",
      .class = "install_input_error"
    ))
  }

  stdout <- tempfile()
  if (type == "zip") {
    external_process(
      make_unzip_process,
      zipfile = archive,
      exdir = exdir,
      stdout = stdout,
      stderr = stdout
    )
  } else {
    external_process(
      make_untar_process,
      tarfile = archive,
      exdir = exdir,
      stdout = stdout,
      stderr = stdout
    )
  }
}
