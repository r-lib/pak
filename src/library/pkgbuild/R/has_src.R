#' Does a source package have `src/` directory?
#'
#' If it does, you definitely need build tools.
#'
#' @param path Path to package (or directory within package).
#' @export
pkg_has_src <- function(path = ".") {
  if (is_dir(path)) {
    src_path <- file.path(pkg_path(path), "src")
    file.exists(src_path)
  } else {
    tryCatch(
      {
        files <- if (is_zip_file(path)) {
          utils::unzip(path, list = TRUE)$Name
        } else if (is_tar_gz_file(path)) {
          utils::untar(path, list = TRUE)
        } else {
          stop("not a zip or tar.gz file")
        }

        if (!any(grepl("^[^/]+/DESCRIPTION$", files))) {
          stop("no DESCRIPTION file")
        }

        any(grepl("^[^/]+/src/?$", files))
      },
      error = function(e) {
        e$message <- paste(
          path,
          "is not a valid package archive file,",
          e$message
        )
        stop(e)
      }
    )
  }
}

is_zip_file <- function(file) {
  buf <- readBin(file, what = "raw", n = 4)
  length(buf) == 4 &&
    buf[1] == 0x50 &&
    buf[2] == 0x4b &&
    (buf[3] == 0x03 || buf[3] == 0x05 || buf[5] == 0x07) &&
    (buf[4] == 0x04 || buf[4] == 0x06 || buf[4] == 0x08)
}

is_gz_file <- function(file) {
  buf <- readBin(file, what = "raw", n = 3)
  length(buf) == 3 &&
    buf[1] == 0x1f &&
    buf[2] == 0x8b &&
    buf[3] == 0x08
}

is_tar_gz_file <- function(file) {
  if (!is_gz_file(file)) {
    return(FALSE)
  }
  con <- gzfile(file, open = "rb")
  on.exit(close(con))
  buf <- readBin(con, what = "raw", n = 262)
  length(buf) == 262 &&
    buf[258] == 0x75 &&
    buf[259] == 0x73 &&
    buf[260] == 0x74 &&
    buf[261] == 0x61 &&
    buf[262] == 0x72
}
