
is_package_archive <- function(file) {
  (is_zip_file(file) || is_tar_gz_file(file)) &&
    is_valid_package_file_name(file)
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
  if (!is_gz_file(file)) return(FALSE)
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

is_valid_package_file_name <- function(filename) {
  grepl(valid_package_archive_name, basename(filename))
}

#' @importFrom utils untar unzip

con_unzip <- function(archive, pkgname) {
  filename <-  paste0(pkgname, "/", "DESCRIPTION")
  con <- unz(archive, filename)
  on.exit(close(con), add = TRUE)
  tmp <- tempfile()
  writeLines(readLines(con), tmp)
  tmp
}

con_untar <- function(archive, pkgname) {
  filename <- paste0(pkgname, "/", "DESCRIPTION")
  tmp <- tempfile()
  suppressWarnings(
    untar(con <- gzfile(archive, open = "rb"), files = filename, exdir = tmp)
  )
  on.exit(close(con), add = TRUE)
  file.path(tmp, pkgname, "DESCRIPTION")
}

get_description_from_package <- function(file) {
  package_name <- sub("_.*$", "", basename(file))

  uncompress <- if (is_zip_file(file)) con_unzip else con_untar
  desc <- uncompress(file, package_name)

  if (!file.exists(desc)) {
    stop("Cannot extract DESCRIPTION from ", sQuote(file))
  }

  desc
}
