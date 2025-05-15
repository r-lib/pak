is_valid_package <- function(file) {
  if (!file.exists(file)) {
    FALSE
  } else if (grepl("\\.zip$", file)) {
    is_valid_package_zip(file)
  } else if (grepl("\\.tgz$|\\.tar\\.gz$", file)) {
    is_valid_package_targz(file)
  } else {
    ## Just ignore other files
    FALSE
  }
}

is_valid_package_zip <- function(file) {
  if (file.info(file)$size == 0) return(FALSE)
  tryCatch(
    is_package_file_list(file, suppressWarnings(zip_list(file))),
    error = function(e) FALSE
  )
}

#' @importFrom utils untar

is_valid_package_targz <- function(file) {
  if (file.info(file)$size == 0) return(FALSE)
  con <- gzfile(file, open = "rb")
  on.exit(close(con), add = TRUE)
  tryCatch(
    is_package_file_list(file, untar(con, list = TRUE)),
    error = function(e) FALSE
  )
}

is_package_file_list <- function(file, list) {
  pkgname <- pkg_name_from_file(file)

  ## A single directory, named after the package
  if (any(!grepl(paste0("^", pkgname, "\\b"), list))) return(FALSE)

  ## DESCRIPTION file
  if (!paste0(pkgname, "/DESCRIPTION") %in% list) return(FALSE)

  return(TRUE)
}

pkg_name_from_file <- function(x) {
  sub("^([a-zA-Z0-9\\.]+)_.*$", "\\1", basename(x))
}
