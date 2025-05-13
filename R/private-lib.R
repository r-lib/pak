#' The directory of the private library
#'
#' This is usually within the package, but in dev mode in can be
#' in the user's cache directory or a manually chosen directory during
#' tests.
#'
#' @noRd

private_lib_dir <- function() {
  mydir <- getNamespaceInfo(asNamespace(.packageName), "path")
  embedded <- file.path(mydir, "library")

  if (file.exists(embedded)) return(c(embedded = embedded))
  ppl <- Sys.getenv("PAK_PRIVATE_LIBRARY", NA_character_)
  if (!is.na(ppl)) return(ppl)

  file.path(
    user_cache_dir("pak"),
    "lib",
    get_minor_r_version(),
    R.Version()$arch
  )
}

#' Attach pak's internal library to the search path
#'
#' This should be only called in a pak subprocess, from `.onLoad()`.
#'
#' @noRd

use_private_lib <- function() {
  lib <- private_lib_dir()
  old <- .libPaths()
  new <- c(lib, old[old != lib])
  .libPaths(new)
}
