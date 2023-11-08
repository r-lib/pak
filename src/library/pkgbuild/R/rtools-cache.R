#' @export
#' @rdname has_rtools
rtools_path <- function() {
  if (!is_windows()) {
    return(NA_character_)
  }

  if (!rtools_path_is_set()) {
    has_rtools()
  }

  cache_get("rtools_path")
}

rtools_path_is_set <- function() {
  cache_exists("rtools_path")
}

rtools_path_set <- function(rtools) {
  stopifnot(is.rtools(rtools))
  path <- file.path(rtools$path, version_info[[rtools$version]]$path)

  # If using gcc49 and _without_ a valid BINPREF already set
  # Do NOT set BINPREF anymore for R 4.0 / rtools40
  if (!is_R4() && using_gcc49() && is.null(rtools$valid_binpref)) {
    Sys.setenv(BINPREF = file.path(rtools$path, "mingw_$(WIN)", "bin", "/"))
  }

  cache_set("rtools_path", path)
}

using_gcc49 <- function() {
  grepl("4.9.3", Sys.getenv("R_COMPILED_BY"), fixed = TRUE)
}
