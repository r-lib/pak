#' Temporarily set debugging compilation flags.
#'
#' @param code to execute.
#' @param CFLAGS flags for compiling C code
#' @param CXXFLAGS flags for compiling C++ code
#' @param FFLAGS flags for compiling Fortran code.
#' @param FCFLAGS flags for Fortran 9x code.
#' @inheritParams compiler_flags
#' @family debugging flags
#' @export
#' @examples
#' flags <- names(compiler_flags(TRUE))
#' with_debug(Sys.getenv(flags))
#' \dontrun{
#' install("mypkg")
#' with_debug(install("mypkg"))
#' }
with_debug <- function(
  code,
  CFLAGS = NULL,
  CXXFLAGS = NULL,
  FFLAGS = NULL,
  FCFLAGS = NULL,
  debug = TRUE
) {
  defaults <- compiler_flags(debug = debug)
  flags <- c(
    CFLAGS = CFLAGS,
    CXXFLAGS = CXXFLAGS,
    FFLAGS = FFLAGS,
    FCFLAGS = FCFLAGS
  )

  flags <- unlist(utils::modifyList(as.list(defaults), as.list(flags)))

  withr_with_makevars(flags, code)
}

#' Tools for testing pkgbuild
#'
#' `with_compiler` temporarily disables code compilation by setting
#' `CC`, `CXX`, makevars to `test`. `without_cache`
#' resets the cache before and after running `code`.
#'
#' @param code Code to execute with broken compilers
#' @export
without_compiler <- function(code) {
  flags <- c(
    CC = "test",
    CXX = "test",
    CXX11 = "test",
    FC = "test"
  )

  if (is_windows()) {
    without_cache({
      cache_set("rtools_path", "")
      withr_with_makevars(flags, code)
    })
  } else {
    without_cache({
      withr_with_makevars(flags, code)
    })
  }
}


#' @export
#' @rdname without_compiler
without_cache <- function(code) {
  cache_reset()
  on.exit(cache_reset())

  code
}


#' @export
#' @rdname without_compiler
without_latex <- function(code) {
  withr_with_options(list(PKGBUILD_TEST_FIXTURE_HAS_LATEX = FALSE), code)
}


#' @export
#' @rdname without_compiler
with_latex <- function(code) {
  withr_with_options(list(PKGBUILD_TEST_FIXTURE_HAS_LATEX = TRUE), code)
}
