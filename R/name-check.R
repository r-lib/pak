
#' Check if an R package name is available
#'
#' @inherit pkgdepends::pkg_name_check description details return
#'
#' @inheritParams pkgdepends::pkg_name_check
#'
#' @export
#' @section Examples:
#' ```{asciicast pkg-name-check}
#' pkg_name_check("sicily")
#' ```

pkg_name_check <- function(name, dictionaries = NULL) {
  remote(
    function(...) {
      ret <- pkgdepends::pkg_name_check(...)
      asNamespace("pak")$pak_preformat(ret)
    },
    list(name, dictionaries)
  )
}
