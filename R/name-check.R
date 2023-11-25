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
  load_all_private()
  ret <- pkg_data[["ns"]][["pkgdepends"]][["pkg_name_check"]](
    name,
    dictionaries
  )

  class(ret) <- c("pak_pkg_name_check", class(ret))
  ret
}

#' @export

format.pak_pkg_name_check <- function(x, ...) {
  load_all_private()
  pkg_data[["ns"]][["pkgdepends"]][["format.pkg_name_check"]](x, ...)
}

#' @export

print.pak_pkg_name_check <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}
