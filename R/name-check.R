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
  ret <- embedded_call("pkgdepends", "pkg_name_check")(name, dictionaries)
  class(ret) <- c("pak_pkg_name_check", class(ret))
  ret
}

#' @export

format.pak_pkg_name_check <- function(x, limit = 6, ...) {
  load_all_private()
  # lots of S3 in pkgdepends for this, we need to do that manually here
  pd <- pkg_data[["ns"]][["pkgdepends"]]
  for (n in c("basics", "wikipedia", "wiktionary", "sentiment", "urban")) {
    if (!is.null(x[[n]])) {
      fn <- paste0("format.pkg_name_check_", n)
      x[[n]] <- pd[[fn]](x[[n]], limit = limit)
    }
  }
  unlist(x)
}

#' @export

print.pak_pkg_name_check <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}
