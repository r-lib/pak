#' Search CRAN packages
#'
#' Search the indexed database of current CRAN packages. It uses the
#' pkgsearch package. See that package for more details and also
#' [pkgsearch::pkg_search()] for pagination, more advanced searching,
#' etc.
#'
#' @param query Search query string.
#' @inheritDotParams pkgsearch::pkg_search size from
#' @return A data frame, that is also a `pak_search_result` object
#' with a custom print method. To see the underlying table, you
#' can use `[]` to drop the extra classes. See examples below.
#'
#' @export
#' @section Examples:
#' Simple search
#' ```{asciicast pkg-search, R.options = list(width = 72)}
#' pkg_search("survival")
#' ```
#'
#' See the underlying data frame
#' ```{asciicast pkg-search-2, R.options = list(width = 72)}
#' psro <- pkg_search("ropensci")
#' psro[]
#' ```

pkg_search <- function(query = NULL, ...) {
  load_extra("pillar")
  load_all_private()
  ret <- withVisible(
    pkg_data[["ns"]][["pkgsearch"]][["pkg_search"]](query, ...)
  )

  class(ret$value) <- c("pak_pkg_search_result", class(ret$value))
  if (ret$visible) ret$value else invisible(ret$value)
}

#' @export

print.pak_pkg_search_result <- function(x, ...) {
  load_all_private()
  pkg_data[["ns"]][["pkgsearch"]][["print.pkg_search_result"]](x, ...)
}

#' @export

summary.pak_pkg_search_result <- function(object, ...) {
  load_all_private()
  pkg_data[["ns"]][["pkgsearch"]][["summary.pkg_search_result"]](
    object,
    ...
  )
}

#' @export

`[.pak_pkg_search_result` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), c("pak_pkg_search_result", "pkg_search_result"))
  NextMethod("[")
}


#' Query the history of a CRAN package
#'
#' @param pkg Package name.
#' @return A data frame, with one row per package version. The columns are
#'   the entries of the `DESCRIPTION` files in the released package
#'   versions.
#' @export
#' @section Examples:
#' ```{asciicast pkg-history, R.options = list(width = 72)}
#' pkg_history("ggplot2")
#' ```

pkg_history <- function(pkg) {
  load_extra("pillar")
  load_all_private()

  pkg_data[["ns"]][["pkgsearch"]][["cran_package_history"]](pkg)
}
