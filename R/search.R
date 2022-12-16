
#' Search CRAN packages
#'
#' Search the indexed database of current CRAN packages. It uses the
#' pkgsearch package. See that package for more details and also
#' [pkgsearch::pkg_search()] for pagination, more advanced searching,
#' etc.
#'
#' @param query Search query string.
#' @param ... Additional arguments passed to [pkgsearch::pkg_search()]
#' @return A data frame, that is also a `pak_search_result` object
#' with a custom print method. To see the underlying table, you
#' can use `[]` to drop the extra classes. See examples below.
#'
#' @export
#' @section Examples:
#' Simple search
#' ```{asciicast pkg-search, R.options = list(width = 72)}
#' pkg_search("survival")
#'```
#'
#' See the underlying data frame
#' ```{asciicast pkg-search-2, R.options = list(width = 72)}
#' psro <- pkg_search("ropensci")
#' psro[]
#' ```

pkg_search <- function(query, ...) {
  load_extra("pillar")
  remote(
    function(...) {
      get("pkg_search_internal", asNamespace("pak"))(...)
    },
    list(query = query, ...)
  )
}

pkg_search_internal <- function(query, ...) {
  res <- pkgsearch::pkg_search(query, ...)
  res$ago <- prettyunits::time_ago(res$date)
  class(res) <- c("pak_search_result", class(res))
  res
}

#' @export

print.pak_search_result <- function(x, ...) {
  catln("")
  if (nrow(x) == 0) {
    catln("x No result. :(")
    return(invisible(x))
  }

  md <- attr(x, "metadata")
  catln(
    "# '", md$query, "' -- hits ", md$from, "-",
    md$from + md$size - 1, " of ", md$total
  )

  num <- as.character(seq(md$from, md$from + md$size - 1))
  for (i in seq_len(nrow(x))) {
    r <- x[i,]
    catln("")
    catln(num[i], " ", r$package, " ", as.character(r$version), " -- by ",
        r$maintainer_name, ", ", r$ago)
    catln(paste(strwrap(r$title, indent = 2), collapse = " "))
  }

  invisible(x)
}

#' @export

`[.pak_search_result` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), c("pak_search_result", "pkg_search_result"))
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
  remote(
    function(...) {
      get("pkg_history_internal", asNamespace("pak"))(...)
    },
    list(pkg = pkg)
  )
}

pkg_history_internal <- function(pkg) {
  pkgsearch::cran_package_history(pkg)
}
