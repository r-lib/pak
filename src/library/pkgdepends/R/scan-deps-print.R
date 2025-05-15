#' @export

format.pkg_scan_deps <- function(x, ...) {
  labels <- c(
    prod = "Dependencies",
    test = "Test dependencies",
    dev = "Development dependencies",
    # TODO: generic label for others
    NULL
  )
  lns <- lapply(seq_along(labels), function(i) {
    deps <- x[x$type == names(labels)[i], , drop = FALSE]
    if (nrow(deps) == 0) return(NULL)
    fls <- tapply(deps$path, deps$package, "c", simplify = FALSE)
    fls[] <- lapply(fls, unique)
    fls <- vcapply(fls, paste, collapse = ", ")
    pkg <- format(names(fls))
    flsw <- cli::console_width() - nchar(pkg[1]) - 5
    c(
      "",
      cli::col_yellow(paste0(labels[i], ":")),
      paste0(
        cli::col_grey("+ "),
        cli::col_blue(pkg),
        cli::col_grey(" @ "),
        cli::col_silver(cli::ansi_strtrim(fls, flsw))
      )
    )
  })

  unlist(lns)
}

#' @export

print.pkg_scan_deps <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

`[.pkg_scan_deps` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pkg_scan_deps")
  NextMethod("[")
}
