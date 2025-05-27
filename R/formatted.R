#' @export

format.pak_preformatted <- function(x, ...) {
  attr(x, "pak_preformatted")
}

#' @export

print.pak_preformatted <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

pak_preformat <- function(x, ...) {
  attr(x, "pak_preformatted") <- format(x, ...)
  class(x) <- c("pak_preformatted", class(x))
  x
}

#' @export

`[.pak_preformatted` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pak_preformatted")
  NextMethod("[")
}
