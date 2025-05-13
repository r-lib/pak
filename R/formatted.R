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
