#' `cat()` helpers
#'
#' These helpers provide useful wrappers around [cat()]: most importantly
#' they all set `sep = ""`, and `cat_line()` automatically adds a newline.
#'
#' @export
#' @param ... For `cat_line()` and `cat_bullet()`, pasted together with
#'   `collapse = "\n"`. For `cat_rule()` and `cat_boxx()` passed on to
#'   [rule()] and [boxx()] respectively.
#' @param bullet Name of bullet character. Indexes into [symbol]
#' @param col,background_col,bullet_col Colors for text, background, and
#'   bullets respectively.
#' @param x An object to print.
#' @param file Output destination. Defaults to standard output.
#' @examples
#' cat_line("This is ", "a ", "line of text.", col = "red")
#' cat_bullet(letters[1:5])
#' cat_bullet(letters[1:5], bullet = "tick", bullet_col = "green")
#' cat_rule()
cat_line <- function(..., col = NULL, background_col = NULL, file = stdout()) {
  out <- paste0(..., collapse = "\n")
  out <- apply_style(out, col)
  out <- apply_style(out, background_col, bg = TRUE)

  cat(out, "\n", sep = "", file = file, append = TRUE)
}

#' @export
#' @rdname cat_line
cat_bullet <- function(
  ...,
  col = NULL,
  background_col = NULL,
  bullet = "bullet",
  bullet_col = NULL,
  file = stdout()
) {
  out <- apply_style(paste0(...), col)
  bullet <- apply_style(symbol[[bullet]], bullet_col)

  cat_line(paste(bullet, out), background_col = background_col, file = file)
}

#' @export
#' @rdname cat_line
cat_boxx <- function(..., file = stdout()) {
  cat_line(boxx(...), file = file)
}

#' @export
#' @rdname cat_line
cat_rule <- function(..., file = stdout()) {
  cat_line(rule(...), file = file)
}

#' @export
#' @rdname cat_line
cat_print <- function(x, file = "") {
  if (!identical(file, "")) {
    sink(file)
    on.exit(sink(NULL))
  }

  print(x)
}
