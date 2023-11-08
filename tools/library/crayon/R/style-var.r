
#' Add style to a string
#'
#' See `names(styles)`, or the crayon manual for available styles.
#'
#' @param string Character vector to style.
#' @param as Style function to apply, either the function object,
#'   or its name, or an object to pass to [make_style()].
#' @param bg Background style, a style function, or a name that
#'   is passed to [make_style()].
#' @return Styled character vector.
#'
#' @export
#' @importFrom methods is
#'
#' @examples
#' ## These are equivalent
#' style("foobar", bold)
#' style("foobar", "bold")
#' bold("foobar")

style <- function(string, as = NULL, bg = NULL) {

  as <- use_or_make_style(as)
  bg <- use_or_make_style(bg, bg = TRUE)

  if (!is(as, "crayon")) stop("Cannot make style from 'as'")
  if (!is(bg, "crayon")) stop("Cannot make style from 'bg'")

  as(bg(string))
}

#' @importFrom methods is

use_or_make_style <- function(style, bg = FALSE) {
  if (is.null(style)) {
    structure(base::identity, class = "crayon")
  } else if (is(style, "crayon")) {
    style
  } else if (style %in% names(styles())) {
    make_crayon(styles()[style])
  } else {
    make_style(style, bg = bg)
  }
}
