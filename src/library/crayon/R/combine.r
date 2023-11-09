
#' Combine two or more ANSI styles
#'
#' Combine two or more styles or style functions into a new style function
#' that can be called on strings to style them.
#'
#' It does not usually make sense to combine two foreground
#' colors (or two background colors), because only the first one
#' applied will be used.
#'
#' It does make sense to combine different kind of styles,
#' e.g. background color, foreground color, bold font.
#'
#' The `$` operator can also be used to combine styles.
#' Note that the left hand side of `$` is a style function,
#' and the right hand side is the name of a style in [styles()].
#'
#' @param ... The styles to combine. They will be applied from
#'   right to left.
#' @return The combined style function.
#'
#' @export
#' @examples
#' ## Use style names
#' alert <- combine_styles("bold", "red4", "bgCyan")
#' cat(alert("Warning!"), "\n")
#'
#' ## Or style functions
#' alert <- combine_styles(bold, red, bgCyan)
#' cat(alert("Warning!"), "\n")
#'
#' ## Combine a composite style
#' alert <- combine_styles(bold, combine_styles(red, bgCyan))
#' cat(alert("Warning!"), "\n")
#'
#' ## Shorter notation
#' alert <- bold $ red $ bgCyan
#' cat(alert("Warning!"), "\n")

combine_styles <- function(...) {
  styles <- lapply(list(...),  use_or_make_style)
  all_ansi <- unlist(lapply(styles, attr, "_styles"), recursive = FALSE)
  make_crayon(all_ansi)
}

#' @rdname combine_styles
#' @param crayon A style function.
#' @param style A style name that is included in `names(styles())`.
#' @export
#' @method $ crayon

`$.crayon` <- function(crayon, style) {
  attr(crayon, "_styles") <-
    c(attr(crayon, "_styles"), data_env$my_styles[style])
  crayon
}
