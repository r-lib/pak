#' Color definition (like RGB) to a name
#'
#' @param color A scalar color that is usable as an input to `col2rgb()`
#'   (assumed to be in the sRGB color space).
#' @return A character string that is the closest named colors to the input
#'   color.  The output will have an attribute of alternate color names (named
#'   "alt").
#' @export
#' @importFrom grDevices col2rgb convertColor

pretty_color <- function(color) {
  stopifnot(length(color) == 1)
  if (is.na(color)) {
    structure(NA_character_, alt = NA_character_)
  } else {
    if (is.factor(color)) color <- as.character(color)
    stopifnot(is.character(color))
    color_rgb <- col2rgb(color)
    color_lab <- convertColor(t(color_rgb), from = "sRGB", to = "Lab", scale.in = 256)
    dist <- color_diff_cie76(
      color_lab,
      as.matrix(color_reference[, c("L", "a", "b")])
    )
    ret <- color_reference$name[dist == min(dist)][1]
    attr(ret, "alt") <- color_reference$name_alt[dist == min(dist)][[1]]
    ret
  }
}

#' @rdname pretty_color
#' @export
pretty_colour <- pretty_color

#' Color names, hexadecimal, and CIE Lab colorspace representations
#'
#' \describe{
#'   \item{hex}{hexadecimal color representation (without the # at the beginning)}
#'   \item{L,a,b}{CIE Lab colorspace representation of `hex`}
#'   \item{name}{Preferred human-readable name of the color}
#'   \item{name_alt}{All available human-readable names of the color}
#'   \item{roygbiv,basic,html,R,pantone,x11,ntc}{Source dataset containing the color}
#' }
#' @source {https://github.com/colorjs/color-namer} and R `colors()`
#' @keywords internal
#' @name color_reference
NULL

color_diff_cie76 <- function(color, refs) {
  d <- t(refs) - c(color)
  sqrt(colSums(d * d))
}
