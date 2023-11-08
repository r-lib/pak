
#' Linear quantities in a human readable string
#'
#' Use `pretty_num()` to format numbers `compute_num()` is the underlying
#' engine that may be useful for custom formatting.
#' 
#' @param number Numeric vector, number related to a linear quantity.
#' @param style Formatting style:
#'   * `"default"` is the original `pretty_num` formatting, and it always
#'     pads the output, so that all vector elements are of the same width,
#'   * `"nopad"` is similar, but does not pad the output,
#'   * `"6"` always uses 6 characters,
#'   The `"6"` style is useful if it is important that the output always
#'   has the same width (number of characters), e.g. in progress bars.
#'   See some examples below.
#' @return Character vector, the formatted sizes.
#'   For `compute_num`, a data frame with columns `amount`, `prefix`,
#'   `negative`.
#'
#' @export
#' @examples
#' numbers <- c(1337, 1.3333e-5, 13333337, 1333333337, 133333333337)
#' pretty_num(numbers)
#' pretty_num(numbers, style = "nopad")
#' pretty_num(numbers, style = "6")

pretty_num <- format_num$pretty_num

#' @rdname pretty_num
#' @param smallest_prefix A character scalar, the smallest prefix to use.
#' @export

compute_num <- format_num$compute_num
