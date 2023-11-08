
#' Bytes in a human readable string
#'
#' Use `pretty_bytes()` to format bytes. `compute_bytes()` is the underlying
#' engine that may be useful for custom formatting.
#' 
#' @param bytes Numeric vector, number of bytes.
#' @param style Formatting style:
#'   * `"default"` is the original `pretty_bytes` formatting, and it always
#'     pads the output, so that all vector elements are of the same width,
#'   * `"nopad"` is similar, but does not pad the output,
#'   * `"6"` always uses 6 characters,
#'   The `"6"` style is useful if it is important that the output always
#'   has the same width (number of characters), e.g. in progress bars.
#'   See some examples below.
#' @return Character vector, the formatted sizes.
#'   For `compute_bytes`, a data frame with columns `amount`, `unit`,
#'   `negative`.
#'
#' @export
#' @examples
#' bytes <- c(1337, 133337, 13333337, 1333333337, 133333333337)
#' pretty_bytes(bytes)
#' pretty_bytes(bytes, style = "nopad")
#' pretty_bytes(bytes, style = "6")

pretty_bytes <- format_bytes$pretty_bytes

#' @rdname pretty_bytes
#' @param smallest_unit A character scalar, the smallest unit to use.
#' @export

compute_bytes <- format_bytes$compute_bytes
