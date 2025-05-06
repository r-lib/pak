#' Unbox a vector or data frame
#'
#' This function marks an atomic vector or data frame as a
#' [singleton](https://en.wikipedia.org/wiki/Singleton_(mathematics)), i.e.
#' a set with exactly 1 element. Thereby, the value will not turn into an
#' `array` when encoded into JSON. This can only be done for
#' atomic vectors of length 1, or data frames with exactly 1 row. To automatically
#' unbox all vectors of length 1 within an object, use the `auto_unbox` argument
#' in [toJSON()].
#'
#' It is usually recommended to avoid this function and stick with the default
#' encoding schema for the various \R{} classes. The only use case for this function
#' is if you are bound to some specific predefined JSON structure (e.g. to
#' submit to an API), which has no natural \R{} representation. Note that the default
#' encoding for data frames naturally results in a collection of key-value pairs,
#' without using `unbox`.
#'
#' @param x atomic vector of length 1, or data frame with 1 row.
#' @return Returns a singleton version of `x`.
#' @export
#' @references <https://en.wikipedia.org/wiki/Singleton_(mathematics)>
#' @examples toJSON(list(foo=123))
#' toJSON(list(foo=unbox(123)))
#'
#' # Auto unbox vectors of length one:
#' x = list(x=1:3, y = 4, z = "foo", k = NULL)
#' toJSON(x)
#' toJSON(x, auto_unbox = TRUE)
#'
#' x <- iris[1,]
#' toJSON(list(rec=x))
#' toJSON(list(rec=unbox(x)))
unbox <- function(x) {
  if (is.null(x)) {
    return(x)
  }
  if (is.data.frame(x)) {
    if (nrow(x) == 1) {
      return(as.scalar(x))
    } else {
      stop("Tried to unbox dataframe with ", nrow(x), " rows.")
    }
  }
  if (length(x) == 1L && inherits(x, "POSIXt")) {
    return(as.scalar(x))
  }
  if (is.null(x) || !is.atomic(x) || length(dim(x)) > 1) {
    stop("Only atomic vectors of length 1 or data frames with 1 row can be unboxed.")
  }
  if (identical(length(x), 1L)) {
    return(as.scalar(x))
  } else {
    stop("Tried to unbox a vector of length ", length(x))
  }
}
