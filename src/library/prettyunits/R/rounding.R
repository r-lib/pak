#' Round a value to a defined number of digits printing out trailing zeros, if
#' applicable
#'
#' @details Values that are not standard numbers like `Inf`, `NA`, and
#'   `NaN` are returned as `"Inf"`, `"NA"`, and `"NaN"`.
#'
#' @param x The number to round.
#' @param digits integer indicating the number of decimal places.
#' @param sci_range See help for [pretty_signif()] (and you likely want
#'   to round with [pretty_signif()] if you want to use this argument).
#' @param sci_sep The separator to use for scientific notation strings
#'   (typically this will be either "e" or "x10^" for computer- or
#'   human-readable output).
#' @return A string with the value.
#' @seealso [round()], [pretty_signif()].
#' @export

pretty_round <- function(x, digits = 0, sci_range = Inf, sci_sep = "e") {
  if (length(digits) == 1) {
    mask_na <- is.na(x)
    mask_aschar <- is.nan(x) | is.infinite(x)
    mask_manip <- !(mask_na | mask_aschar)
    ret <- rep(NA, length(x))
    ## Put in the special values
    if (any(mask_na)) {
      ret[mask_na] <- "NA"
    }
    if (any(mask_aschar)) {
      ret[mask_aschar] <- as.character(x[mask_aschar])
    }
    if (any(mask_manip)) {
      xtmp <- round(x[mask_manip], digits)
      mask_sci <-
        xtmp != 0 &
          abs(log10(abs(xtmp))) >= sci_range
      mask_no_sci <- !mask_sci
      if (any(mask_sci)) {
        logval <- floor(log10(abs(xtmp[mask_sci])))
        ret[mask_manip][mask_sci] <-
          paste0(
            formatC(xtmp[mask_sci] / 10^logval, format = "f", digits = digits + logval),
            sci_sep,
            formatC(logval, format = "d")
          )
      }
      if (any(mask_no_sci)) {
        if (digits < 0) {
          ret[mask_manip][mask_no_sci] <-
            formatC(xtmp[mask_no_sci], format = "f", digits = 0)
        } else {
          ret[mask_manip][mask_no_sci] <-
            formatC(xtmp[mask_no_sci], format = "f", digits = digits)
        }
      }
    }
    ret
  } else if (length(x) == length(digits)) {
    mapply(pretty_round, x, digits = digits, sci_range = sci_range, sci_sep = sci_sep)
  } else {
    stop("digits must either be a scalar or the same length as x")
  }
}

#' Round a value to a defined number of significant digits printing out trailing
#' zeros, if applicable
#'
#' @details Values that are not standard numbers like `Inf`, `NA`, and
#'   `NaN` are returned as `"Inf"`, `"NA"`, and `NaN`.
#'
#' @param x The number to round.
#' @param digits integer indicating the number of significant digits.
#' @param sci_range integer (or `Inf`) indicating when to switch to
#'   scientific notation instead of floating point. Zero indicates always use
#'   scientific; `Inf` indicates to never use scientific notation;
#'   otherwise, scientific notation is used when `abs(log10(x)) > sci_range`.
#' @param sci_sep The separator to use for scientific notation strings
#'   (typically this will be either "e" or "x10^" for computer- or
#'   human-readable output).
#' @return A string with the value.
#' @seealso [signif()], [pretty_round()].
#' @export

pretty_signif <- function(x, digits = 6, sci_range = 6, sci_sep = "e") {
  mask_na <- is.na(x)
  mask_aschar <- is.nan(x) | is.infinite(x)
  mask_manip <- !(mask_na | mask_aschar)
  ret <- rep(NA, length(x))
  ## Put in the special values
  if (any(mask_na)) {
    ret[mask_na] <- "NA"
  }
  if (any(mask_aschar)) {
    ret[mask_aschar] <- as.character(x[mask_aschar])
  }
  if (any(mask_manip)) {
    xtmp <- x[mask_manip]
    toplog <- bottomlog <- rep(NA, length(xtmp))
    ## When 0 give the digits as the output
    bottomlog[xtmp %in% 0] <- digits
    ## Otherwise set it to digits orders of magnitude lower than the
    ## current value
    toplog <- log10(abs(xtmp))
    ## When the order of magnitude is an exact log 10, move up one so
    ## that the math works for determing the lower log.
    mask.exact.log <- (toplog %% 1) %in% 0
    toplog[mask.exact.log] <- toplog[mask.exact.log] + 1
    toplog <- ceiling(toplog)
    bottomlog[is.na(bottomlog)] <- digits - toplog[is.na(bottomlog)]
    ## Find times when rounding increases the toplog and shift up the
    ## bottomlog to a corresponding degree. e.g. x=0.9999 and digits=2
    ## should be 1.0 not 1.00.
    newtoplog <- log10(abs(round(xtmp, digits = bottomlog)))
    mask.exact.log <- (newtoplog %% 1) %in% 0
    newtoplog[mask.exact.log] <- newtoplog[mask.exact.log] + 1
    newtoplog <- ceiling(newtoplog)
    mask.move.up <- toplog < newtoplog
    bottomlog[mask.move.up] <- bottomlog[mask.move.up] - 1
    ## Do the rounding
    ret[mask_manip] <- pretty_round(xtmp,
      digits = bottomlog,
      sci_range = sci_range, sci_sep = sci_sep
    )
  }
  ret
}
