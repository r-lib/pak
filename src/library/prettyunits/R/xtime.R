
#' Pretty formatting of milliseconds
#'
#' @param ms Numeric vector of milliseconds
#' @param compact If true, then only the first non-zero
#'   unit is used. See examples below.
#' @return Character vector of formatted time intervals.
#'
#' @family time
#' @export
#' @examples
#' pretty_ms(c(1337, 13370, 133700, 1337000, 1337000000))
#'
#' pretty_ms(c(1337, 13370, 133700, 1337000, 1337000000),
#'           compact = TRUE)

pretty_ms <- format_time$pretty_ms

#' Pretty formatting of seconds
#'
#' @param sec Numeric vector of seconds.
#' @return Character vector of formatted time intervals.
#'
#' @inheritParams pretty_ms
#' @family time
#' @export
#' @examples
#' pretty_sec(c(1337, 13370, 133700, 1337000, 13370000))
#'
#' pretty_sec(c(1337, 13370, 133700, 1337000, 13370000),
#'            compact = TRUE)

pretty_sec <- format_time$pretty_sec

#' Pretty formatting of time intervals (difftime objects)
#'
#' @param dt A \code{difftime} object, a vector of time
#'   differences.
#' @return Character vector of formatted time intervals.
#'
#' @inheritParams pretty_ms
#' @family time
#' @export
#' @examples
#' pretty_dt(as.difftime(1000, units = "secs"))
#' pretty_dt(as.difftime(0, units = "secs"))

pretty_dt <- format_time$pretty_dt
