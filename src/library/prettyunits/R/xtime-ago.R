
#' Human readable format of the time interval since a time point
#'
#' It calls \code{\link{vague_dt}} to do the actual formatting.
#'
#' @param date Date(s), \code{as.POSIXct} will be called on them.
#' @param format Format, currently available formats are:
#'   \sQuote{default}, \sQuote{short}, \sQuote{terse}. See examples below.
#' @return Character vector of the formatted time intervals.
#'
#' @export
#' @examples
#' now <- Sys.time()
#'
#' time_ago(now)
#' time_ago(now - as.difftime(30, units = "secs"))
#' time_ago(now - as.difftime(14, units = "mins"))
#' time_ago(now - as.difftime(5, units = "hours"))
#' time_ago(now - as.difftime(25, units = "hours"))
#' time_ago(now - as.difftime(5, units = "days"))
#' time_ago(now - as.difftime(30, units = "days"))
#' time_ago(now - as.difftime(365, units = "days"))
#' time_ago(now - as.difftime(365 * 10, units = "days"))
#'
#' ## Short format
#' time_ago(format = "short", now)
#' time_ago(format = "short", now - as.difftime(30, units = "secs"))
#' time_ago(format = "short", now - as.difftime(14, units = "mins"))
#' time_ago(format = "short", now - as.difftime(5, units = "hours"))
#' time_ago(format = "short", now - as.difftime(25, units = "hours"))
#' time_ago(format = "short", now - as.difftime(5, units = "days"))
#' time_ago(format = "short", now - as.difftime(30, units = "days"))
#' time_ago(format = "short", now - as.difftime(365, units = "days"))
#' time_ago(format = "short", now - as.difftime(365 * 10, units = "days"))
#'
#' ## Even shorter, terse format, (almost always) exactly 3 characters wide
#' time_ago(format = "terse", now)
#' time_ago(format = "terse", now - as.difftime(30, units = "secs"))
#' time_ago(format = "terse", now - as.difftime(14, units = "mins"))
#' time_ago(format = "terse", now - as.difftime(5, units = "hours"))
#' time_ago(format = "terse", now - as.difftime(25, units = "hours"))
#' time_ago(format = "terse", now - as.difftime(5, units = "days"))
#' time_ago(format = "terse", now - as.difftime(30, units = "days"))
#' time_ago(format = "terse", now - as.difftime(365, units = "days"))
#' time_ago(format = "terse", now - as.difftime(365 * 10, units = "days"))

time_ago <- format_time_ago$time_ago

#' Human readable format of a time interval
#'
#' @param dt A \code{difftime} object, the time interval(s).
#' @param format Format, currently available formats are:
#'   \sQuote{default}, \sQuote{short}, \sQuote{terse}. See examples below.
#' @return Character vector of the formatted time intervals.
#'
#' @export
#' @examples
#' vague_dt(as.difftime(30, units = "secs"))
#' vague_dt(as.difftime(14, units = "mins"))
#' vague_dt(as.difftime(5, units = "hours"))
#' vague_dt(as.difftime(25, units = "hours"))
#' vague_dt(as.difftime(5, units = "days"))
#' vague_dt(as.difftime(30, units = "days"))
#' vague_dt(as.difftime(365, units = "days"))
#' vague_dt(as.difftime(365 * 10, units = "days"))
#'
#' ## Short format
#' vague_dt(format = "short", as.difftime(30, units = "secs"))
#' vague_dt(format = "short", as.difftime(14, units = "mins"))
#' vague_dt(format = "short", as.difftime(5, units = "hours"))
#' vague_dt(format = "short", as.difftime(25, units = "hours"))
#' vague_dt(format = "short", as.difftime(5, units = "days"))
#' vague_dt(format = "short", as.difftime(30, units = "days"))
#' vague_dt(format = "short", as.difftime(365, units = "days"))
#' vague_dt(format = "short", as.difftime(365 * 10, units = "days"))
#'
#' ## Even shorter, terse format, (almost always) exactly 3 characters wide
#' vague_dt(format = "terse", as.difftime(30, units = "secs"))
#' vague_dt(format = "terse", as.difftime(14, units = "mins"))
#' vague_dt(format = "terse", as.difftime(5, units = "hours"))
#' vague_dt(format = "terse", as.difftime(25, units = "hours"))
#' vague_dt(format = "terse", as.difftime(5, units = "days"))
#' vague_dt(format = "terse", as.difftime(30, units = "days"))
#' vague_dt(format = "terse", as.difftime(365, units = "days"))
#' vague_dt(format = "terse", as.difftime(365 * 10, units = "days"))

vague_dt <- format_time_ago$vague_dt
