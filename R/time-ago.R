
format_time_ago <- local({

  e <- expression

  `%s%` <- function(lhs, rhs) {
    assert_string(lhs)
    do.call(
      sprintf,
      c(list(lhs), as.list(rhs))
    )
  }

  assert_string <- function(x) {
    stopifnot(is.character(x), length(x) == 1L)
  }

  assert_diff_time <- function(x) {
    stopifnot(inherits(x, "difftime"))
  }

  vague_dt_default <- list(
    list(c = e(seconds < 10), s = "moments ago"),
    list(c = e(seconds < 45), s = "less than a minute ago"),
    list(c = e(seconds < 90), s = "about a minute ago"),
    list(c = e(minutes < 45), s = e("%d minutes ago" %s% round(minutes))),
    list(c = e(minutes < 90), s = "about an hour ago"),
    list(c = e(hours < 24),   s = e("%d hours ago" %s% round(hours))),
    list(c = e(hours < 42),   s = "a day ago"),
    list(c = e(days < 30),    s = e("%d days ago" %s% round(days))),
    list(c = e(days < 45),    s = "about a month ago"),
    list(c = e(days < 335),   s = e("%d months ago" %s% round(days / 30))),
    list(c = e(years < 1.5),  s = "about a year ago"),
    list(c = TRUE,            s = e("%d years ago" %s% round(years)))
  )

  vague_dt_short <- list(
    list(c = e(seconds < 50), s = "<1 min"),
    list(c = e(minutes < 50), s = e("%d min" %s% round(minutes))),
    list(c = e(hours < 1.5),  s = "1 hour"),
    list(c = e(hours < 18),   s = e("%d hours" %s% round(hours))),
    list(c = e(hours < 42),   s = "1 day"),
    list(c = e(days < 30),    s = e("%d day" %s% round(days))),
    list(c = e(days < 45),    s = "1 mon"),
    list(c = e(days < 335),   s = e("%d mon" %s% round(days / 30))),
    list(c = e(years < 1.5),  s = "1 year"),
    list(c = TRUE,            s = e("%d years" %s% round(years)))
  )

  vague_dt_terse <- list(
    list(c = e(seconds < 50), s = e("%2ds" %s% round(seconds))),
    list(c = e(minutes < 50), s = e("%2dm" %s% round(minutes))),
    list(c = e(hours < 18),   s = e("%2dh" %s% round(hours))),
    list(c = e(days < 30),    s = e("%2dd" %s% round(days))),
    list(c = e(days < 335),   s = e("%2dM" %s% round(days / 30))),
    list(c = TRUE,            s = e("%2dy" %s% round(years)))
  )

  vague_dt_formats <- list(
    "default" = vague_dt_default,
    "short" = vague_dt_short,
    "terse" = vague_dt_terse
  )

  time_ago <- function(date, format = c("default", "short", "terse")) {

    date <- as.POSIXct(date)

    if (length(date) > 1) return(sapply(date, time_ago, format = format))

    seconds <- difftime(Sys.time(), date, units = "secs")

    vague_dt(seconds, format = format)
  }

  vague_dt <- function(dt, format = c("default", "short", "terse")) {

    assert_diff_time(dt)

    units(dt) <- "secs"
    seconds <- as.vector(dt)

    ## Simplest to quit here for empty input
    if (!length(seconds)) return(character())

    pieces <- list(
      minutes = seconds / 60,
      hours = seconds / 60 / 60,
      days = seconds / 60 / 60 / 24,
      years = seconds / 60 / 60 / 24 / 365.25
    )

    format <- match.arg(format)

    for (p in vague_dt_formats[[format]]) {
      if (eval(p$c, pieces)) return(eval(p$s, pieces))
    }
  }

  structure(
    list(
      .internal = environment(),
      time_ago = time_ago,
      vague_dt = vague_dt
    ),
    class = c("standalone_time_ago", "standalone")
  )
})
