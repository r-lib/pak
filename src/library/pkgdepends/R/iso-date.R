milliseconds <- function(x) as.difftime(as.numeric(x) / 1000, units = "secs")
seconds <- function(x) as.difftime(as.numeric(x), units = "secs")
minutes <- function(x) as.difftime(as.numeric(x), units = "mins")
hours <- function(x) as.difftime(as.numeric(x), units = "hours")
days <- function(x) as.difftime(as.numeric(x), units = "days")
weeks <- function(x) as.difftime(as.numeric(x), units = "weeks")
wday <- function(x) as.POSIXlt(x, tz = "UTC")$wday + 1
with_tz <- function(x, tzone = "") as.POSIXct(as.POSIXlt(x, tz = tzone))
ymd <- function(x) as.POSIXct(x, format = "%Y %m %d", tz = "UTC")
yj <- function(x) as.POSIXct(x, format = "%Y %j", tz = "UTC")

parse_iso_8601 <- function(dates, default_tz = "UTC") {
  if (default_tz == "") default_tz <- Sys.timezone()
  dates <- as.character(dates)
  match <- re_match(dates, iso_regex)
  matching <- !is.na(match$.match)
  result <- rep(.POSIXct(NA_real_, tz = ""), length.out = length(dates))
  result[matching] <- parse_iso_parts(match[matching, ], default_tz)
  class(result) <- c("POSIXct", "POSIXt")
  with_tz(result, "UTC")
}

parse_iso_parts <- function(mm, default_tz) {
  num <- nrow(mm)

  ## -----------------------------------------------------------------
  ## Date first

  date <- .POSIXct(rep(NA_real_, num), tz = "")

  ## Years-days
  fyd <- is.na(date) & mm$yearday != ""
  date[fyd] <- yj(paste(mm$year[fyd], mm$yearday[fyd]))

  ## Years-weeks-days
  fywd <- is.na(date) & mm$week != "" & mm$weekday != ""
  date[fywd] <- iso_week(mm$year[fywd], mm$week[fywd], mm$weekday[fywd])

  ## Years-weeks
  fyw <- is.na(date) & mm$week != ""
  date[fyw] <- iso_week(mm$year[fyw], mm$week[fyw], "1")

  ## Years-months-days
  fymd <- is.na(date) & mm$month != "" & mm$day != ""
  date[fymd] <- ymd(paste(mm$year[fymd], mm$month[fymd], mm$day[fymd]))

  ## Years-months
  fym <- is.na(date) & mm$month != ""
  date[fym] <- ymd(paste(mm$year[fym], mm$month[fym], "01"))

  ## Years
  fy <- is.na(date)
  date[fy] <- ymd(paste(mm$year, "01", "01"))

  ## -----------------------------------------------------------------
  ## Now the time

  th <- mm$hour != ""
  date[th] <- date[th] + hours(mm$hour[th])

  tm <- mm$min != ""
  date[tm] <- date[tm] + minutes(mm$min[tm])

  ts <- mm$sec != ""
  date[ts] <- date[ts] + seconds(mm$sec[ts])

  ## -----------------------------------------------------------------
  ## Fractional time

  frac <- as.numeric(sub(",", ".", mm$frac))

  tfs <- !is.na(frac) & mm$sec != ""
  date[tfs] <- date[tfs] + milliseconds(round(frac[tfs] * 1000))

  tfm <- !is.na(frac) & mm$sec == "" & mm$min != ""
  sec <- trunc(frac[tfm] * 60)
  mil <- round((frac[tfm] * 60 - sec) * 1000)
  date[tfm] <- date[tfm] + seconds(sec) + milliseconds(mil)

  tfh <- !is.na(frac) & mm$sec == "" & mm$min == ""
  min <- trunc(frac[tfh] * 60)
  sec <- trunc((frac[tfh] * 60 - min) * 60)
  mil <- round((((frac[tfh] * 60) - min) * 60 - sec) * 1000)
  date[tfh] <- date[tfh] + minutes(min) + seconds(sec) + milliseconds(mil)

  ## -----------------------------------------------------------------
  ## Time zone

  ftzpm <- mm$tzpm != ""
  m <- ifelse(mm$tzpm[ftzpm] == "+", -1, 1)
  ftzpmh <- ftzpm & mm$tzhour != ""
  date[ftzpmh] <- date[ftzpmh] + m * hours(mm$tzhour[ftzpmh])
  ftzpmm <- ftzpm & mm$tzmin != ""
  date[ftzpmm] <- date[ftzpmm] + m * minutes(mm$tzmin[ftzpmm])

  ftzz <- mm$tz == "Z"
  date[ftzz] <- as.POSIXct(date[ftzz], "UTC")

  ftz <- mm$tz != "Z" & mm$tz != ""
  date[ftz] <- as.POSIXct(date[ftz], mm$tz[ftz])

  if (default_tz != "UTC") {
    ftna <- mm$tzpm == "" & mm$tz == ""
    if (any(ftna)) {
      dd <- as.POSIXct(
        format_iso_8601(date[ftna]),
        "%Y-%m-%dT%H:%M:%S+00:00",
        tz = default_tz
      )
      date[ftna] <- dd
    }
  }

  as.POSIXct(date, "UTC")
}

iso_regex <- paste0(
  "^\\s*",
  "(?<year>[\\+-]?\\d{4}(?!\\d{2}\\b))",
  "(?:(?<dash>-?)",
  "(?:(?<month>0[1-9]|1[0-2])",
  "(?:\\g{dash}(?<day>[12]\\d|0[1-9]|3[01]))?",
  "|W(?<week>[0-4]\\d|5[0-3])(?:-?(?<weekday>[1-7]))?",
  "|(?<yearday>00[1-9]|0[1-9]\\d|[12]\\d{2}|3",
  "(?:[0-5]\\d|6[1-6])))",
  "(?<time>[T\\s](?:(?:(?<hour>[01]\\d|2[0-3])",
  "(?:(?<colon>:?)(?<min>[0-5]\\d))?|24\\:?00)",
  "(?<frac>[\\.,]\\d+(?!:))?)?",
  "(?:\\g{colon}(?<sec>[0-5]\\d)(?:[\\.,]\\d+)?)?",
  "(?<tz>[zZ]|(?<tzpm>[\\+-])",
  "(?<tzhour>[01]\\d|2[0-3]):?(?<tzmin>[0-5]\\d)?)?)?)?$"
)

iso_week <- function(year, week, weekday) {
  wdmon <- function(date) {
    (wday(date) + 5L) %% 7L
  }
  thu <- function(date) {
    date - days(wdmon(date) - 3L)
  }

  thu(ymd(paste(year, "01", "04"))) +
    weeks(as.numeric(week) - 1L) +
    days(as.numeric(weekday) - 4L)
}

format_iso_8601 <- function(date) {
  format(as.POSIXlt(date, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
}
