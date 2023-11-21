format_time <- local({
  assert_diff_time <- function(x) {
    stopifnot(inherits(x, "difftime"))
  }

  parse_ms <- function(ms) {
    stopifnot(is.numeric(ms))

    data.frame(
      days = floor(ms / 86400000),
      hours = floor((ms / 3600000) %% 24),
      minutes = floor((ms / 60000) %% 60),
      seconds = round((ms / 1000) %% 60, 1)
    )
  }

  first_positive <- function(x) which(x > 0)[1]

  trim <- function(x) gsub("^\\s+|\\s+$", "", x)

  pretty_ms <- function(ms, compact = FALSE) {
    stopifnot(is.numeric(ms))

    parsed <- t(parse_ms(ms))

    if (compact) {
      units <- c("d", "h", "m", "s")
      parsed2 <- parsed
      parsed2[] <- paste0(parsed, units)
      idx <- cbind(
        apply(parsed, 2, first_positive),
        seq_len(length(ms))
      )
      tmp <- paste0("~", parsed2[idx])

      # handle NAs
      tmp[is.na(parsed2[idx])] <- NA_character_
      tmp
    } else {
      ## Exact for small ones
      exact <- paste0(ceiling(ms), "ms")
      exact[is.na(ms)] <- NA_character_

      ## Approximate for others, in seconds
      merge_pieces <- function(pieces) {
        ## handle NAs
        if (all(is.na(pieces))) {
          return(NA_character_)
        }

        ## handle non-NAs
        paste0(
          if (pieces[1]) paste0(pieces[1], "d "),
          if (pieces[2]) paste0(pieces[2], "h "),
          if (pieces[3]) paste0(pieces[3], "m "),
          if (pieces[4]) paste0(pieces[4], "s ")
        )
      }
      approx <- trim(apply(parsed, 2, merge_pieces))

      ifelse(ms < 1000, exact, approx)
    }
  }

  pretty_sec <- function(sec, compact = FALSE) {
    pretty_ms(sec * 1000, compact = compact)
  }

  pretty_dt <- function(dt, compact = FALSE) {
    assert_diff_time(dt)

    units(dt) <- "secs"

    pretty_sec(as.vector(dt), compact = compact)
  }

  structure(
    list(
      .internal = environment(),
      pretty_ms = pretty_ms,
      pretty_sec = pretty_sec,
      pretty_dt = pretty_dt
    ),
    class = c("standalone_time", "standalone")
  )
})
