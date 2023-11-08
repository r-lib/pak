#' @importFrom cli symbol
#' @importFrom utils head tail
#' @importFrom prettyunits pretty_dt

# This is adapted from https://github.com/r-lib/rcmdcheck/blob/7ee14764c2b17ee2c2f4131a9e19d1b56a66ed0f/R/callback.R
block_callback <- function(quiet) {
  partial_line <- ""

  state <- "OK"
  should_time <- FALSE
  line_started <- Sys.time()
  now <- NULL
  prev_line <- ""

  no <- function(x, what = "") {
    pattern <- paste0(" \\.\\.\\. ", what, "$")
    sub("^\\* ", "", sub(pattern, "", x))
  }

  time_if_long <- function() {
    elapsed <- now - line_started
    if (elapsed > as.difftime(1 / 3, units = "secs")) {
      style(timing = paste0(" (", pretty_dt(elapsed), ")"))
    } else {
      ""
    }
  }

  do_line <- function(x) {
    should_time <<- FALSE
    now <<- Sys.time()

    ## Test mode is special. It will change the 'state' back to 'OK',
    ## once it is done.
    xx <- if (is_new_check(x)) {
      do_new_check(x)
    } else if (grepl("^Status: ", x)) {
      ## We just skip the status, it is printed out anyway, as the return
      ## value
      NA_character_
    } else {
      do_continuation(x)
    }

    prev_line <<- x

    ## NA_character_ can omit output
    if (is.na(xx)) {
      return()
    }

    if (should_time) xx <- style(xx, timing = time_if_long())

    line_started <<- now

    cat(xx, "\n", sep = "")
    flush(stdout())
  }

  do_new_check <- function(x) {
    should_time <<- TRUE
    if (grepl(" \\.\\.\\. OK\\s*$", x)) {
      state <<- "OK"
      style(ok = symbol$tick, "  ", pale = no(x, "OK"))
    } else if (grepl(" \\.\\.\\. NOTE\\s*$", x)) {
      state <<- "NOTE"
      style(note = c("N  ", no(x, "NOTE")))
    } else if (grepl(" \\.\\.\\. WARNING\\s*$", x)) {
      state <<- "WARNING"
      style(warn = c("W  ", no(x, "WARNING")))
    } else if (grepl(" \\.\\.\\. ERROR\\s*$", x)) {
      state <<- "ERROR"
      style(err = c("E  ", no(x, "ERROR")))
    } else if (grepl("^\\* checking tests \\.\\.\\.[ ]?$", x)) {
      state <<- "tests"
      style(pale = c(symbol$line, "  ", no(x)))
    } else if (grepl("^\\* DONE\\s*$", x)) {
      state <<- "OK"
      NA_character_
    } else {
      style(pale = c(symbol$line, "  ", no(x)))
    }
  }

  do_continuation <- function(x) {
    paste0("   ", x)
  }

  function(x) {
    if (quiet) {
      return()
    }

    x <- paste0(partial_line, x)
    partial_line <<- ""
    lines <- strsplit(x, "\r?\n")[[1]]
    if (last_char(x) != "\n") {
      partial_line <<- tail(lines, 1)
      lines <- head(lines, -1)
    }
    cat("  \r")
    lapply(lines, do_line)
    cat0(sub("^[\\* ]", "  ", partial_line), "\r")
  }
}

is_new_check <- function(x) {
  grepl("^\\* ", x)
}

simple_callback <- function(quiet) {
  function(x) {
    if (quiet) {
      return()
    }
    cat(x)
  }
}
