progress_chars <- function() {
  if (cli::is_utf8_output()) {
    list(
      build = "\U0001f4e6",
      inst = "\u2705",
      lpar = "\u2e28",
      rpar = "\u2e29",
      fill = "\u2588",
      half = "\u2592"
    )
  } else {
    list(
      build = "[BLD]",
      inst = "[INS]",
      lpar = "(",
      rpar = ")",
      fill = "#",
      half = "-"
    )
  }
}

res__create_progress_bar <- function(self, private) {
  self
  private

  if (!should_show_progress_bar()) return()

  bar <- new.env(parent = emptyenv())
  bar$spinner <- cli::get_spinner()
  bar$spinner_state <- 1L
  bar$chars <- progress_chars()

  bar$status <- cli::cli_status("", .auto_close = FALSE)

  bar$timer <- new_async_timer(
    1 / 10,
    function() res__show_progress_bar(self, private)
  )
  bar$timer$listen_on("error", function(e) {
    stop(e)
  })

  bar
}

res__show_progress_bar <- function(self, private) {
  # Maybe progress is off
  if (is.null(private$bar)) return()

  # This can be called _after_ the resolution is over
  if (is.null(private$bar$status)) return()

  deps <- nrow(private$state)
  direct <- sum(private$state$direct)
  direct_done <- sum(!is.na(private$state$status) & private$state$direct)

  bar <- if (direct >= 5) {
    make_bar(private$bar$chars, direct_done / direct, width = 15)
  } else {
    ""
  }

  state <- make_progress_main(deps, direct_done, direct)
  spinner <- make_progress_spinner(self, private)
  msg <- make_trailing_progress_msg(self, private)

  # TODO: check width
  str <- "{bar} {state} {spinner} {msg}"
  cli::cli_status_update(private$bar$status, str)
}

make_bar <- function(chars, p, width = 15) {
  width <- width - 2L

  w <- if (isTRUE(all.equal(p, 1))) width else trunc(width * p)

  pchars <- rep(chars$fill, w)
  xchars <- rep(" ", max(width - w, 0))
  bar <- paste(
    c(chars$lpar, pchars, xchars, chars$rpar),
    collapse = ""
  )

  if (is_older_rstudio()) bar else cli::col_green(bar)
}

make_progress_main <- function(deps, done, total) {
  if (is_older_rstudio()) {
    bggrey <- fgdark <- function(x) x
  } else {
    bggrey <- cli::make_ansi_style("grey", bg = TRUE)
    fgdark <- cli::col_black
  }
  paste0(
    "Found ",
    bggrey(fgdark(paste0(" ", deps, " "))),
    " deps for ",
    bggrey(fgdark(paste0(" ", done, "/", total, " "))),
    " pkgs"
  )
}

make_progress_spinner <- function(self, private) {
  bar <- private$bar
  spin <- bar$spinner$frames[[bar$spinner_state]]
  bar$spinner_state <-
    bar$spinner_state %% length(bar$spinner$frames) + 1L
  private$bar <- bar
  paste0("[", spin, "]")
}

make_trailing_progress_msg <- function(self, private) {
  ongoing <- private$state[is.na(private$state$status), ]
  if (nrow(ongoing) == 0) return("Done")

  types <- vcapply(ongoing$remote, "[[", "type")
  remote <- if (all(types %in% c("cran", "bioc", "standard"))) {
    ongoing$remote[[order(ongoing$started_at)[1]]]
  } else {
    nonstd <- ongoing[!types %in% c("cran", "bioc", "special"), ]
    nonstd$remote[[order(nonstd$started_at)[1]]]
  }

  if (remote$type %in% c("cran", "bioc", "standard")) {
    "Resolving standard (CRAN/BioC) packages"
  } else if (remote$type %in% "installed") {
    "Checking installed packages"
  } else {
    paste0("Resolving ", remote$ref)
  }
}

res__done_progress_bar <- function(self, private) {
  if (is.null(private$bar)) return()
  cli::cli_status_clear(private$bar$status, result = "clear")
  private$bar <- NULL
}
