alert <- function(type, msg, ..., .envir = parent.frame()) {
  switch(
    type,
    success = cli::cli_alert_success(msg, ..., .envir = .envir),
    info = cli::cli_alert_info(msg, ..., .envir = .envir),
    warning = cli::cli_alert_warning(msg, ..., .envir = .envir),
    danger = cli::cli_alert_danger(msg, ..., .envir = .envir)
  )
}

create_progress_bar <- function(state) {
  bar <- new.env(parent = emptyenv())
  bar$spinner <- cli::get_spinner()
  bar$spinner_state <- 1L
  if (should_show_progress_bar()) {
    bar$status <- cli::cli_status("Installing...", .auto_close = FALSE)
  }

  bar$simple <- is_older_rstudio()

  bar
}

update_progress_bar <- function(state, tick = 0) {
  if (is.null(state$progress$status)) return()

  plan <- state$plan
  total <- nrow(plan)
  installed <- sum(plan$install_done)
  built <- sum(plan$build_done)

  building <- sum(buildingl <- !plan$build_done & !is.na(plan$worker_id))
  installing <- sum(!buildingl & !is.na(plan$worker_id))

  chars <- progress_chars()

  xbar <- make_install_bar(installed / total, built / total, width = 15)
  xbuilt <- make_progress_block(state, chars$build, built, total, building)
  xinst <- make_progress_block(state, chars$inst, installed, total, installing)
  xmsg <- make_install_trailing_progress_msg(state)

  w <- cli::console_width()
  v1 <- paste0(xbuilt, " | ", xinst, " | ")
  v2 <- paste0(xbar, " | ", v1)
  v3 <- paste0(v2, xmsg)
  if (cli::ansi_nchar(v3, type = "width") <= w) {
    st <- v3
  } else if (cli::ansi_nchar(v2, type = "width") <= w) {
    st <- v2
  } else {
    st <- v1
  }

  if (state$progress$simple) st <- cli::ansi_strip(st)
  cli::cli_status_update(state$progress$status, st)
}

## p1 <= p2 must hold

make_install_bar <- function(p1, p2, width) {
  width <- width - 2L

  w1 <- if (isTRUE(all.equal(p1, 1))) width else trunc(width * p1)
  w2 <- if (isTRUE(all.equal(p2, 1))) width - w1 else trunc(width * (p2 - p1))

  chars <- progress_chars()
  p1chars <- rep(chars$fill, w1)
  p2chars <- rep(chars$half, w2)
  xchars <- rep(" ", max(width - w1 - w2, 0))
  bar <- paste(
    c(chars$lpar, p1chars, p2chars, xchars, chars$rpar),
    collapse = ""
  )

  if (is_older_rstudio()) bar else cli::col_green(bar)
}

make_progress_block <- function(state, sym, done, total, prog) {
  prgs <- state$progress
  spin <- prgs$spinner$frames[[prgs$spinner_state]]
  prgs$spinner_state <- prgs$spinner_state %% length(prgs$spinner$frames) + 1L
  paste0(
    sym,
    "  ",
    done,
    "/",
    total,
    if (prog) paste0(" ", spin, " ", prog) else "    "
  )
}

done_progress_bar <- function(state) {
  if (!is.null(state$progress$status)) {
    cli::cli_status_clear(state$progress$status)
  }
}

make_install_trailing_progress_msg <- function(state) {
  working <- !is.na(state$plan$worker_id)
  installing <- state$plan$build_done & working
  building <- !state$plan$build_done & working

  building_pkgs <- paste(state$plan$package[building], collapse = ", ")
  installing_pkgs <- paste(state$plan$package[installing], collapse = ", ")

  paste0(
    if (any(building)) paste0("building ", building_pkgs),
    if (any(building) && any(installing)) ", ",
    if (any(installing)) paste0("installing ", installing_pkgs)
  )
}
