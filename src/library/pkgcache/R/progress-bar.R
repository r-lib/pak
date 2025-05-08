create_progress_bar <- function(data) {
  bar <- new.env(parent = emptyenv())

  if (isTRUE(getOption("pkg.show_progress", FALSE))) {
    bar$status <- cli::cli_status(
      "Checking for {nrow(data)} new metadata file{?s}",
      .auto_close = FALSE
    )
  } else {
    bar$status <- cli::cli_status(character(), .auto_close = FALSE)
  }

  bar$spinner <- cli::get_spinner()
  bar$spinner_state <- 1L

  bar$data <- data
  bar$data$uptodate <- NA
  bar$data$size <- NA_integer_
  bar$data$current <- NA_integer_

  bar$timer <- async_timer$new(1 / 10, function() show_progress_bar(bar))
  bar$timer$listen_on("error", function(...) {
  })

  bar
}

update_progress_bar_progress <- function(bar, data) {
  wh <- match(data$url, bar$data$url)
  ## If TRUE, then it stays TRUE, status 304 might report progress, we
  ## want to ignore that
  if (!isTRUE(bar$data$uptodate[[wh]])) {
    bar$data$uptodate[[wh]] <- FALSE
    bar$data$current[[wh]] <- data[["current"]]
    bar$data$size[[wh]] <- data[["total"]]
  }
}

update_progress_bar_uptodate <- function(bar, url) {
  wh <- match(url, bar$data$url)
  bar$data$uptodate[[wh]] <- TRUE
  bar$data$current[[wh]] <- NA_integer_
  bar$data$size[[wh]] <- NA_integer_
}

update_progress_bar_done <- function(bar, url) {
  wh <- match(url, bar$data$url)
  bar$data$uptodate[[wh]] <- FALSE
  bar$data$current[[wh]] <- bar$data$size[[wh]] <-
    file.size(bar$data$path[[wh]])
}

show_progress_bar <- function(bar) {
  if (
    is.null(bar$status) ||
      !isTRUE(getOption("pkg.show_progress", FALSE))
  ) {
    return()
  }

  data <- bar$data
  uptodate <- sum(data$uptodate, na.rm = TRUE)
  numfiles <- nrow(data)
  current <- sum(data$current, na.rm = TRUE)
  total <- sum(data$size, na.rm = TRUE)
  downloads <- paste0(
    "[",
    format_bytes$pretty_bytes(current),
    " / ",
    format_bytes$pretty_bytes(total),
    "]"
  )

  spinner <- bar$spinner$frames[bar$spinner_state]
  bar$spinner_state <- bar$spinner_state + 1L
  if (bar$spinner_state > length(bar$spinner$frames)) {
    bar$spinner_state <- 1L
  }

  cli::cli_status_update(
    bar$status,
    c(
      "{spinner} Updating metadata database [{uptodate}/{numfiles}] | ",
      "Downloading {downloads}"
    )
  )
}

finish_progress_bar <- function(ok, bar) {
  if (!ok) {
    cli::cli_status_clear(
      bar$status,
      result = "failed",
      msg_failed = "{.alert-danger Metadata update failed}"
    )
  } else if (FALSE %in% bar$data$uptodate) {
    dl <- vlapply(bar$data$uptodate, identical, FALSE)
    files <- sum(dl)
    bytes <- format_bytes$pretty_bytes(sum(bar$data$size[dl], na.rm = TRUE))
    cli::cli_status_clear(
      bar$status,
      result = "done",
      msg_done = "{.alert-success Updated metadata database: {bytes} in {files} file{?s}.}"
    )
  } else {
    cli::cli_status_clear(bar$status)
  }

  bar$status <- NULL
}
