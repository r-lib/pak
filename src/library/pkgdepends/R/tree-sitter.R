ts_languages <- c(r = 0L, markdown = 1L, "markdown-inline" = 2L, yaml = 3L)

s_expr <- function(
  code,
  language = c("r", "markdown", "markdown-inline", "yaml"),
  ranges = NULL
) {
  language <- tolower(language)
  language <- ts_languages[match.arg(language)]
  if (is.character(code)) code <- charToRaw(paste(code, collapse = "\n"))
  call_with_cleanup(c_s_expr, code, language, ranges)
}

code_query <- function(
  code = NULL,
  query,
  file = NULL,
  language = c("r", "markdown", "markdown-inline", "yaml"),
  ranges = NULL
) {
  language <- tolower(language)
  language <- ts_languages[match.arg(language)]
  qlen <- nchar(query, type = "bytes") + 1L # + \n
  qbeg <- c(1L, cumsum(qlen))
  qnms <- names(query) %||% rep(NA_character_, length(query))
  query1 <- paste0(query, "\n", collapse = "")

  if (!is.null(code)) {
    if (is.character(code)) code <- charToRaw(paste(code, collapse = "\n"))
    res <- call_with_cleanup(c_code_query, code, query1, language, ranges)
  } else {
    res <- call_with_cleanup(c_code_query_path, file, query1, language, ranges)
  }

  qorig <- as.integer(cut(res[[1]][[3]], breaks = qbeg, include.lowest = TRUE))

  list(
    patterns = data_frame(
      id = seq_along(res[[1]][[1]]),
      name = qnms[qorig],
      pattern = res[[1]][[1]],
      match_count = res[[1]][[2]]
    ),
    matched_captures = data_frame(
      id = viapply(res[[2]], "[[", 3L),
      pattern = viapply(res[[2]], "[[", 1L),
      match = viapply(res[[2]], "[[", 2L),
      start_byte = viapply(res[[2]], "[[", 6L),
      end_byte = viapply(res[[2]], "[[", 7L),
      start_row = viapply(res[[2]], "[[", 8L),
      start_column = viapply(res[[2]], "[[", 9L),
      end_row = viapply(res[[2]], "[[", 10L),
      end_column = viapply(res[[2]], "[[", 11L),
      name = vcapply(res[[2]], "[[", 4L),
      code = vcapply(res[[2]], "[[", 5L)
    )
  )
}
