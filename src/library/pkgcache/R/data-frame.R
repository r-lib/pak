find_in_data_frame <- function(df, ..., .list = NULL) {
  cols <- drop_nulls(c(list(...), .list))
  idx <- seq_len(nrow(df))
  for (i in seq_along(cols)) {
    if (length(idx) == 0) break
    n <- names(cols)[i]
    idx <- idx[df[[n]][idx] %in% cols[[i]]]
  }

  idx
}

append_to_data_frame <- function(df, ..., .list = NULL) {
  cols <- c(list(...), .list)
  assert_that(all_named(cols))

  if (length(new <- setdiff(names(cols), names(df)))) {
    na_col <- rep(NA_character_, nrow(df))
    df[new] <- replicate(length(new), na_col, simplify = FALSE)
  }

  cols <- ifelse(names(df) %in% names(cols), cols[names(df)], NA_integer_)
  res <- rbind(df, cols, stringsAsFactors = FALSE)
  names(cols) <- names(df)
  res <- rbind(df, as.data.frame(as.list(cols), stringsAsFactors = FALSE))
  names(res) <- names(df)
  res
}

delete_from_data_frame <- function(df, ..., .list = NULL) {
  idx <- find_in_data_frame(df, ..., .list = .list)
  if (length(idx)) df[-idx, ] else df
}
