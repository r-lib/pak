list_to_vec <- function(x) {
  isdates <- is_datelist(x)
  out <- unlist(null_to_na(x), recursive = FALSE, use.names = FALSE)
  if (isdates && is.numeric(out)) {
    structure(out, class = c("POSIXct", "POSIXt"))
  } else {
    out
  }
}
