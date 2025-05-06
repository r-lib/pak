unescape_unicode <- function(x) {
  #single string only
  stopifnot(is.character(x) && length(x) == 1)

  #find matches
  m <- gregexpr("(\\\\)+u[0-9a-z]{4}", x, ignore.case = TRUE)

  if (m[[1]][1] > -1) {
    #parse matches
    p <- vapply(
      regmatches(x, m)[[1]],
      function(txt) {
        gsub("\\", "\\\\", parse(text = paste0('"', txt, '"'))[[1]], fixed = TRUE, useBytes = TRUE)
      },
      character(1),
      USE.NAMES = FALSE
    )

    #substitute parsed into original
    regmatches(x, m) <- list(p)
  }

  x
}
