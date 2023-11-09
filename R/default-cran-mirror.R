
default_cran_mirror <- function() {
  mirror <- getOption("repos")["CRAN"]
  if (is.null(mirror) || is.na(mirror) || mirror == "@CRAN@") {
    c(CRAN = "https://cran.rstudio.com")
  } else {
    c(CRAN = unname(mirror))
  }
}
