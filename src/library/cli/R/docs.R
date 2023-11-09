
#' Frequently Asked Questions
#'
#' @name faq
#' @includeRmd man/chunks/FAQ.Rmd
NULL


docs_progress_c_api <- function() {
  if (file.exists("inst/include/cli/progress.h")) {
    lines <- readLines("inst/include/cli/progress.h")
  } else {
    lines <- readLines("../inst/include/cli/progress.h")
  }

  ## Remove non-matching lines, but leave an empty line between blocks
  ptn <- "^//'[ ]?"
  mtch <- grepl(ptn, lines)
  lines[!mtch] <- ""
  prev <- c("", lines[-length(lines)])
  lines <- lines[mtch | prev != lines]

  ## Remove doc pattern
  lines <- sub(ptn, "", lines)

  tmp <- tempfile(fileext = ".Rmd")
  cat(lines, sep = "\n", file = tmp)
  tmp
}

#' @title The cli progress C API
#' @name progress-c
#' @section The cli progress C API:
#'
#' ```{r include = FALSE, cache = FALSE, child = cli:::docs_progress_c_api()}
#' ```
NULL

#' @title cli environment variables and options
#' @name cli-config
#'
#' @section User facing configuration:
#'
#' ```{r include = FALSE, child = "vignettes/cli-config-user.Rmd"}
#' ```
#'
#' @section Internal configuration:
#'
#' ```{r include = FALSE, child = "vignettes/cli-config-internal.Rmd"}
#' ```
NULL
