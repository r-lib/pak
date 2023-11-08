# nocov start
#' Determines the path of the currently running script
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' \R does not store nor export the path of the currently running
#'   script.  This is an attempt to circumvent this limitation by applying
#'   heuristics (such as call stack and argument inspection) that work in many
#'   cases.
#' **CAVEAT**: Use this function only if your workflow does not permit other
#' solution: if a script needs to know its location, it should be set outside
#' the context of the script if possible.
#'
#' @details This functions currently work only if the script was `source`d,
#'   processed with `knitr`,
#'   or run with `Rscript` or using the `--file` parameter to the
#'   `R` executable.  For code run with `Rscript`, the exact value
#'   of the parameter passed to `Rscript` is returned.
#'
#' @section Life cycle:
#'
#' These functions are now available in the \pkg{whereami} package.
#'
#' @return The path of the currently running script, NULL if it cannot be
#'   determined.
#' @seealso [base::source()], [utils::Rscript()], [base::getwd()]
#' @references [https://stackoverflow.com/q/1815606/946850]()
#' @author Kirill Müller, Hadley Wickham, Michael R. Head
#' @keywords internal
#' @examples
#' \dontrun{
#' thisfile()
#' }
#' @export
thisfile <- function() {
  lifecycle::deprecate_soft(
    "2.0.0",
    "rprojroot::thisfile()",
    "whereami::thisfile()"
  )

  if (!is.null(res <- thisfile_source())) {
    res
  } else if (!is.null(res <- thisfile_r())) {
    res
  } else if (!is.null(res <- thisfile_rscript())) {
    res
  } else if (!is.null(res <- thisfile_knit())) {
    res
  } else {
    NULL
  }
}

#' @rdname thisfile
#' @export
thisfile_source <- function() {
  lifecycle::deprecate_soft(
    "2.0.0",
    "rprojroot::thisfile_source()",
    "whereami::thisfile_source()"
  )

  for (i in -(1:sys.nframe())) {
    if (identical(args(sys.function(i)), args(base::source))) {
      return(normalizePath(sys.frame(i)$ofile))
    }
  }

  NULL
}

#' @rdname thisfile
#' @importFrom utils tail
#' @export
thisfile_r <- function() {
  lifecycle::deprecate_soft(
    "2.0.0",
    "rprojroot::thisfile_r()",
    "whereami::thisfile_r()"
  )

  cmd_args <- commandArgs(trailingOnly = FALSE)
  if (!grepl("^R(?:|term)(?:|[.]exe)$", basename(cmd_args[[1L]]), ignore.case = TRUE)) {
    return(NULL)
  }

  cmd_args_trailing <- commandArgs(trailingOnly = TRUE)
  leading_idx <-
    seq.int(from = 1, length.out = length(cmd_args) - length(cmd_args_trailing))
  cmd_args <- cmd_args[leading_idx]
  file_idx <- c(which(cmd_args == "-f") + 1, which(grepl("^--file=", cmd_args)))
  res <- gsub("^(?:|--file=)(.*)$", "\\1", cmd_args[file_idx])

  # If multiple --file arguments are given, R uses the last one
  res <- tail(res[res != ""], 1)
  if (length(res) > 0) {
    return(res)
  }

  NULL
}

#' @rdname thisfile
#' @importFrom utils tail
#' @export
thisfile_rscript <- function() {
  lifecycle::deprecate_soft(
    "2.0.0",
    "rprojroot::thisfile_rscript()",
    "whereami::thisfile_rscript()"
  )

  cmd_args <- commandArgs(trailingOnly = FALSE)
  if (!grepl("^R(?:term|script)(?:|[.]exe)$", basename(cmd_args[[1L]]), ignore.case = TRUE)) {
    return(NULL)
  }

  cmd_args_trailing <- commandArgs(trailingOnly = TRUE)
  leading_idx <-
    seq.int(from = 1, length.out = length(cmd_args) - length(cmd_args_trailing))
  cmd_args <- cmd_args[leading_idx]
  res <- gsub("^(?:--file=(.*)|.*)$", "\\1", cmd_args)

  # If multiple --file arguments are given, R uses the last one
  res <- tail(res[res != ""], 1)
  if (length(res) > 0) {
    return(res)
  }

  NULL
}

#' @rdname thisfile
#' @export
thisfile_knit <- function() {
  lifecycle::deprecate_soft(
    "2.0.0",
    "rprojroot::thisfile_knit()",
    "whereami::thisfile_knit()"
  )

  if (requireNamespace("knitr")) {
    return(knitr::current_input(dir = TRUE))
  }

  NULL
}
# nocov end
