
## TODO: continuation lines

idesc_str <- function(self, private, by_field, normalize = TRUE,
                      mode = c("file", "screen")) {

  stopifnot(is_flag(by_field))
  mode <- match.arg(mode)
  cols <- names(private$data)
  if (normalize) cols <- field_order(cols)
  col_str <- vapply(
    cols, FUN.VALUE = "",
    FUN = function(col) {
      if (normalize) {
        format(private$data[[col]], mode = mode)
      } else {
        paste0(
          private$data[[col]]$key, ": ",
          mark_continuation_lines(private$data[[col]]$value)
        )
      }
    })

  if (by_field) col_str else paste(col_str, collapse = "\n")
}

field_order <- function(fields) {
  first <- c(
    "Type", "Package", "Title", "Version", "Date",
    "Authors@R", "Author", "Maintainer",
    "Description", "License", "URL", "BugReports",
    "Depends", setdiff(dep_types, "Depends"), "VignetteBuilder",
    "RdMacros", "Remotes"
  )

  last <- collate_fields

  c(
    intersect(first, fields),
    sort(setdiff(fields, c(first, last))),
    intersect(last, fields)
  )
}

color_bad <- function(x) {
  if (identical(check_field(x), TRUE)) x$value else cli::col_red(x$value)
}

#' @export
#' @method format DescriptionField

format.DescriptionField <- function(x, ..., width = 75) {
  mark_continuation_lines(paste(
    strwrap(paste0(cli::col_blue(x$key), ": ", color_bad(x)), exdent = 4, width = width),
    collapse = "\n"
  ))
}

#' @export
#' @method format DescriptionDependencyList

format.DescriptionDependencyList <- function(x, ...) {
  paste0(
    cli::col_blue(x$key), ":\n",
    paste0(
      "    ",
      sort(str_trim(strsplit(color_bad(x), ",", fixed = TRUE)[[1]])),
      collapse = ",\n"
    )
  )
}

#' @export
#' @method format DescriptionPackageList

format.DescriptionPackageList <- format.DescriptionDependencyList

#' @export
#' @method format DescriptionRemotes

format.DescriptionRemotes <- format.DescriptionDependencyList

#' @export
#' @method format DescriptionCollate

format.DescriptionCollate <- function(x, ...) {
  paste0(
    cli::col_blue(x$key), ":",
    deparse_collate(parse_collate(color_bad(x)))
  )
}

#' @export
#' @method format DescriptionAuthorsAtR

format.DescriptionAuthorsAtR <- function(x, mode = c("file", "screen"),
                                         ...) {
  xx <- parse_authors_at_r(x$value)

  if (mode == "screen") {
    good <- check_field(x)
    xxx <- if (good) xx else cli::col_red(xx)
    paste0(
      cli::col_blue(x$key), " (parsed):\n",
      paste0("    * ", format(xxx), collapse = "\n")
    )

  } else {
    paste0(
      x$key, ":",
      sub("\n$", "", deparse_authors_at_r(xx))
    )
  }
}


idesc_print <- function(self, private) {
  cat(
    idesc_str(self, private, by_field = FALSE, mode = "screen"),
    sep = "",
    "\n"
  )
  invisible(self)
}


idesc_normalize <- function(self, private) {
  self$reformat_fields()
  self$reorder_fields()
  invisible(self)
}

idesc_reformat_fields <- function(self, private) {
  old <- options(cli.num_colors = 1, crayon.enabled = FALSE)
  on.exit(options(old), add = TRUE)
  norm_fields <- idesc_str(self, private, by_field = TRUE)
  for (f in names(norm_fields)) {
    private$data[[f]]$value <-
      sub(paste0(f, ":[ ]?"), "", norm_fields[[f]])
  }
  invisible(self)
}

idesc_reorder_fields <- function(self, private) {
  private$data <- private$data[field_order(names(private$data))]
  invisible(self)
}
