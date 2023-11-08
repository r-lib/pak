idesc_to_latex <- function(self, private) {
  cols <- field_order(names(private$data))
  col_str <- unlist(lapply(
    cols,
    FUN = function(col) {
      toLatex(private$data[[col]])
    }))

  structure(
    c(
      "\\begin{description}",
      "  \\raggedright{}",
      paste0("  ", col_str, collapse = "\n"),
      "\\end{description}"
    ),
    class = "Latex"
  )
}


#' @importFrom utils toLatex
NULL

#' @export
toLatex.character <- function(object, ...) {
  object <- gsub("'([^ ']*)'", "`\\1'", object, useBytes = TRUE)
  object <- gsub("\"([^\"]*)\"", "``\\1''", object, useBytes = TRUE)
  object <- gsub("\\", "\\textbackslash ", object, fixed = TRUE,
               useBytes = TRUE)
  object <- gsub("([{}$#_^%])", "\\\\\\1", object, useBytes = TRUE)
  object
}

#' @export
toLatex.DescriptionField <- function(object, ...) {
  paste0("\\item[", object$key, "] ", toLatex(object$value))
}

#' @export
toLatex.DescriptionCollate <- function(object, ...) {
  invisible(NULL)
}

#' @export
toLatex.DescriptionURLList <- function(object, ...) {
  paste0("\\item[", object$key, "] ", format_url(parse_url_list(object$value)))
}

#' @export
toLatex.DescriptionURL <- function(object, ...) {
  paste0("\\item[", object$key, "] ", format_url(object$value))
}


#' @export
toLatex.DescriptionAuthorsAtR <- function(object, ...) {
  xx <- parse_authors_at_r(object$value)
  c(
    "\\item[Authors@R] ~\\\\",
    "  \\begin{description}",
    paste0("    ", vapply(xx, toLatex, character(1L)), collapse = "\n"),
    "  \\end{description}"
  )
}

#' @export
toLatex.person <- function(object, ...) {
  paste0(
    "\\item",
    format(object, include = c("role")),
    " ",
    format(object, include = c("given", "family")),
    " ",
    if (!is.null(object$email))
      paste0("<\\href{mailto:", object$email, "}{", object$email, "}>"),
    format(object, include = c("comment"))
  )
}

format_url <- function(object) {
  paste0("\\url{", object, "}", collapse = ", ")
}
