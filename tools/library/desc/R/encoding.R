
#' Check encoding of new or existing fields
#'
#' If `new_fields` is `NULL`, then the existing
#' fields are checked. Otherwise `new_fields` are checked.
#'
#' Warnings are given for non-ascii fields, if the `Encoding`
#' field is not set.
#'
#' @param self Object.
#' @param private Private env.
#' @param new_fields New fields, or `NULL` to check existing fields.
#' @return Object, invisibly.
#'
#' @keywords internal

check_encoding <- function(self, private, new_fields) {
  if (!is.na(self$get('Encoding'))) return(invisible(self))

  fields <- if (is.null(new_fields)) {
    as.list(self$get(self$fields()))
  } else {
    new_fields
  }

  nonascii <- !vapply(fields, is_ascii, TRUE)

  if (any(nonascii)) {
    warning(
      "Consider adding an Encoding field to DESCRIPTION,\n",
      "Non-ASCII character(s) in ",
      paste(names(fields)[nonascii], collapse = ", ")
    )
  }

  invisible(self)
}
