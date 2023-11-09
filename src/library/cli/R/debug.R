
#' Debug cli internals
#'
#' Return the current state of a cli app. It includes the currently
#' open tags, their ids, classes and their computed styles.
#'
#' The returned data frame has a print method, and if you want to create
#' a plain data frame from it, index it with an empty bracket:
#' `cli_debug_doc()[]`.
#'
#' To see all currently active themes, use `app$themes`, e.g. for the
#' default app: `default_app()$themes`.
#'
#' @param app The cli app to debug. Defaults to the current app.
#'   if there is no app, then it creates one by calling [start_app()].
#' @return Data frame with columns: `tag`, `id`, `class` (space separated),
#'   theme (id of the theme the element added), `styles` (computed styles
#'   for the element).
#'
#' @seealso [cli_sitrep()]. To debug containers, you can set the
#' `CLI-DEBUG_BAD_END` environment variable to `true`, and then cli will
#' warn when it cannot find the specified container to close (or any
#' contained at all).
#'
#' @examples
#' \dontrun{
#' cli_debug_doc()
#'
#' olid <- cli_ol()
#' cli_li()
#' cli_debug_doc()
#' cli_debug_doc()[]
#'
#' cli_end(olid)
#' cli_debug_doc()
#' }

cli_debug_doc <- function(app = default_app() %||% start_app()) {
  tgs <- vcapply(app$doc, "[[", "tag")
  ids <- vcapply(app$doc, "[[", "id")
  cls <- vcapply(app$doc, function(x) paste(x$class, collapse = " "))
  thm <- lapply(app$doc, function(x) x$theme)

  df <- data.frame(
    stringsAsFactors = FALSE,
    tag    = tgs,
    id     = ids,
    class  = cls,
    theme  = I(as.list(thm)),
    styles = I(as.list(unname(app$styles)))
  )

  class(df) <- c("cli_doc", class(df))
  df
}

#' @export

format.cli_doc <- function(x, ...) {
  nz <- nrow(x) > 0
  c("<cli document>",
    paste0(
      if (nz) "<",
      x$tag,
      if (nz) " id=\"", x$id, if (nz) "\"",
      ifelse (x$class == "", "", paste0(" class=\"", x$class, "\"")),
      if (nz) ">",
      ifelse (vlapply(x$theme, is.null), "", " +theme")
    )
  )
}

#' @export

print.cli_doc <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

`[.cli_doc` <- function(x, ...) {
  class(x) <- setdiff(class(x), "cli_doc")
  NextMethod()
}
