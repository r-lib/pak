#' List of items
#'
#' It is often useful to print out a list of items, tasks a function or
#' package performs, or a list of notes.
#'
#' @details
#'
#' Items may be formatted differently, e.g. they can have a prefix symbol.
#' Formatting is specified by the names of `text`, and can be themed.
#' cli creates a `div` element of class `bullets` for the whole bullet list.
#' Each item is another `div` element of class `bullet-<name>`, where
#' `<name>` is the name of the entry in `text`. Entries in `text` without
#' a name create a `div` element of class `bullet-empty`, and if the
#' name is a single space character, the class is `bullet-space`.
#'
#' The built-in theme defines the following item types:
#' * No name: Item without a prefix.
#' * ` `: Indented item.
#' * `*`: Item with a bullet.
#' * `>`: Item with an arrow or pointer.
#' * `v`: Item with a green "tick" symbol, like [cli_alert_success()].
#' * `x`: Item with a ref cross, like [cli_alert_danger()].
#' * `!`: Item with a yellow exclamation mark, like [cli_alert_warning()].
#' * `i`: Info item, like [cli_alert_info()].
#'
#' You can define new item type by simply defining theming for the
#' corresponding `bullet-<name>` classes.
#'
#' ```{asciicast cli-bullets}
#' cli_bullets(c(
#'         "noindent",
#'   " " = "indent",
#'   "*" = "bullet",
#'   ">" = "arrow",
#'   "v" = "success",
#'   "x" = "danger",
#'   "!" = "warning",
#'   "i" = "info"
#' ))
#' ```
#'
#' @param text Character vector of items. See details below on how names
#' are interpreted.
#' @param id Optional id of the `div.bullets` element, can be used in themes.
#' @param class Optional additional class(es) for the `div.bullets` element.
#' @param .envir Environment to evaluate the glue expressions in.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_bullets <- function(
  text,
  id = NULL,
  class = NULL,
  .envir = parent.frame()
) {
  cli__message(
    "bullets",
    list(
      text = structure(
        lapply(text, glue_cmd, .envir = .envir),
        names = names(text)
      ),
      id = id,
      class = class
    )
  )
}

#' List of verbatim items
#'
#' `cli_format_bullets_raw()` is similar to [cli_bullets()], but it does
#' not perform any inline styling or glue substitutions in the input.
#'
#' `format_bullets_raw()` returns the output instead of printing it.
#'
#' @param text Character vector of items. See details below on how names
#' are interpreted.
#' @param id Optional id of the `div.bullets` element, can be used in themes.
#' @param class Optional additional class(es) for the `div.bullets` element.
#'
#' @seealso These functions support [inline markup][inline-markup].
#' @seealso See [cli_bullets()] for examples.
#' @family functions supporting inline markup
#' @export

cli_bullets_raw <- function(text, id = NULL, class = NULL) {
  text <- cli_escape(text)
  cli__message(
    "bullets",
    list(
      text = structure(
        lapply(text, glue_no_cmd),
        names = names(text)
      ),
      id = id,
      class = class
    )
  )
}

#' @rdname cli_bullets_raw
#' @export

format_bullets_raw <- function(text, id = NULL, class = NULL) {
  cli_fmt(cli_bullets_raw(text, id, class))
}
