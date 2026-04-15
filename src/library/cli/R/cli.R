#' Compose multiple cli functions
#'
#' `cli()` will record all `cli_*` calls in `expr`, and emit them together
#' in a single message. This is useful if you want to built a larger
#' piece of output from multiple `cli_*` calls.
#'
#' Use this function to build a more complex piece of CLI that would not
#' make sense to show in pieces.
#'
#' ```{asciicast cli-cli}
#' cli({
#'   cli_h1("Title")
#'   cli_h2("Subtitle")
#'   cli_ul(c("this", "that", "end"))
#' })
#' ```
#'
#' @param expr Expression that contains `cli_*` calls. Their output is
#' collected and sent as a single message.
#' @return Nothing.
#'
#' @export

cli <- function(expr) {
  cond <- cli__message_create("meta", cli__rec(expr))
  # cli() might be nested
  record <- getOption("cli.record")
  if (is.null(record)) {
    cli__message_emit(cond)
  } else {
    cli_recorded[[record]] <- c(cli_recorded[[record]], list(cond))
  }
  invisible()
}

cli__rec <- function(expr) {
  id <- new_uuid()
  cli_recorded[[id]] <- list()
  on.exit(rm(list = id, envir = cli_recorded), add = TRUE)
  old <- options(cli.record = id)
  on.exit(options(old), add = TRUE)
  expr
  cli_recorded[[id]]
}

cli__fmt <- function(
  record,
  collapse = FALSE,
  strip_newline = FALSE,
  app = NULL
) {
  app <- app %||% default_app() %||% start_app(.auto_close = FALSE)

  old <- app$output
  oldsig <- app$signal
  on.exit(app$output <- old, add = TRUE)
  on.exit(app$signal <- oldsig, add = TRUE)
  out <- rawConnection(raw(1000), open = "wb")
  on.exit(close(out), add = TRUE)
  app$output <- out
  app$signal <- FALSE

  for (msg in record) {
    do.call(app[[msg$type]], msg$args)
  }

  txt <- rawToChar(rawConnectionValue(out))
  Encoding(txt) <- "UTF-8"
  if (!collapse) {
    txt <- unlist(strsplit(txt, "\n", fixed = TRUE))
  } else if (strip_newline) {
    txt <- substr(txt, 1, nchar(txt) - 1L)
  }
  txt
}

#' Capture the output of cli functions instead of printing it
#'
#' @param expr Expression to evaluate, containing `cli_*()` calls,
#'   typically.
#' @param collapse Whether to collapse the output into a single character
#'   scalar, or return a character vector with one element for each line.
#' @param strip_newline Whether to strip the trailing newline.
#'
#' @export
#' @examples
#' cli_fmt({
#'   cli_alert_info("Loading data file")
#'   cli_alert_success("Loaded data file")
#' })

cli_fmt <- function(expr, collapse = FALSE, strip_newline = FALSE) {
  rec <- cli__rec(expr)
  cli__fmt(rec, collapse, strip_newline)
}

#' Format and returns a line of text
#'
#' You can use this function to format a line of cli text, without emitting
#' it to the screen. It uses [cli_text()] internally.
#'
#' `format_inline()` performs no width-wrapping.
#'
#' @param ... Passed to [cli_text()].
#' @param .envir Environment to evaluate the expressions in.
#' @param collapse Whether to collapse the result if it has multiple
#'   lines, e.g. because of `\f` characters.
#' @param keep_whitespace Whether to keep all whitepace (spaces, newlines
#'   and form feeds) as is in the input.
#' @return Character scalar, the formatted string.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export
#' @examples
#' format_inline("A message for {.emph later}, thanks {.fn format_inline}.")

format_inline <- function(
  ...,
  .envir = parent.frame(),
  collapse = TRUE,
  keep_whitespace = TRUE
) {
  opts <- options(cli.width = Inf)
  on.exit(options(opts), add = TRUE)
  fun <- if (keep_whitespace) cli_inline else cli_text
  cli_fmt(
    fun(..., .envir = .envir),
    collapse = collapse,
    strip_newline = TRUE
  )
}

#' CLI text
#'
#' Write some text to the screen. This function is most appropriate for
#' longer paragraphs. See [cli_alert()] for shorter status messages.
#'
#' @details
#'
#' ## Text wrapping
#'
#' Text is wrapped to the console width, see [console_width()].
#'
#' ```{asciicast cli-text}
#' cli_text(cli:::lorem_ipsum())
#' ```
#'
#' ## New lines
#'
#' A `cli_text()` call always appends a newline character to the end.
#'
#' ```{asciicast cli-text-newline}
#' cli_text("First line.")
#' cli_text("Second line.")
#' ```
#'
#' ## Styling
#'
#' You can use [inline markup][inline-markup], as usual.
#'
#' ```{asciicast cli-text-markup}
#' cli_text("The {.fn cli_text} function in the {.pkg cli} package.")
#' ```
#'
#' ## Interpolation
#'
#' String interpolation via glue works as usual. Interpolated vectors
#' are collapsed.
#'
#' ```{asciicast cli-text-glue}
#' pos <- c(5, 14, 25, 26)
#' cli_text("We have {length(pos)} missing measurements: {pos}.")
#' ```
#'
#' ## Styling and interpolation
#'
#' Use double braces to combine styling and string interpolation.
#'
#' ```{asciicast cli-text-glue-style}
#' fun <- "cli-text"
#' pkg <- "cli"
#' cli_text("The {.fn {fun}} function in the {.pkg {pkg}} package.")
#' ```
#'
#' ## Multiple arguments
#'
#' Arguments are concatenated.
#'
#' ```{asciicast cli-text-concat}
#' cli_text(c("This ", "will ", "all "), "be ", "one ", "sentence.")
#' ```
#'
#' ## Containers
#'
#' You can use `cli_text()` within cli [containers].
#'
#' ```{asciicast cli-text-containers}
#' ul <- cli_ul()
#' cli_li("First item.")
#' cli_text("Still the {.emph first} item")
#' cli_li("Second item.")
#' cli_text("Still the {.emph second} item")
#' cli_end(ul)
#' ```
#'
#' @param ... The text to show, in character vectors. They will be
#'   concatenated into a single string. Newlines are _not_ preserved.
#' @param .envir Environment to evaluate the glue expressions in.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_text <- function(..., .envir = parent.frame()) {
  cli__message(
    "text",
    list(text = glue_cmd(..., .envir = .envir, .call = sys.call()))
  )
}

cli_inline <- function(..., .envir = parent.frame()) {
  cli__message(
    "inline_text",
    list(
      text = glue_cmd(..., .envir = .envir, .call = sys.call(), .trim = FALSE)
    )
  )
}

#' CLI verbatim text
#'
#' It is not wrapped, but printed as is. Long lines will overflow.
#' No glue substitution is performed on verbatim text.
#'
#' @details
#'
#' ## Line breaks
#'
#' ```{asciicast cli-verbatim}
#' cli_verbatim("This has\nthree\nlines,")
#' ```
#'
#' ## Special characters
#'
#' No glue substitution happens here.
#'
#' ```{asciicast cli-verbatim-2}
#' cli_verbatim("No string {interpolation} or {.emph styling} here")
#' ```
#'
#' @param ... The text to show, in character vectors. Each element is
#'   printed on a new line.
#' @param .envir Environment to evaluate the glue expressions in.
#'
#' @seealso [cli_code()] for printing R or other source code.
#' @export

cli_verbatim <- function(..., .envir = parent.frame()) {
  cli__message("verbatim", c(list(...), list(.envir = .envir)))
}

#' CLI headings
#'
#' cli has three levels of headings.
#'
#' @details
#'
#' This is how the headings look with the default builtin theme.
#'
#' ```{asciicast, cli-h1}
#' cli_h1("Header {.emph 1}")
#' cli_h2("Header {.emph 2}")
#' cli_h3("Header {.emph 3}")
#' ```
#'
#' @param text Text of the heading. It can contain inline markup.
#' @param id Id of the heading element, string. It can be used in themes.
#' @param class Class of the heading element, string. It can be used in
#'   themes.
#' @param .envir Environment to evaluate the glue expressions in.
#'
#' @seealso These functions supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_h1 <- function(text, id = NULL, class = NULL, .envir = parent.frame()) {
  cli__message(
    "h1",
    list(
      text = glue_cmd(text, .envir = .envir, .call = sys.call()),
      id = id,
      class = class
    )
  )
}

#' @rdname cli_h1
#' @export

cli_h2 <- function(text, id = NULL, class = NULL, .envir = parent.frame()) {
  cli__message(
    "h2",
    list(
      text = glue_cmd(text, .envir = .envir, .call = sys.call()),
      id = id,
      class = class
    )
  )
}

#' @rdname cli_h1
#' @export

cli_h3 <- function(text, id = NULL, class = NULL, .envir = parent.frame()) {
  cli__message(
    "h3",
    list(
      text = glue_cmd(text, .envir = .envir, .call = sys.call()),
      id = id,
      class = class
    )
  )
}

#' Generic CLI container
#'
#' See [containers]. A `cli_div` container is special, because it may
#' add new themes, that are valid within the container.
#'
#' @details
#'
#' ## Custom themes
#'
#' ```{asciicast cli-div}
#' d <- cli_div(theme = list(h1 = list(color = "cyan",
#'                                     "font-weight" = "bold")))
#' cli_h1("Custom title")
#' cli_end(d)
#' ```
#'
#' ## Auto-closing
#'
#' By default a `cli_div()` is closed automatically when the calling
#' frame exits.
#'
#' ```{asciicast cli-div-close}
#' div <- function() {
#'   cli_div(class = "tmp", theme = list(.tmp = list(color = "yellow")))
#'   cli_text("This is yellow")
#' }
#' div()
#' cli_text("This is not yellow any more")
#' ```
#'
#' @param id Element id, a string. If `NULL`, then a new id is generated
#'   and returned.
#' @param class Class name, sting. Can be used in themes.
#' @param theme A custom theme for the container. See [themes].
#' @param .auto_close Whether to close the container, when the calling
#'   function finishes (or `.envir` is removed, if specified).
#' @param .envir Environment to evaluate the glue expressions in. It is
#'   also used to auto-close the container if `.auto_close` is `TRUE`.
#' @return The id of the new container element, invisibly.
#'
#' @export

cli_div <- function(
  id = NULL,
  class = NULL,
  theme = NULL,
  .auto_close = TRUE,
  .envir = parent.frame()
) {
  cli__message(
    "div",
    list(id = id, class = class, theme = theme),
    .auto_close = .auto_close,
    .envir = .envir
  )
}

#' CLI paragraph
#'
#' The builtin theme leaves an empty line between paragraphs.
#' See also [containers].
#'
#' ```{asciicast cli-par}
#' clifun <- function() {
#'   cli_par()
#'   cli_text(cli:::lorem_ipsum())
#' }
#' clifun()
#' clifun()
#' ```
#'
#' @param id Element id, a string. If `NULL`, then a new id is generated
#'   and returned.
#' @param class Class name, sting. Can be used in themes.
#' @inheritParams cli_div
#' @return The id of the new container element, invisibly.
#'
#' @export

cli_par <- function(
  id = NULL,
  class = NULL,
  .auto_close = TRUE,
  .envir = parent.frame()
) {
  cli__message(
    "par",
    list(id = id, class = class),
    .auto_close = .auto_close,
    .envir = .envir
  )
}

#' Close a CLI container
#'
#' Containers aut0-close by default, but sometimes you need to explicitly
#' close them. Closing a container also closes all of its nested
#' containers.
#'
#' @details
#'
#' ## Explicit closing
#'
#' ```{asciicast cli-end}
#' cnt <- cli_par()
#' cli_text("First paragraph.")
#' cli_end(cnt)
#' cnt <- cli_par()
#' cli_text("Second paragraph.")
#' cli_end(cnt)
#' ```
#'
#' ## Closing a stack of containers
#'
#' ```{asciicast cli-end-many}
#' list <- cli_ul()
#' cli_li("Item one:")
#' cli_li("Item two:")
#' cli_par()
#' cli_text("Still item two.")
#' cli_end(list)
#' cli_text("Not in the list any more")
#' ```
#'
#' ## Omitting `id`
#'
#' If `id` is omitted, the container that was opened last will be closed.
#'
#' ```{asciicast cli-end-noid}
#' cli_par()
#' cli_text("First paragraph")
#' cli_end()
#' cli_par()
#' cli_text("Second paragraph")
#' cli_end()
#' ```
#'
#' ## Debugging containers
#'
#' You can use the internal `cli:::cli_debug_doc()` function to see the
#' currently open containers.
#'
#' ```{asciicast cli-end-debug}
#' fun <- function() {
#'   cli_div(id = "mydiv")
#'   cli_par(class = "myclass")
#'   cli:::cli_debug_doc()
#' }
#' fun()
#' ```
#'
#' @param id Id of the container to close. If missing, the current
#' container is closed, if any.
#'
#' @export

cli_end <- function(id = NULL) {
  cli__message("end", list(id = id %||% NA_character_))
}

#' Unordered CLI list
#'
#' An unordered list is a container, see [containers].
#'
#' @details
#'
#' ## Adding all items at once
#'
#' ```{asciicast cli-ul}
#' fun <- function() {
#'   cli_ul(c("one", "two", "three"))
#' }
#' fun()
#' ```
#'
#' ## Adding items one by one
#'
#' ```{asciicast cli-ul-2}
#' fun <- function() {
#'   cli_ul()
#'   cli_li("{.emph one}")
#'   cli_li("{.emph two}")
#'   cli_li("{.emph three}")
#'   cli_end()
#' }
#' fun()
#' ```
#'
#' @param items If not `NULL`, then a character vector. Each element of
#'   the vector will be one list item, and the list container will be
#'   closed by default (see the `.close` argument).
#' @param id Id of the list container. Can be used for closing it with
#'   [cli_end()] or in themes. If `NULL`, then an id is generated and
#'   returned invisibly.
#' @param class Class of the list container. Can be used in themes.
#' @param .close Whether to close the list container if the `items` were
#'   specified. If `FALSE` then new items can be added to the list.
#' @inheritParams cli_div
#' @return The id of the new container element, invisibly.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_ul <- function(
  items = NULL,
  id = NULL,
  class = NULL,
  .close = TRUE,
  .auto_close = TRUE,
  .envir = parent.frame()
) {
  cli__message(
    "ul",
    list(
      items = lapply(items, glue_cmd, .envir = .envir, .call = sys.call()),
      id = id,
      class = class,
      .close = .close
    ),
    .auto_close = .auto_close,
    .envir = .envir
  )
}

#' Ordered CLI list
#'
#' An ordered list is a container, see [containers].
#'
#' @details
#'
#' ## Adding all items at once
#'
#' ```{asciicast cli-ol}
#' fun <- function() {
#'   cli_ol(c("one", "two", "three"))
#' }
#' fun()
#' ```
#'
#' ## Adding items one by one
#'
#' ```{asciicast cli-ol-2}
#' ## Adding items one by one
#' fun <- function() {
#'   cli_ol()
#'   cli_li("{.emph one}")
#'   cli_li("{.emph two}")
#'   cli_li("{.emph three}")
#'   cli_end()
#' }
#' fun()
#' ```
#'
#' ## Nested lists
#'
#' ```{asciicast cli-ol-3}
#' fun <- function() {
#'   cli_div(theme = list(ol = list("margin-left" = 2)))
#'   cli_ul()
#'   cli_li("one")
#'   cli_ol(c("foo", "bar", "foobar"))
#'   cli_li("two")
#'   cli_end()
#'   cli_end()
#' }
#' fun()
#' ```
#'
#' @inheritParams cli_ul
#' @return The id of the new container element, invisibly.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_ol <- function(
  items = NULL,
  id = NULL,
  class = NULL,
  .close = TRUE,
  .auto_close = TRUE,
  .envir = parent.frame()
) {
  cli__message(
    "ol",
    list(
      items = lapply(items, glue_cmd, .envir = .envir, .call = sys.call()),
      id = id,
      class = class,
      .close = .close
    ),
    .auto_close = .auto_close,
    .envir = .envir
  )
}

#' Definition list
#'
#' A definition list is a container, see [containers].
#'
#' @details
#'
#' ## All items at once
#'
#' ```{asciicast cli-dl}
#' fun <- function() {
#'   cli_dl(c(foo = "one", bar = "two", baz = "three"))
#' }
#' fun()
#' ```
#'
#' ## Items one by one
#'
#' ```{asciicast cli-dl-2}
#' fun <- function() {
#'   cli_dl()
#'   cli_li(c(foo = "{.emph one}"))
#'   cli_li(c(bar = "two"))
#'   cli_li(c(baz = "three"))
#' }
#' fun()
#' ```
#'
#' @param items Named character vector, or `NULL`. If not `NULL`, they
#'   are used as list items.
#' @param labels Item labels. Defaults the names in `items`.
#' @inheritParams cli_ul
#' @return The id of the new container element, invisibly.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_dl <- function(
  items = NULL,
  labels = names(items),
  id = NULL,
  class = NULL,
  .close = TRUE,
  .auto_close = TRUE,
  .envir = parent.frame()
) {
  if (!is.null(items) && !is_named(items)) {
    throw(cli_error(
      "{.arg items} must be a named character vector",
      "i" = if (!is_named(items)) "{.arg items} is not named"
    ))
  }

  cli__message(
    "dl",
    list(
      items = lapply(items, glue_cmd, .envir = .envir, .call = sys.call()),
      labels = if (!is.null(labels)) {
        lapply(labels, glue_cmd, .envir = .envir, .call = sys.call())
      },
      id = id,
      class = class,
      .close = .close
    ),
    .auto_close = .auto_close,
    .envir = .envir
  )
}

#' CLI list item(s)
#'
#' A list item is a container, see [containers].
#'
#' @details
#'
#' ## Nested lists
#'
#' ```{asciicast cli-li}
#' fun <- function() {
#'   ul <- cli_ul()
#'   cli_li("one:")
#'   cli_ol(letters[1:3])
#'   cli_li("two:")
#'   cli_li("three")
#'   cli_end(ul)
#' }
#' fun()
#' ```
#'
#' @param items Character vector of items, or `NULL`.
#' @param labels For definition lists the item labels.
#' @param id Id of the new container. Can be used for closing it with
#'   [cli_end()] or in themes. If `NULL`, then an id is generated and
#'   returned invisibly.
#' @param class Class of the item container. Can be used in themes.
#' @inheritParams cli_div
#' @return The id of the new container element, invisibly.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_li <- function(
  items = NULL,
  labels = names(items),
  id = NULL,
  class = NULL,
  .auto_close = TRUE,
  .envir = parent.frame()
) {
  cli__message(
    "li",
    list(
      items = lapply(items, glue_cmd, .envir = .envir, .call = sys.call()),
      labels = if (!is.null(labels)) {
        lapply(labels, glue_cmd, .envir = .envir, .call = sys.call())
      },
      id = id,
      class = class
    ),
    .auto_close = .auto_close,
    .envir = .envir
  )
}

#' CLI alerts
#'
#' Alerts are typically short status messages.
#'
#' @details
#'
#' ## Success
#'
#' ```{asciicast alert-success}
#' nbld <- 11
#' tbld <- prettyunits::pretty_sec(5.6)
#' cli_alert_success("Built {.emph {nbld}} status report{?s} in {tbld}.")
#' ```
#'
#' ## Info
#'
#' ```{asciicast alert-info}
#' cfl <- "~/.cache/files/latest.cache"
#' cli_alert_info("Updating cache file {.path {cfl}}.")
#' ```
#'
#' ## Warning
#'
#' ```{asciicast alert-warning}
#' cfl <- "~/.cache/files/latest.cache"
#' cli_alert_warning("Failed to update cache file {.path {cfl}}.")
#' ```
#'
#' ## Danger
#'
#' ```{asciicast alert-danger}
#' cfl <- "~/.config/report.yaml"
#' cli_alert_danger("Cannot validate config file at {.path {cfl}}.")
#' ```
#'
#' ## Text wrapping
#'
#' Alerts are printed without wrapping, unless you set `wrap = TRUE`:
#'
#' ```{asciicast alert-wrap, asciicast_rows = 4}
#' cli_alert_info("Data columns: {.val {names(mtcars)}}.")
#' cli_alert_info("Data columns: {.val {names(mtcars)}}.", wrap = TRUE)
#' ```
#'
#' @param text Text of the alert.
#' @param id Id of the alert element. Can be used in themes.
#' @param class Class of the alert element. Can be used in themes.
#' @param wrap Whether to auto-wrap the text of the alert.
#' @param .envir Environment to evaluate the glue expressions in.
#'
#' @seealso These functions supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_alert <- function(
  text,
  id = NULL,
  class = NULL,
  wrap = FALSE,
  .envir = parent.frame()
) {
  cli__message(
    "alert",
    list(
      text = glue_cmd(text, .envir = .envir, .call = sys.call()),
      id = id,
      class = class,
      wrap = wrap
    )
  )
}

#' @rdname cli_alert
#' @export

cli_alert_success <- function(
  text,
  id = NULL,
  class = NULL,
  wrap = FALSE,
  .envir = parent.frame()
) {
  cli__message(
    "alert_success",
    list(
      text = glue_cmd(text, .envir = .envir, .call = sys.call()),
      id = id,
      class = class,
      wrap = wrap
    )
  )
}

#' @rdname cli_alert
#' @export

cli_alert_danger <- function(
  text,
  id = NULL,
  class = NULL,
  wrap = FALSE,
  .envir = parent.frame()
) {
  cli__message(
    "alert_danger",
    list(
      text = glue_cmd(text, .envir = .envir, .call = sys.call()),
      id = id,
      class = class,
      wrap = wrap
    )
  )
}

#' @rdname cli_alert
#' @export

cli_alert_warning <- function(
  text,
  id = NULL,
  class = NULL,
  wrap = FALSE,
  .envir = parent.frame()
) {
  cli__message(
    "alert_warning",
    list(
      text = glue_cmd(text, .envir = .envir, .call = sys.call()),
      id = id,
      class = class,
      wrap = wrap
    )
  )
}

#' @rdname cli_alert
#' @export

cli_alert_info <- function(
  text,
  id = NULL,
  class = NULL,
  wrap = FALSE,
  .envir = parent.frame()
) {
  cli__message(
    "alert_info",
    list(
      text = glue_cmd(text, .envir = .envir, .call = sys.call()),
      id = id,
      class = class,
      wrap = wrap
    )
  )
}

#' CLI horizontal rule
#'
#' It can be used to separate parts of the output.
#'
#' @details
#'
#' ## Inline styling and interpolation
#'
#' ```{asciicast cli-rule}
#' pkg <- "mypackage"
#' cli_rule(left = "{.pkg {pkg}} results")
#' ```
#'
#' ## Theming
#'
#' The line style of the rule can be changed via the the `line-type`
#' property. Possible values are:
#'
#' * `"single"`: (same as `1`), a single line,
#' * `"double"`: (same as `2`), a double line,
#' * `"bar1"`, `"bar2"`, `"bar3"`, etc., `"bar8"` uses varying height bars.
#'
#' Colors and background colors can similarly changed via a theme.
#'
#' ```{asciicast cli-rule-line-type}
#' d <- cli_div(theme = list(rule = list(
#'   color = "cyan",
#'   "line-type" = "double")))
#' cli_rule("Summary", right = "{.pkg mypackage}")
#' cli_end(d)
#' ```
#'
#' @param .envir Environment to evaluate the glue expressions in.
#' @inheritParams rule
#' @inheritParams cli_div
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_rule <- function(
  left = "",
  center = "",
  right = "",
  id = NULL,
  .envir = parent.frame()
) {
  cli__message(
    "rule",
    list(
      left = glue_cmd(left, .envir = .envir, .call = sys.call()),
      center = glue_cmd(center, .envir = .envir, .call = sys.call()),
      right = glue_cmd(right, .envir = .envir, .call = sys.call()),
      id = id
    )
  )
}

#' CLI block quote
#'
#' A section that is quoted from another source. It is typically indented.
#'
#' @details
#'
#' ```{asciicast cli-blockquote}
#' evil <- paste(
#'   "The real problem is that programmers have spent far too much time",
#'   "worrying about efficiency in the wrong places and at the wrong",
#'   "times; premature optimization is the root of all evil (or at least",
#'   "most of it) in programming.")
#' cli_blockquote(evil, citation = "Donald Ervin Knuth")
#' ```
#'
#' @param quote Text of the quotation.
#' @param citation Source of the quotation, typically a link or the name
#'   of a person.
#' @inheritParams cli_div
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

cli_blockquote <- function(
  quote,
  citation = NULL,
  id = NULL,
  class = NULL,
  .envir = parent.frame()
) {
  cli__message(
    "blockquote",
    list(
      quote = glue_cmd(quote, .envir = .envir, .call = sys.call()),
      citation = glue_cmd(citation, .envir = .envir, .call = sys.call()),
      id = id,
      class = class
    )
  )
}

#' A block of code
#'
#' A helper function that creates a `div` with class `code` and then calls
#' `cli_verbatim()` to output code lines. The builtin theme formats these
#' containers specially. In particular, it adds syntax highlighting to
#' valid R code.
#'
#' @details
#'
#' ```{asciicast cli-code}
#' myfun <- function() {
#'   message("Just an example function")
#'   graphics::pairs(iris, col = 1:4)
#' }
#' cli_code(format(myfun))
#' ```
#'
#' @param lines Character vector, each line will be a line of code, and
#'   newline characters also create new lines. Note that _no_ glue
#'   substitution is performed on the code.
#' @param ... More character vectors, they are appended to `lines`.
#' @param language Programming language. This is also added as a class,
#'   in addition to `code`.
#' @param .auto_close Passed to `cli_div()` when creating the container of
#'   the code. By default the code container is closed after emitting
#'   `lines` and `...` via `cli_verbatim()`. You can keep that container
#'   open with `.auto_close` and/or `.envir`, and then calling
#'   `cli_verbatim()` to add (more) code. Note that the code will be
#'   formatted and syntax highlighted separately for each `cli_verbatim()`
#'   call.
#' @param .envir Passed to `cli_div()` when creating the container of the
#'   code.
#' @return The id of the container that contains the code.
#'
#' @export

cli_code <- function(
  lines = NULL,
  ...,
  language = "R",
  .auto_close = TRUE,
  .envir = environment()
) {
  lines <- c(lines, unlist(list(...)))
  id <- cli_div(
    class = paste("code", language),
    .auto_close = .auto_close,
    .envir = .envir
  )
  cli_verbatim(lines)
  invisible(id)
}

cli_recorded <- new.env(parent = emptyenv())

cli__message <- function(
  type,
  args,
  .auto_close = TRUE,
  .envir = NULL,
  record = getOption("cli.record")
) {
  if ("id" %in% names(args) && is.null(args$id)) args$id <- new_uuid()

  if (.auto_close && !is.null(.envir) && !identical(.envir, .GlobalEnv)) {
    if (type == "status") {
      defer(
        cli_status_clear(id = args$id, result = args$auto_result),
        envir = .envir,
        priority = "first"
      )
    } else {
      defer(cli_end(id = args$id), envir = .envir, priority = "first")
    }
  }

  cond <- cli__message_create(type, args)

  if (is.null(record)) {
    cli__message_emit(cond)
  } else {
    cli_recorded[[record]] <- c(cli_recorded[[record]], list(cond))
  }

  invisible(args$id)
}

cli__message_create <- function(type, args) {
  cond <- list(
    message = paste("cli message", type),
    type = type,
    args = args,
    pid = clienv$pid
  )

  class(cond) <- c(
    getOption("cli.message_class"),
    "cli_message",
    "condition"
  )

  cond
}

cli__message_emit <- function(cond) {
  withRestarts(
    {
      signalCondition(cond)
      cli__default_handler(cond)
    },
    cli_message_handled = function() NULL
  )
}

cli__default_handler <- function(msg) {
  custom_handler <- getOption("cli.default_handler")

  if (is.function(custom_handler)) {
    custom_handler(msg)
  } else {
    cli_server_default(msg)
  }
}
