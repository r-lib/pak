
cliapp <- function(theme = getOption("cli.theme"),
                   user_theme = getOption("cli.user_theme"),
                   output = c("auto", "message", "stdout", "stderr")) {

  app <- new_class(
    "cliapp",

    new = function(theme, user_theme, output)
      clii_init(app, theme, user_theme, output),

    ## Meta
    meta = function(...) {
      txt <- cli__fmt(list(...), collapse = TRUE, app = app)
      clii__message(txt, appendLF = FALSE, output = app$output, signal = app$signal)
    },

    ## Themes
    list_themes = function()
      clii_list_themes(app),
    add_theme = function(theme)
      clii_add_theme(app, theme),
    remove_theme = function(id)
      clii_remove_theme(app, id),

    ## Close container(s)
    end = function(id = NULL)
      clii_end(app, id),

    ## Generic container
    div = function(id = NULL, class = NULL, theme = NULL)
      clii_div(app, id, class, theme),

    ## Paragraphs
    par = function(id = NULL, class = NULL)
      clii_par(app, id, class),

    ## Text, wrapped
    text = function(text)
      clii_text(app, text),

    ## Text, not wrapped
    inline_text = function(text)
      clii_inline_text(app, text),

    ## Text, not wrapped, verbatim
    verbatim = function(...)
      clii_verbatim(app, ...),

    ## Markdow(ish) text, wrapped: emphasis, strong emphasis, links, code
    md_text = function(...)
      clii_md_text(app, ...),

    ## Headings
    h1 = function(text, id = NULL, class = NULL)
      clii_h1(app, text, id, class),
    h2 = function(text, id = NULL, class = NULL)
      clii_h2(app, text, id, class),
    h3 = function(text, id = NULL, class = NULL)
      clii_h3(app, text, id, class),

    ## Block quote
    blockquote = function(quote, citation = NULL, id, class = NULL)
      clii_blockquote(app, quote, citation, id, class),

    ## Lists
    ul = function(items = NULL, id = NULL, class = NULL, .close = TRUE)
      clii_ul(app, items, id, class, .close),
    ol = function(items = NULL, id = NULL, class = NULL, .close = TRUE)
      clii_ol(app, items, id, class, .close),
    dl = function(items = NULL, labels = NULL, id = NULL, class = NULL, .close = TRUE)
      clii_dl(app, items, labels, id, class, .close),
    li = function(items = NULL, labels = NULL, id = NULL, class = NULL)
      clii_li(app, items, labels, id, class),

    ## Tables
    table = function(cells, id = NULL, class = NULL)
      clii_table(app, cells, class),

    ## Alerts
    alert = function(text, id = NULL, class = NULL, wrap = FALSE)
      clii_alert(app, "alert", text, id, class, wrap),
    alert_success = function(text, id = NULL, class = NULL, wrap = FALSE)
      clii_alert(app, "alert-success", text, id, class, wrap),
    alert_danger = function(text, id = NULL, class = NULL, wrap = FALSE)
      clii_alert(app, "alert-danger", text, id, class, wrap),
    alert_warning = function(text, id = NULL, class = NULL, wrap = FALSE)
      clii_alert(app, "alert-warning", text, id, class, wrap),
    alert_info = function(text, id = NULL, class = NULL, wrap = FALSE)
      clii_alert(app, "alert-info", text, id, class, wrap),

    ## Bullets
    bullets = function(text, id = NULL, class = NULL)
      clii_bullets(app, text, id, class),

    ## Horizontal rule
    rule = function(left, center, right, id = NULL)
      clii_rule(app, left, center, right, id),

    ## Status bar
    status = function(id = NULL, msg, msg_done = NULL, msg_failed = NULL,
                      keep = FALSE, auto_result = "clear", globalenv = FALSE)
      clii_status(app, id, msg, msg_done, msg_failed, keep, auto_result, globalenv),
    status_clear = function(id = NULL, result, msg_done = NULL, msg_failed = NULL)
      clii_status_clear(app, id, result, msg_done, msg_failed),
    status_update = function(id = NULL, msg, msg_done = NULL, msg_failed = NULL)
      clii_status_update(app, id, msg, msg_done, msg_failed),

    doc = NULL,
    themes = NULL,
    styles = NULL,
    delayed_item = NULL,
    status_bar = list(),

    margin = 0,
    output = NULL,

    get_current_style = function()
      utils::tail(app$styles, 1)[[1]],

    xtext = function(text = NULL, .list = NULL, indent = 0, padding = 0, wrap = TRUE)
      clii__xtext(app, text, .list = .list, indent = indent,
                  padding = padding, wrap = wrap),

    vspace = function(n = 1)
      clii__vspace(app, n),

    inline = function(text = NULL, .list = NULL)
      clii__inline(app, text, .list = .list),

    item_text = function(type, name, cnt_id, items = list(), .list = NULL)
      clii__item_text(app, type, name, cnt_id, items, .list = .list),

    get_width = function(extra = 0)
      clii__get_width(app, extra),
    cat = function(lines)
      clii__cat(app, lines),
    cat_ln = function(lines, indent = 0, padding = 0)
      clii__cat_ln(app, lines, indent, padding)
  )

  if (! inherits(output, "connection")) output <- match.arg(output)
  app$new(theme, user_theme, output)

  app
}

clii_init <- function(app, theme, user_theme, output) {
  app$doc <- list()
  app$output <- output
  app$styles <- NULL

  if (Sys.getenv("CLI_NO_BUILTIN_THEME", "") != "true") {
    app$add_theme(builtin_theme())
  }
  app$add_theme(theme)
  app$add_theme(user_theme)

  clii__container_start(app, "body", id = "body")

  invisible(app)
}

## Text -------------------------------------------------------------

clii_text <- function(app, text) {
  app$xtext(text)
}

clii_inline_text <- function(app, text) {
  app$xtext(text, wrap = FALSE)
}

clii_verbatim <- function(app, ..., .envir) {
  style <- app$get_current_style()
  text <- unlist(strsplit(unlist(list(...)), "\n", fixed = TRUE))
  if (!is.null(style$fmt)) text <- style$fmt(text)
  app$cat_ln(text)
  invisible(app)
}

clii_md_text <- function(app, ...) {
  throw(cli_error("Markdown text is not implemented yet"))
}

## Headings ----------------------------------------------------------

clii_h1 <- function(app, text, id, class) {
  clii__heading(app, "h1", text, id, class)
}

clii_h2 <- function(app, text, id, class) {
  clii__heading(app, "h2", text, id, class)
}

clii_h3 <- function(app, text, id, class) {
  clii__heading(app, "h3", text, id, class)
}

clii__heading <- function(app, type, text, id, class) {
  id <- new_uuid()
  clii__container_start(app, type, id = id, class = class)
  on.exit(clii__container_end(app, id), add = TRUE)
  text <- app$inline(text)
  style <- app$get_current_style()
  if (is.function(style$fmt)) text <- style$fmt(text)
  app$cat_ln(text)
  invisible(app)
}

## Block quote ------------------------------------------------------

clii_blockquote <- function(app, quote, citation, id, class) {
  c1 <- clii__container_start(app, "blockquote", id = id, class = class)
  on.exit(clii__container_end(app, id), add = TRUE)
  app$xtext(quote)

  c2 <- clii__container_start(app, "cite", id = new_uuid())
  app$xtext(citation)
}

## Table ------------------------------------------------------------

clii_table <- function(app, cells, id, class) {
  throw(cli_error("Tables are not implemented yet"))
}

## Rule -------------------------------------------------------------

clii_rule <- function(app, left, center, right, id) {
  left <- app$inline(left)
  center <- app$inline(center)
  right <- app$inline(right)
  clii__container_start(app, "rule", id = id)
  on.exit(clii__container_end(app, id), add = TRUE)
  style <- app$get_current_style()
  before <- call_if_fun(style$before) %||% ""
  after <- call_if_fun(style$after) %||% ""
  width <- console_width() - ansi_nchar(before) - ansi_nchar(after)
  text <- rule(left, center, right, line = style$`line-type` %||% 1)
  text[1] <- paste0(before, text[1])
  text[length(text)] <- paste0(text[length(text)], after)
  if (is.function(style$fmt)) text <- style$fmt(text)
  app$cat_ln(text)
}

## Alerts -----------------------------------------------------------

clii_alert <- function(app, type, text, id, class, wrap) {
  clii__container_start(app, "div", id = id,
                       class = paste(class, "alert", type))
  on.exit(clii__container_end(app, id), add = TRUE)
  if (wrap) {
    app$xtext(text)
  } else {
    text <- app$inline(text)
    style <- app$get_current_style()
    before <- call_if_fun(style$before) %||% ""
    after <- call_if_fun(style$after) %||% ""
    before <- gsub(" ", "\u00a0", before)
    after <- gsub(" ", "\u00a0", after)
    text[1] <- paste0(before, text[1])
    text[length(text)] <- paste0(text[length(text)], after)
    if (is.function(style$fmt)) text <- style$fmt(text)
    app$cat_ln(text)
  }
}

## Bullets -------------------------------------------------------------

clii_bullets <- function(app, text, id, class) {
  clii__container_start(app, "div", id = id, class = paste("memo bullets", class))
  on.exit(clii__container_end(app, id), add = TRUE)

  # Normalize names a bit, so we can use them as class names
  nms <- as.character(names(text))
  length(nms) <- length(text)
  nms[is.na(nms) | nms == ""] <- "empty"
  nms[nms == " "] <- "space"
  nms <- gsub(" ", "-", nms)
  # cls is vectorized here (!)
  cls <- paste0("bullet memo-item bullet-", nms, " memo-item=", nms)

  lapply(seq_along(text), function(i) {
    iid <- new_uuid()
    clii__container_start(app, "div", id = iid, class = cls[i])
    on.exit(clii__container_end(app, iid), add = TRUE)
    app$text(text[[i]])
  })

  invisible()
}
