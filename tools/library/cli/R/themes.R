
#' List the currently active themes
#'
#' If there is no active app, then it calls [start_app()].
#'
#' @return A list of data frames with the active themes.
#' Each data frame row is a style that applies to selected CLI tree nodes.
#' Each data frame has columns:
#' * `selector`: The original CSS-like selector string. See [themes].
#' * `parsed`: The parsed selector, as used by cli for matching to nodes.
#' * `style`: The original style.
#' * `cnt`: The id of the container the style is currently applied to, or
#'   `NA` if the style is not used.
#'
#' @export
#' @seealso [themes]

cli_list_themes <- function() {
  app <- default_app() %||% start_app()
  app$list_themes()
}

clii_list_themes <- function(app) {
  app$themes
}

clii_add_theme <- function(app, theme) {
  id <- new_uuid()
  app$themes <-
    c(app$themes, structure(list(theme_create(theme)), names = id))
  id
}

clii_remove_theme <- function(app, id) {
  if (! id %in% names(app$themes)) return(invisible(FALSE))
  app$themes[[id]] <- NULL
  invisible(TRUE)
}

#' The built-in CLI theme
#'
#' This theme is always active, and it is at the bottom of the theme
#' stack. See [themes].
#'
#' # Showcase
#'
#' ```{asciicast builtin-theme}
#' cli_h1("Heading 1")
#' cli_h2("Heading 2")
#' cli_h3("Heading 3")
#'
#' cli_par()
#' cli_alert_danger("Danger alert")
#' cli_alert_warning("Warning alert")
#' cli_alert_info("Info alert")
#' cli_alert_success("Success alert")
#' cli_alert("Alert for starting a process or computation",
#'   class = "alert-start")
#' cli_end()
#'
#' cli_text("Packages and versions: {.pkg cli} {.version 1.0.0}.")
#' cli_text("Time intervals: {.timestamp 3.4s}")
#'
#' cli_text("{.emph Emphasis} and  {.strong strong emphasis}")
#'
#' cli_text("This is a piece of code: {.code sum(x) / length(x)}")
#' cli_text("Function names: {.fn cli::simple_theme}")
#'
#' cli_text("Files: {.file /usr/bin/env}")
#' cli_text("URLs: {.url https://r-project.org}")
#'
#' cli_h2("Longer code chunk")
#' cli_par(class = "code R")
#' cli_verbatim(
#'   '# window functions are useful for grouped mutates',
#'   'mtcars %>%',
#'   '  group_by(cyl) %>%',
#'   '  mutate(rank = min_rank(desc(mpg)))')
#' ```
#'
#' @seealso [themes], [simple_theme()].
#' @return A named list, a CLI theme.
#'
#' @param dark Whether to use a dark theme. The `cli.theme_dark` option
#'   can be used to request a dark theme explicitly. If this is not set,
#'   or set to `"auto"`, then cli tries to detect a dark theme, this
#'   works in recent RStudio versions and in iTerm on macOS.
#' @export

builtin_theme <- function(dark = getOption("cli.theme_dark", "auto")) {

  dark <- detect_dark_theme(dark)

  list(
    body = list(
      "class-map" = list(
        fs_path = "file",
        "cli-progress-bar" = "progress-bar"
      )
    ),

    h1 = list(
      "font-weight" = "bold",
      "margin-top" = 1,
      "margin-bottom" = 0,
      fmt = function(x) cli::rule(x, line_col = "cyan")),
    h2 = list(
      "font-weight" = "bold",
      "margin-top" = 1,
      "margin-bottom" = 1,
      fmt = function(x) paste0(symbol$line, symbol$line, " ", x, " ",
                               symbol$line, symbol$line)),
    h3 = list(
      "margin-top" = 1,
      fmt = function(x) paste0(symbol$line, symbol$line, " ", x, " ")),

    ".alert" = list(
      before = function() paste0(symbol$arrow_right, " ")
    ),
    ".alert-success" = list(
      before = function() paste0(col_green(symbol$tick), " ")
    ),
    ".alert-danger" = list(
      before = function() paste0(col_red(symbol$cross), " ")
    ),
    ".alert-warning" = list(
      before = function() paste0(col_yellow("!"), " ")
    ),
    ".alert-info" = list(
      before = function() paste0(col_cyan(symbol$info), " ")
    ),

    ".bullets .bullet-empty" = list(),
    ".bullets .bullet-space" = list("margin-left" = 2),
    ".bullets .bullet-v" = list(
      "text-exdent" = 2,
      before = function(x) paste0(col_green(symbol$tick), " ")
    ),
    ".bullets .bullet-x" = list(
      "text-exdent" = 2,
      before = function(x) paste0(col_red(symbol$cross), " ")
    ),
    ".bullets .bullet-!" = list(
      "text-exdent" = 2,
      before = function(x) paste0(col_yellow("!"), " ")
    ),
    ".bullets .bullet-i" = list(
      "text-exdent" = 2,
      before = function(x) paste0(col_cyan(symbol$info), " ")
    ),
    ".bullets .bullet-*" = list(
      "text-exdent" = 2,
      before = function(x) paste0(col_cyan(symbol$bullet), " ")
    ),
    ".bullets .bullet->" = list(
      "text-exdent" = 2,
      before = function(x) paste0(symbol$arrow_right, " ")
    ),
    ".bullets .bullet-1" = list(
    ),

    par = list("margin-top" = 0, "margin-bottom" = 1),
    ul = list(
      "list-style-type" = function() symbol$bullet
    ),

    # these are tags in HTML, but in cli they are inline
    span.dt = list(postfix = ": "),
    span.dd = list(),

    # This means that list elements have a margin, if they are nested
    "ul ul li" = list("margin-left" = 2),
    "ul ol li" = list("margin-left" = 2),
    "ul dl li" = list("margin-left" = 2),
    "ol ul li" = list("margin-left" = 2),
    "ol ol li" = list("margin-left" = 2),
    "ol dl li" = list("margin-left" = 2),
    "ol ul li" = list("margin-left" = 2),
    "ol ol li" = list("margin-left" = 2),
    "ol dl li" = list("margin-left" = 2),

    blockquote = list("padding-left" = 4L, "padding-right" = 10L,
                      "font-style" = "italic", "margin-top" = 1L,
                      "margin-bottom" = 1L,
                      before = function() symbol$dquote_left,
                      after = function() symbol$dquote_right),
    "blockquote cite" = list(
      before = function() paste0(symbol$em_dash, " "),
      "font-style" = "italic",
      "font-weight" = "bold"
    ),

    .code = list(fmt = format_code(dark)),
    .code.R = list(fmt = format_r_code(dark)),

    span.emph = list("font-style" = "italic"),
    span.strong = list("font-weight" = "bold"),
    span.code = theme_code_tick(dark),

    span.q   = list(fmt = quote_weird_name2),
    span.pkg = list(color = "blue"),
    span.fn = theme_function(dark),
    span.fun = theme_function(dark),
    span.arg = theme_code_tick(dark),
    span.kbd = list(before = "[", after = "]", color = "blue"),
    span.key = list(before = "[", after = "]", color = "blue"),
    span.file = theme_file(),
    span.path = theme_file(),
    span.email = list(
      color = "blue",
      transform = function(x) make_link(x, type = "email"),
      fmt = quote_weird_name
    ),
    span.url = list(
      before = "<", after = ">",
      color = "blue", "font-style" = "italic",
      transform = function(x) make_link(x, type = "url")
    ),
    span.href = list(
      transform = function(x) make_link(x, type = "href")
    ),
    span.help = list(
      transform = function(x) make_link(x, type = "help")
    ),
    span.topic = list(
      transform = function(x) make_link(x, type = "topic")
    ),
    span.vignette = list(
      transform = function(x) make_link(x, type = "vignette")
    ),
    span.run = list(
      transform = function(x) make_link(x, type = "run")
    ),
    span.var = theme_code_tick(dark),
    span.col = theme_code_tick(dark),
    span.str = list(fmt = encode_string),
    span.envvar = theme_code_tick(dark),
    span.val = list(
      transform = function(x, ...) cli_format(x, ...),
      color = "blue"
    ),
    span.field = list(color = "green"),
    span.cls = list(collapse = "/", color = "blue", before = "<", after = ">"),
    "span.progress-bar" = list(
      transform = theme_progress_bar,
      color = "green"
    ),
    span.obj_type_friendly = list(
      transform = function(x) format_inline(typename(x))
    ),
    span.type = list(
      transform = function(x) format_inline(typename(x))
    ),
    span.or = list("vec-sep2" = " or ", "vec-last" = ", or "),
    span.timestamp = list(before = "[", after = "]", color = "grey")
  )
}

encode_string <- function(x) {
  encodeString(x, quote = "\"")
}

quote_weird_name0 <- function(x) {
  x <- gsub(" ", "\u00a0", x)
  x2 <- ansi_strip(x)

  fc <- first_character(x2)
  sc <- second_character(x2)
  lc <- last_character(x2)

  wfst <- !is_alnum(fc, ok = "~") || (fc == "~" && !is_alnum(sc))
  wlst <- !is_alnum(lc)

  if (wfst || wlst) {
    lsp <- leading_space(x2)
    tsp <- trailing_space(x2)
    if (nzchar(lsp)) {
      x <- paste0(
        bg_blue(lsp),
        ansi_substr(x, nchar(lsp) + 1, ansi_nchar(x))
      )
    }
    if (nzchar(tsp)) {
      x <- paste0(
        ansi_substr(x, 1, ansi_nchar(x) - nchar(tsp)),
        bg_blue(tsp)
      )
    }
  }

  list(x, wfst || wlst)
}

quote_weird_name <- function(x) {
  x2 <- quote_weird_name0(x)
  if (x2[[2]] || num_ansi_colors() == 1) {
    x2[[1]] <- paste0("'", x2[[1]], "'")
  }
  x2[[1]]
}

quote_weird_name2 <- function(x) {
  x2 <- quote_weird_name0(x)
  paste0("\"", x2[[1]], "\"")
}

theme_progress_bar <- function(x, app, style) {
  make_progress_bar(x$current / x$total, style = style)
}

detect_dark_theme <- function(dark) {
  tryCatch({
    if (dark == "auto") {
      dark <- if (Sys.getenv("RSTUDIO", "0") == "1") {
        get_rstudio_theme()$dark
      } else if (is_iterm()) {
        is_iterm_dark()
      } else if (is_emacs()) {
        Sys.getenv("ESS_BACKGROUND_MODE", "light") == "dark"
      } else {
        FALSE
      }
    }
  }, error = function(e) FALSE)

  isTRUE(dark)
}

theme_code <- function(dark) {
  list()
}

tick_formatter <- function(x) {
  tt <- grepl("`", x, fixed = TRUE) + 1L
  t1 <- c("`", "`` ")[tt]
  t2 <- c("`", " ``")[tt]
  paste0(t1, x, t2)
}

tick_formatter_fun <- function(x) {
  tt <- grepl("`", x, fixed = TRUE) + 1L
  t1 <- c("`", "`` ")[tt]
  t2 <- c("()`", " ()``")[tt]
  paste0(t1, x, t2)
}

theme_code_tick <- function(dark) {
  utils::modifyList(
    theme_code(dark),
    list(transform = tick_formatter)
  )
}

theme_function <- function(dark) {
  utils::modifyList(
    theme_code(dark),
    list(transform = function(x) tick_formatter_fun(make_link(x, type = "fun")))
  )
}

theme_file <- function() {
  list(
    color = "blue",
    transform = function(x) make_link(x, type = "file"),
    fmt = quote_weird_name
  )
}

format_r_code <- function(dark) {
  function(x) {
    x <- ansi_strip(x)
    lines <- unlist(strsplit(x, "\n", fixed = TRUE))
    code_highlight(lines)
  }
}

format_code <- function(dark) {
  function(x) {
    unlist(strsplit(x, "\n", fixed = TRUE))
  }
}

theme_create <- function(theme) {
  mtheme <- theme
  mtheme[] <- lapply(mtheme, create_formatter)
  selectors <- names(theme)
  res <- data.frame(
    stringsAsFactors = FALSE,
    selector = as.character(selectors),
    parsed = I(lapply(selectors, parse_selector) %||% list()),
    style = I(mtheme %||% list()),
    cnt = rep(NA_character_, length(selectors))
  )

  rownames(res) <- NULL
  res
}

create_formatter <- function(x) {
  is_bold <- identical(x[["font-weight"]], "bold")
  is_italic <- identical(x[["font-style"]], "italic")
  is_underline <- identical(x[["text-decoration"]], "underline")
  is_color <- "color" %in% names(x)
  is_bg_color <- "background-color" %in% names(x)

  if (!is_bold && !is_italic && !is_underline && !is_color
      && !is_bg_color) return(x)

  if (is_color && is.null(x[["color"]])) {
    x[["color"]] <- "none"
  }
  if (is_bg_color && is.null(x[["background-color"]])) {
    x[["background-color"]] <- "none"
  }

  fmt <- c(
    if (is_bold) list(style_bold),
    if (is_italic) list(style_italic),
    if (is_underline) list(style_underline),
    if (is_color) make_ansi_style(x[["color"]]),
    if (is_bg_color) make_ansi_style(x[["background-color"]], bg = TRUE)
  )

  new_fmt <- do.call(combine_ansi_styles, fmt)

  if (is.null(x[["fmt"]])) {
    x[["fmt"]] <- new_fmt
  } else {
    orig_fmt <- x[["fmt"]]
    x[["fmt"]] <- function(x) orig_fmt(new_fmt(x))
  }

  x
}

merge_embedded_styles <- function(old, new) {
  # before and after is not inherited, fmt is not inherited, either
  # side margins are additive, class mappings are merged
  # rest is updated, counter is reset, prefix and postfix are merged
  old$before <- old$after <- old$fmt <- NULL
  old$transform <- NULL

  # these will be applied on the container, so we don't need them inside
  old$color <- old$`background-color` <- NULL

  top <- new$`margin-top` %||% 0L
  bottom <- new$`margin-bottom` %||% 0L
  left <- (old$`margin-left` %||% 0L) + (new$`margin-left` %||% 0L)
  right <- (old$`margin-right` %||% 0L) + (new$`margin-right` %||% 0L)

  prefix <- paste0(old$prefix, new$prefix)
  postfix <- paste0(new$postfix, old$postfix)

  map <- utils::modifyList(old$`class-map` %||% list(), new$`class-map` %||% list())

  start <- new$start %||% 1L

  mrg <- utils::modifyList(old, new)
  mrg[c("margin-top", "margin-bottom", "margin-left", "margin-right",
        "start", "class-map", "prefix", "postfix")] <-
    list(top, bottom, left, right, start, map, prefix, postfix)

  ## Formatter needs to be re-generated
  create_formatter(mrg)
}

#' Parse a CSS3-like selector
#'
#' This is the rather small subset of CSS3 that is supported:
#'
#' Selectors:
#'
#' * Type selectors, e.g. `input` selects all `<input>` elements.
#' * Class selectors, e.g. `.index` selects any element that has a class
#'   of "index".
#' * ID selector. `#toc` will match the element that has the ID `"toc"`.
#'
#' Combinators:
#'
#' * Descendant combinator, i.e. the space, that combinator selects nodes
#'   that are descendants of the first element. E.g. `div span` will match
#'   all `<span>` elements that are inside a `<div>` element.
#'
#' @param x CSS3-like selector string.
#'
#' @keywords internal

parse_selector <- function(x) {
  lapply(strsplit(x, " ", fixed = TRUE)[[1]], parse_selector_node)
}

parse_selector_node <- function(x) {

  parse_ids <- function(y) {
    r <- strsplit(y, "#", fixed = TRUE)[[1]]
    if (length(r) > 1) r[-1] <- paste0("#", r[-1])
    r
  }

  parts <- strsplit(x, ".", fixed = TRUE)[[1]]
  if (length(parts) > 1) parts[-1] <- paste0(".", parts[-1])
  parts <- unlist(lapply(parts, parse_ids))
  parts <- parts[parts != ""]

  m_cls <- grepl("^\\.", parts)
  m_ids <- grepl("^#", parts)

  list(tag = as.character(unique(parts[!m_cls & !m_ids])),
       class = str_tail(unique(parts[m_cls])),
       id = str_tail(unique(parts[m_ids])))
}

#' Match a selector node to a container
#'
#' @param node Selector node, as parsed by `parse_selector_node()`.
#' @param cnt Container node, has elements `tag`, `id`, `class`.
#'
#' The selector node matches the container, if all these hold:
#'
#' * The id of the selector is missing or unique.
#' * The tag of the selector is missing or unique.
#' * The id of the container is missing or unique.
#' * The tag of the container is unique.
#' * If the selector specifies an id, it matches the id of the container.
#' * If the selector specifies a tag, it matches the tag of the container.
#' * If the selector specifies class names, the container has all these
#'   classes.
#'
#' @keywords internal

match_selector_node <- function(node, cnt) {
  if (length(node$id) > 1 || length(cnt$id) > 1) return(FALSE)
  if (length(node$tag) > 1 || length(cnt$tag) > 1) return(FALSE)
  all(node$id %in% cnt$id) &&
    all(node$tag %in% cnt$tag) &&
    all(node$class %in% cnt$class)
}

#' Match a selector to a container stack
#'
#' @param sels A list of selector nodes.
#' @param cnts A list of container nodes.
#'
#' The last selector in the list must match the last container, so we
#' do the matching from the back. This is because we use this function
#' to calculate the style of newly encountered containers.
#'
#' @keywords internal

match_selector <- function(sels, cnts) {
  sptr <- length(sels)
  cptr <- length(cnts)

  # Last selector must match the last container
  if (sptr == 0 || sptr > cptr) return(FALSE)
  match <- match_selector_node(sels[[sptr]], cnts[[cptr]])
  if (!match) return (FALSE)

  # Plus the rest should match somehow
  sptr <- sptr - 1L
  cptr <- cptr - 1L
  while (sptr != 0L && sptr <= cptr) {
    match <- match_selector_node(sels[[sptr]], cnts[[cptr]])
    if (match) {
      sptr <- sptr - 1L
      cptr <- cptr - 1L
    } else {
      cptr <- cptr - 1L
    }
  }

  sptr == 0
}
