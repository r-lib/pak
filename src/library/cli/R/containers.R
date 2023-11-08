
add_child <- function(x, tag, ...) {
  push(x, list(tag = tag, ...))
}

clii__container_start <- function(app, tag, class = NULL,
                                  id = NULL, theme = NULL) {

  id <- id %||% new_uuid()
  if (!length(class)) class <- ""
  class <- setdiff(unique(strsplit(class, " ", fixed = TRUE)[[1]]), "")

  app$doc <- add_child(app$doc, tag, id = id, class = class,
                           theme = theme)

  ## Go over all themes, and collect the selectors that match the
  ## current element
  new_sels <- list()
  for (t in seq_along(app$themes)) {
    theme <- app$themes[[t]]
    for (i in seq_len(nrow(theme))) {
      if (match_selector(theme$parsed[[i]], app$doc)) {
        app$themes[[t]]$cnt[i] <- id
        new_sels <- utils::modifyList(new_sels, theme$style[[i]])
      }
    }
  }
  new_style <- merge_embedded_styles(last(app$styles) %||% list(), new_sels)
  app$styles <- push(app$styles, new_style, name = id)

  ## Top margin, if any
  app$vspace(new_style$`margin-top` %||% 0)

  invisible(id)
}

clii__container_end <- function(app, id) {
  debug <- is_yes(Sys.getenv("CLI_DEBUG_BAD_END", ""))

  ## Defaults to last container
  if (is.null(id) || is.na(id)) id <- last(app$doc)$id

  ## Do not remove the <body>
  if (id == "body") {
    if (debug) warning("No cli container to close")
    return(invisible(app))
  }

  ## Do we have 'id' at all?
  wh <- which(vlapply(app$doc, function(x) identical(x$id, id)))[1]
  if (is.na(wh)) {
    if (debug) warning("Can't find cli container '", id, "' to close")
    return(invisible(app))
  }

  ## ids to remove
  del_ids <- unlist(lapply(utils::tail(app$doc, - (wh - 1L)), "[[", "id"))

  ## themes to remove
  del_thm <- unlist(lapply(utils::tail(app$doc, - (wh - 1L)), "[[", "theme"))

  ## Remove the whole subtree of 'cnt'
  app$doc <- utils::head(app$doc, wh - 1L)

  ## Bottom margin
  del_from <- match(id, names(app$styles))
  bottom <- max(viapply(
    app$styles[del_from:length(app$styles)],
    function(x) as.integer(x$`margin-bottom` %||% 0L)
  ))
  app$vspace(bottom)

  ## Remove styles
  app$styles <- utils::head(app$styles, del_from - 1L)

  ## Remove claimed styles that are not used any more
  for (t in seq_along(app$themes)) {
    m <- app$themes[[t]]$cnt %in% del_ids
    app$themes[[t]]$cnt[m] <- NA_character_
  }

  ## Remove themes
  app$themes <- app$themes[setdiff(names(app$themes), del_thm)]

  invisible(app)
}

## div --------------------------------------------------------------

clii_div <- function(app, id, class, theme) {
  theme_id <- app$add_theme(theme)
  clii__container_start(app, "div", class, id, theme = theme_id)
  id
}

## Paragraph --------------------------------------------------------

clii_par <- function(app, id, class) {
  clii__container_start(app, "par", class, id)
}

## Lists ------------------------------------------------------------

clii_ul <- function(app, items, id, class, .close) {
  id <- clii__container_start(app, "ul", id = id, class = class)
  if (length(items)) { app$li(items); if (.close) app$end(id) }
  invisible(id)
}

clii_ol <- function(app, items, id, class, .close) {
  id <- clii__container_start(app, "ol", id = id, class = class)
  if (length(items)) { app$li(items); if (.close) app$end(id) }
  invisible(id)
}

clii_dl <- function(app, items, labels, id, class, .close) {
  id <- clii__container_start(app, "dl", id = id, class = class)
  if (length(items)) { app$li(items, labels); if (.close) app$end(id) }
  invisible(id)
}

clii_li <- function(app, items, labels, id, class) {
  id <- id %||% new_uuid()

  ## check the last active list container
  last <- length(app$doc)
  while (! app$doc[[last]]$tag %in% c("ul", "ol", "dl", "body")) {
    last <- last - 1L
  }

  ## if not the last container, close the ones below it
  if (app$doc[[last]]$tag != "body" &&
      last != length(app$doc)) {
    app$end(app$doc[[last + 1L]]$id)
  }

  ## if none, then create an ul container
  if (app$doc[[last]]$tag == "body") {
    cnt_id <- app$ul()
    type <- "ul"
  } else {
    cnt_id <- app$doc[[last]]$id
    type <- app$doc[[last]]$tag
  }

  if (length(items) > 0) {
    for (i in seq_along(items)) {
      id <- clii__container_start(app, "li", id = id, class = class)
      app$item_text(type, labels[[i]], cnt_id, items[[i]])
      if (i < length(items)) app$end(id)
    }
  } else {
    app$delayed_item <- list(type = type, cnt_id = cnt_id)
    id <- clii__container_start(app, "li", id = id, class = class)
  }

  invisible(id)
}

clii__item_text <- function(app, type, name, cnt_id, text, .list) {

  style <- app$get_current_style()
  cnt_style <- app$styles[[cnt_id]]

  head <- if (type == "dl") name else glue_delay(name)

  head$str <- if (type == "ul") {
    paste0(call_if_fun(style$`list-style-type`) %||% "*", " ")
  } else if (type == "ol") {
    res <- paste0(cnt_style$start %||% 1L, ". ")
    app$styles[[cnt_id]]$start <- (cnt_style$start %||% 1L) + 1L
    res
  } else if (type == "dl") {
    mrk <- text$values$marker
    text$str <- paste0("<", mrk, ".dd ", text$str, mrk, ">")
    mrk2 <- head$values$marker
    paste0("<", mrk2, ".dt ", head$str, mrk2, ">")
  }

  app$xtext(
    .list = c(list(head), list(text), .list),
    indent = - (style$`padding-left` %||% 0),
    padding = (cnt_style$`padding-left` %||% 0)
  )
}

## Close container(s) -----------------------------------------------

clii_end <- function(app, id) {
  clii__container_end(app, id)
}
