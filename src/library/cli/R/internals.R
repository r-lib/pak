
call_if_fun <- function(x) {
  if (is.function(x)) x() else x
}

clii__xtext <- function(app, text, .list, indent, padding, ln = TRUE, wrap = TRUE) {
  style <- app$get_current_style()
  text <- app$inline(text, .list = .list)
  exdent <- style$`text-exdent` %||% 0L

  esc <- function(x) gsub(" ", "\u00a0", x, fixed = TRUE)

  bef <- call_if_fun(style$before)
  if (!is.null(bef)) text[1] <- paste0(esc(bef), text[1])
  aft <- call_if_fun(style$after)
  if (!is.null(aft)) text[length(text)] <- paste0(text[length(text)], esc(aft))

  if (!is.null(style$fmt)) text <- style$fmt(text)

  if (wrap) {
    text <- ansi_strwrap(
      text,
      exdent = exdent,
      width = app$get_width(extra = padding)
    )
  } else {
    text <- ansi_simplify(text)
  }

  app$cat_ln(text, indent = indent, padding)
  invisible(app)
}

clii__get_width <- function(app, extra) {
  style <- app$get_current_style()
  left <- style$`margin-left` %||% 0 + style$`padding-left` %||% 0
  right <- style$`margin-right` %||% 0 + style$`padding-right` %||% 0
  console_width() - left - right - extra
}

clii__cat <- function(app, lines) {
  clii__message(lines, appendLF = FALSE, output = app$output, signal = app$signal)
}

clii__cat_ln <- function(app, lines, indent, padding) {
  if (!is.null(item <- app$state$delayed_item)) {
    app$state$delayed_item <- NULL
    return(app$item_text(item$type, NULL, item$cnt_id, .list = lines))
  }

  style <- app$get_current_style()

  ## left margin
  left <- padding + (style$`margin-left` %||% 0) + (style$`padding-left` %||% 0)
  if (length(lines) && left) lines <- paste0(strrep(" ", left), lines)

  ## indent or negative indent
  if (length(lines)) {
    if (indent < 0) {
      lines[1] <- dedent(lines[1], - indent)
    } else if (indent > 0) {
      lines[1] <- paste0(strrep(" ", indent), lines[1])
    }
  }

  ## zero out margin
  app$margin <- 0

  signal <- !identical(app$signal, FALSE)
  if (signal && length(app$status_bar)) clii__clear_status_bar(app)
  app$cat(paste0(paste0(lines, "\n"), collapse = ""))
  if (signal && length(app$status_bar)) {
    app$cat(paste0(app$status_bar[[1]]$content, "\r"))
  }
}

clii__vspace <- function(app, n) {
  if (app$margin < n) {
    sp <- strrep("\n", n - app$margin)
    signal <- !identical(app$signal, FALSE)
    if (signal && length(app$status_bar)) clii__clear_status_bar(app)
    clii__message(sp, appendLF = FALSE, output = app$output, signal = app$signal)
    app$margin <- n
    if (signal && length(app$status_bar)) {
      app$cat(paste0(app$status_bar[[1]]$content, "\r"))
    }
  }
}

get_real_output <- function(output) {
  if (! inherits(output, "connection")) {
    output <- switch(
      output,
      "auto" = cli_output_connection(),
      "message" = ,
      "stderr" = stderr(),
      "stdout" = stdout()
    )
  }
  output
}

clii__message <- function(..., domain = NA, appendLF = TRUE,
                          output = stderr(), signal = TRUE) {

  msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
  output <- get_real_output(output)

  # to avoid non-breaking spaces in files, if output is redirected
  msg <- gsub("\u00a0", " ", msg, fixed = TRUE)

  if (identical(signal, FALSE)) {
    safe_cat0(msg, file = output)

  } else {
    withRestarts(muffleMessage = function() NULL, {
      cond <- simpleMessage(msg)
      class(cond) <- c("cliMessage", class(cond))
      signalCondition(cond)
      safe_cat0(msg, file = output)
    })
  }
}

safe_cat0 <- function(x, file) {
  if (inherits(file, "rawConnection")) {
    x <- enc2utf8(x)
    writeBin(charToRaw(x), file)
  } else {
    cat(x, file = file, sep = "")
  }
}
