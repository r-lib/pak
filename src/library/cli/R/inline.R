
if (getRversion() >= "2.15.1") utils::globalVariables("app")

inline_generic <- function(app, x, style) {

  if (is.character(x) && any(grepl("\n", x))) {
    if (getOption("cli.warn_inline_newlines", FALSE)) {
      warning("cli replaced newlines within {. ... } with spaces")
    }
    x <- gsub_("\n", " ", x, useBytes = TRUE, fixed = TRUE)
  }

  before <- call_if_fun(style$before)
  after <- call_if_fun(style$after)
  transform <- style$transform
  if (is.function(transform)) {
    if (length(formals(transform)) == 1) {
      x <- transform(x)
    } else {
      x <- transform(x, app = app, style = style)
    }
  }
  collapse <- style$collapse
  if (is.character(collapse)) {
    x <- paste0(x, collapse = collapse[1])
  }
  if (is.function(collapse)) {
    x <- collapse(x)
  }
  xx <- paste0(before, x, after)
  fmt <- style$fmt
  if (!is.null(fmt) && is.function(fmt)) {
    if (length(formals(fmt)) == 1) {
      xx <- vcapply(xx, fmt)
    } else {
      xx <- vcapply(xx, fmt, app = app, style = style)
    }
  }
  prefix <- call_if_fun(style$prefix)
  postfix <- call_if_fun(style$postfix)

  paste0(prefix, xx, postfix)
}

inline_collapse <- function(x, style = list()) {
  sep <- style[["vec-sep"]] %||% style[["vec_sep"]] %||% ", "
  sep2 <- style[["vec-sep2"]] %||% style[["vec_sep2"]] %||% " and "
  last <- style[["vec-last"]] %||% style[["vec_last"]] %||% ", and "

  trunc <- style[["vec-trunc"]] %||% style[["vec_trunc"]] %||% 20L
  col_style <- style[["vec-trunc-style"]] %||% "both-ends"

  ansi_collapse(
    x,
    sep = sep,
    sep2 = sep2,
    last = last,
    trunc = trunc,
    style = col_style
  )
}

#' This glue transformer performs the inline styling of cli
#'
#' The two rules are the following:
#' * If `code` is a class expression (i.e. it starts with a dot),
#'   then we open a `<span>` with the right class and recurse.
#' * If `code` is not a class expression (i.e. it does not start with a
#'   dor), then we collapse vectors.
#'
#' The rest of the work is about:
#' * Making sure that the outside container's non-inherited style is _not_
#'   applied to the substitution. E.g. in
#'   ```r
#'   cli_h2("This is heading number {n}")
#'   ```
#'   The `before` and `after`, etc. style attributes should not be applied
#'   to `{n}`.
#' * While making sure that the inlide style's non-interited style is
#'   applied to brace expressions. I.e. in
#'   ```r
#'   cli_text("{.fun {x}}")
#'   ```
#'   every element of `x` should be formatted as a function name.
#' * Adding extra classes from the `class-map` to the right `<span>`.
#' * Adding extra styles from [cli_vec()] to the right `<span>`.
#'
#' A class expression is a brace expression if it is of the form:
#' ```
#' {.class {expr}}
#' ```
#' I.e. `{expr}` must be a single glue substitution which is not a class
#' expression. These are treated differently internally, because they do
#' collapsing, and the styling must be applied before collapsing. For other
#' expressions the styling is applied after collapsing. E.g.
#' ```r
#' cli_text("{.fun {1:3}}")
#' #> `1()`, `2()`, and `3()`
#'
#' cli_text("{.fun f{1}}")
#' #> `f1()`
#' ```
#' In the first case `.fun` is applied to each element of `1:3`, whereas
#' in the second case, the `{1}` (rather trivial) substitution is performed
#' first, and then `.fun` is applied to `"f1"`.
#'
#' See the rest of the comments inline (pun intended).
#'
#' @param code The text inside the `{...}` glue substitution.
#' @param envir Environment with the data to perform the styling.
#'   The actual substituted values have the form `v<x>`, where `<x>` is
#'   a number. The rest of the values are metadata. E.g. the app itself is
#'   added as `envir$app`.
#' @return The substituted and styles text, a character scalar.
#'
#' @noRd

inline_transformer <- function(code, envir) {
  app <- envir$app

  match <- regexpr(inline_regex(), code, perl = TRUE)
  has_style <- match != -1

  if (has_style) {
    # styling
    starts <- attr(match, "capture.start")
    ends <- starts + attr(match, "capture.length") - 1L
    captures <- substring(code, starts, ends)
    funname <- captures[[1]]
    text <- captures[[2]]

    id <- clii__container_start(app, "span", class = funname)
    on.exit(clii__container_end(app, id), add = TRUE)

    # If we don't have a brace expression, then we add another class-less
    # `<span>` here, because this will be the one replaced by the pure
    # substitutions (the other branch of the `if`). This ensures that
    # (non-inherited) styling will _not_ be applied before collapsing them,
    # but only to the whole non-brace expression. We don't need to end this
    # container, because the one above (`id`) will end this one as well.

    braceexp <- grepl("^[<][^.][^}]*[>]$", text) &&
      count_brace_exp(text, .open = "<", .close = ">") == 1
    if (!braceexp) {
      id2 <- clii__container_start(app, "span", class = NULL)
    }

    out <- glue(
      text,
      .envir = envir,
      .transformer = inline_transformer,
      .open = paste0("<", envir$marker),
      .close = paste0(envir$marker, ">")
    )

    # If we don't have a brace expression, then (non-inherited) styling was
    # not applied internally, and we need to apply it now. We also need to
    # end the dummy class-less `<span>` here, so we use the original styled
    # contained (`id`).

    if (!braceexp) {
      clii__container_end(app, id2)
      style <- app$get_current_style()
      out <- inline_generic(app, out, style)
    }

    out

  } else {
    # plain substitution
    expr <- parse(text = code, keep.source = FALSE)
    val <- eval(expr, envir = envir)

    # If we are inside another `<span>`, then we'll "replace" that with a
    # new one, so we can add extra classes (from `class-map`) and styles
    # (from [cli_vec()] to it. Replacing means that we look up the right
    # classes and the id, end the container, and create another one with
    # the same classes (+ from `class_map`), the same id, and potentially
    # the styles from [cli_vec()].
    #
    # If we are not inside another `<span>`, then we'll just add a
    # class-less span, so that the non-inherited styles (e.g. `before`) are
    # not used before collapsing.

    node <- utils::tail(app$doc, 1)[[1]]
    if (node$tag == "span") {
      class <- node$class
      id <- node$id
      clii__container_end(app, id = node$id)
    } else {
      class <- NULL
      id <- NULL
    }

    rcls <- class(val)
    stls <- app$get_current_style()$`class-map`
    cls <- na.omit(match(rcls, names(stls)))[1]
    if (!is.na(cls)) class <- c(class, stls[[cls]])

    vec_style <- attr(val, "cli_style")
    tid <- if (!is.null(vec_style)) {
      app$add_theme(list(span = vec_style))
    }

    id <- clii__container_start(
      app, "span", id = id,
      class = paste(class, collapse = " "), theme = tid
    )
    # We don't need to end the replacement container, that happens upstream.
    if (node$tag != "span") {
      on.exit(clii__container_end(app, id), add = TRUE)
    }

    style <- app$get_current_style()
    inline_collapse(
      inline_generic(app, val, style = style),
      style = style
    )
  }
}

clii__inline <- function(app, text, .list) {
  ## Inject that app, so we can style
  texts <- c(if (!is.null(text)) list(text), .list)
  out <- lapply(texts, function(t) {
    t$values$app <- app
    glue(
      t$str,
      .envir = t$values,
      .transformer = inline_transformer,
      .open = paste0("<", t$values$marker),
      .close = paste0(t$values$marker, ">"),
      .trim = FALSE
    )
  })
  paste(out, collapse = "")
}

inline_regex <- function() "(?s)^[.]([-[:alnum:]_]+)[[:space:]]+(.*)"

make_cmd_transformer <- function(values, .call = NULL) {
  values$marker <- random_id()
  values$qty <- NA_integer_
  values$num_subst <- 0L
  values$postprocess <- FALSE
  values$pmarkers <- list()

  # These are common because of purrr's default argument names, so we
  # hardcode them es exceptions. They are in packages
  # crossmap, crosstable, rstudio.prefs, rxode2, starter.
  # rxode2 has the other ones, and we should fix that in rxode2
  # the function calls are in the oolong packagee, need to fix this as well.
  exceptions <- c(
    ".x", ".y", ".",
    ".md", ".met", ".med", ".mul", ".muR", ".dir", ".muU",
    ".sym_flip(bool_word)", ".sym_flip(bool_topic)", ".sym_flip(bool_wsi)"
  )

  # it is not easy to do better than this, we would need to pass a call
  # down from the exported functions

  caller <- .call %||% sys.call(-1)
  function(code, envir) {
    first_char <- substr(code, 1, 1)

    # {?} pluralization
    if (first_char == "?") {
      parse_plural(code, values)

    # {.} cli style
    } else if (first_char == "." && ! code %in% exceptions) {
      m <- regexpr(inline_regex(), code, perl = TRUE)
      has_match <- m != -1
      if (!has_match) {
        throw(cli_error(
          call. = caller,
          "Invalid cli literal: {.code {{{abbrev(code, 10)}}}} starts with a dot.",
          "i" = "Interpreted literals must not start with a dot in cli >= 3.4.0.",
          "i" = paste("{.code {{}}} expressions starting with a dot are",
                      "now only used for cli styles."),
          "i" = paste("To avoid this error, put a space character after",
                      "the starting {.code {'{'}} or use parentheses:",
                      "{.code {{({abbrev(code, 10)})}}}.")
        ))
      }

      starts <- attr(m, "capture.start")
      ends <- starts + attr(m, "capture.length") - 1L
      captures <- substring(code, starts, ends)
      funname <- captures[[1]]
      text <- captures[[2]]

      out <- glue(
        text,
        .envir = envir,
        .transformer = sys.function(),
        .cli = TRUE
      )
      paste0("<", values$marker, ".", funname, " ", out, values$marker, ">")

    # {} plain substitution
    } else {
      expr <- parse(text = code, keep.source = FALSE) %??%
        cli_error(
          call. = caller,
          "Could not parse cli {.code {{}}} expression:
           {.code {abbrev(code, 20)}}."
        )
      res <- eval(expr, envir = envir) %??%
        cli_error(
          call. = caller,
          "Could not evaluate cli {.code {{}}} expression:
           {.code {abbrev(code, 20)}}."
        )

      id <- paste0("v", length(values))
      values[[id]] <- res
      values$qty <- if (length(res) == 0) 0 else res
      values$num_subst <- values$num_subst + 1L
      paste0("<", values$marker, id, values$marker, ">")
    }
  }
}

glue_cmd <- function(..., .envir, .call = sys.call(-1), .trim = TRUE) {
  str <- paste0(unlist(list(...), use.names = FALSE), collapse = "")
  values <- new.env(parent = emptyenv())
  transformer <- make_cmd_transformer(values, .call = .call)
  pstr <- glue(
    str,
    .envir = .envir,
    .transformer = transformer,
    .cli = TRUE,
    .trim = .trim
  )
  glue_delay(
    str = post_process_plurals(pstr, values),
    values = values
  )
}

glue_no_cmd <- function(...) {
  str <- paste0(unlist(list(...), use.names = FALSE), collapse = "")
  values <-new.env(parent = emptyenv())
  glue_delay(
    str = str,
    values = values
  )
}

glue_delay <- function(str, values = NULL) {
  structure(
    list(str = str, values = values),
    class = "cli_glue_delay"
  )
}
