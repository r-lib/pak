#' Auto-linking existing styles
#'
#' They keep formatting. It is not possible to use a different link text
#' with them. We could add link text support, but theming is applied to the
#' result of these tags, and it would look weird for link text. (I.e. if
#' there is link text you don't want to append `()` to the function name,
#' etc.)
#'
#' N | Goal                                        | Input                           |Links to (link text is always the verbatim content, styled)
#' --|---------------------------------------------|---------------------------------|---------------------------------------------------------------------
#' 1 | auto-link emails                            | `{.email foo@bar.com}`          | `mailto:foo@bar.com`
#' 2 | auto-link file                              | `{.file path/file}`             | `file:///abs/path/dile`
#' 3 | auto-link file with line and column numbers | `{.file /abs/path:line:col}`    | `file:///abs/path:line:col`, `params = list(line = line, col = col)`
#' 4 | auto-link function                          | `{.fun pkg::fun}`               | `x-r-help:pkg::fun`
#' 5 | mention function w/o package                | `{.fun fun}`                    | no link is created for this form
#' 6 | auto-link url                               | `{.url url}`                    | `url`
#'
#' ## New styles to create links
#'
#' These all have link text support, via the `[text](link)` markdown syntax.
#'
#' N | Goal                                        | Input                           | Link text         | Links to                 | Non-link form
#' --|---------------------------------------------|---------------------------------|-------------------|--------------------------|------------------------------------
#' 7 | link qualified function name to help        | `{.help pkg::fun}`              | `{.fun pkg::fun}` | `x-r-help:pkg::fun`      | `{.fun ?pkg::fun}`
#' 8 | link to function with link text             | `{.help [text](pkg::fun)}`      | `text`            | `x-r-help:pkg::fun`      | `text ({.fun pkg::fun})`
#' 9 | link to topic                               | `{.topic pkg::topic}`           | `pkg::topic`      | `x-r-help:pkg::topic`    | `{.code pkg::topic}`
#' 10| link to topic with link text                | `{.topic [text](pkg::topic)}`   | `text`            | `x-r-help:pkg::topic`    | `text ({.code pkg::topic})`
#' 11| link url                                    | `{.href url}`                   | `{.url url}`      | `url`                    | `{.url url}`
#' 12| link url with link text                     | `{.href [text](url)}`           | `text`            | `url`                    | `text ({.url url})`
#' 13| link running expr                           | `{.run expr}`                   | `{.code expr}`    | `x-r-run:expr`           | `{.code expr}`
#' 14| link running expr, show code                | `{.run [code](expr)}`           | `{.code code}`    | `x-r-run:expr`           | `{.code expr}`
#' 15| link to vignette                            | `{.vignette pkg::name}`         | `pkg::name`       | `x-r-vignette:pkg::name` | `{.code vignette(pkg::name)}`
#' 16| link to vignette with link text             | `{.vignette [text](pkg::name)}` | `text`            | `x-r-vignette:pkg::name` | `text ({.code vignette(pkg::name)})`
#'
#' @name cli-links
#' @noRd
NULL

make_link <- function(
  txt,
  type = c(
    "email",
    "file",
    "fun",
    "help",
    "href",
    "run",
    "topic",
    "url",
    "vignette"
  )
) {
  type <- match.arg(type)

  switch(
    type,
    email = make_link_email(txt),
    file = make_link_file(txt),
    fun = make_link_fun(txt),
    help = make_link_help(txt),
    href = make_link_href(txt),
    run = make_link_run(txt),
    topic = make_link_topic(txt),
    url = make_link_url(txt),
    vignette = make_link_vignette(txt),
    throw(cli_error(
      "Unknown hyperlink type: {.code {type}}, internal cli error"
    )) # nocov
  )
}

# -- {.email} -------------------------------------------------------------

make_link_email <- function(txt) {
  style_hyperlink(txt, paste0("mailto:", txt))
}

# -- {.file} and {.path} --------------------------------------------------

# if txt already contains a hyperlink, then we do not add another link
# this is needed because some packages, e.g. roxygen2 currently create
# links to files manually:
# https://github.com/r-lib/roxygen2/blob/3ddfd7f2e35c3a71d5705ab4f49e851cd8da306d/R/utils.R#L91

make_link_file <- function(txt) {
  ret <- txt
  linked <- grepl("\007|\033\\\\", txt)
  ret[!linked] <- vcapply(which(!linked), function(i) {
    params <- parse_file_link_params(txt[i])
    link <- construct_file_link(params)
    style_hyperlink(
      txt[i],
      link$url,
      params = link$params
    )
  })
  ret
}

parse_file_link_params <- function(txt) {
  pattern <- "^(?<path>.*?)(?::(?<line>\\d*))?(?::(?<column>\\d*))?$"
  matches <- re_match(txt, pattern)
  ret <- as.list(matches)
  ret[!nzchar(ret)] <- list(NULL)
  if (is.null(ret[["path"]])) ret[["path"]] <- ""
  ret
}

construct_file_link <- function(params) {
  fmt <- get_config_chr("hyperlink_file_url_format")

  if (is.null(fmt)) {
    return(construct_file_link_OG(params))
  }

  params$path <- sub("^file://", "", params$path)
  params$path <- path.expand(params$path)

  looks_absolute <- function(path) {
    grepl("^/", params$path) ||
      (is_windows() && grepl("^[a-zA-Z]:", params$path))
  }
  if (!looks_absolute(params$path)) {
    params$path <- file.path(getwd(), params$path)
  }
  if (!grepl("^/", params$path)) {
    params$path <- paste0("/", params$path)
  }

  res <- interpolate_parts(fmt, params)
  list(url = res)
}

# the order of operations is very intentional and important:
# column, then line, then path
# relates to how interpolate_part() works
interpolate_parts <- function(fmt, params) {
  res <- interpolate_part(fmt, "column", params$column)
  res <- interpolate_part(res, "line", params$line)
  interpolate_part(res, "path", params$path)
}

# interpolate a part, if possible
# if no placeholder for part, this is a no-op
# if placeholder exists, but no value to fill, remove placeholder (and everything after it!)
interpolate_part <- function(
  fmt,
  part = c("column", "line", "path"),
  value = NULL
) {
  part <- match.arg(part)
  re <- glue(
    "^(?<before>.*)(?<part>\\{<<<part>>>\\})(?<after>.*?)$",
    .open = "<<<",
    .close = ">>>"
  )
  m <- re_match(fmt, re)

  if (is.na(m$part) || !nzchar(m$part)) {
    return(fmt)
  }

  if (is.null(value) || !nzchar(value)) {
    return(sub("}[^}]*$", "}", m$before))
  }

  paste0(m$before, value, m$after)
}

# handle the iterm and RStudio cases, which predated the notion of configuring
# the file hyperlink format
construct_file_link_OG <- function(params) {
  params$path <- abs_path(params$path)

  if (Sys.getenv("R_CLI_HYPERLINK_STYLE") == "iterm") {
    fmt <- "{path}#{line}:{column}"
    res <- interpolate_parts(fmt, params)
    return(list(url = res))
  }

  # RStudio takes line and col via params
  loc <- if (is.null(params$line)) {
    NULL
  } else {
    list(line = params$line, col = params$column %||% 1)
  }

  list(url = params$path, params = loc)
}

abs_path <- function(x) {
  x <- path.expand(x)
  vcapply(x, abs_path1, USE.NAMES = FALSE)
}

abs_path1 <- function(x) {
  if (grepl("^file://", x)) return(x)
  if (grepl("^/", x)) return(paste0("file://", x))
  if (is_windows() && grepl("^[a-zA-Z]:", x)) return(paste0("file://", x))
  paste0("file://", file.path(getwd(), x))
}

# -- {.fun} ---------------------------------------------------------------

make_link_fun <- function(txt) {
  tolink <- grepl("::", txt, fixed = TRUE)
  linked <- grepl("\007|\033\\\\", txt)
  todo <- tolink & !linked
  if (!any(todo)) return(txt)

  sprt <- ansi_hyperlink_types()$help
  if (!sprt) {
    return(txt)
  }

  fmt <- get_hyperlink_format("help")
  # the format has a placeholder for 'topic'
  topic <- txt[todo]
  done <- style_hyperlink(text = topic, url = glue(fmt))

  txt[todo] <- done

  txt
}

# -- {.help} --------------------------------------------------------------

make_link_help <- function(txt) {
  mch <- re_match(txt, "^\\[(?<text>.*)\\]\\((?<url>.*)\\)$")
  text <- ifelse(is.na(mch$text), txt, mch$text)
  topic <- ifelse(is.na(mch$url), txt, mch$url)

  sprt <- ansi_hyperlink_types()$help
  if (!sprt) {
    topic2 <- vcapply(topic, function(x) format_inline("{.fun ?{x}}"))
    return(ifelse(text == topic, topic2, paste0(text, " (", topic2, ")")))
  }

  fmt <- get_hyperlink_format("help")
  style_hyperlink(text = text, url = glue(fmt))
}

# -- {.href} --------------------------------------------------------------

make_link_href <- function(txt) {
  mch <- re_match(txt, "^\\[(?<text>.*)\\]\\((?<url>.*)\\)$")
  text <- ifelse(is.na(mch$text), txt, mch$text)
  url <- ifelse(is.na(mch$url), txt, mch$url)
  if (ansi_has_hyperlink_support()) {
    link <- style_hyperlink(text = text, url = url)
    style <- is.na(mch$text)
    link[style] <- vcapply(
      url[style],
      function(url1) format_inline("{.url {url1}}")
    )
    link
  } else {
    url2 <- vcapply(url, function(url1) format_inline("{.url {url1}}"))
    ifelse(text == url, url2, paste0(text, " (", url2, ")"))
  }
}

# -- {.run} ---------------------------------------------------------------

make_link_run <- function(txt) {
  mch <- re_match(txt, "^\\[(?<text>.*)\\]\\((?<url>.*)\\)$")
  text <- ifelse(is.na(mch$text), txt, mch$text)
  code <- ifelse(is.na(mch$url), txt, mch$url)

  sprt <- ansi_hyperlink_types()$run
  if (!sprt) {
    return(vcapply(text, function(code1) format_inline("{.code {code1}}")))
  }

  fmt <- get_hyperlink_format("run")
  style_hyperlink(text = text, url = glue(fmt))
}

# -- {.topic} -------------------------------------------------------------

make_link_topic <- function(txt) {
  mch <- re_match(txt, "^\\[(?<text>.*)\\]\\((?<url>.*)\\)$")
  text <- ifelse(is.na(mch$text), txt, mch$text)
  topic <- ifelse(is.na(mch$url), txt, mch$url)

  sprt <- ansi_hyperlink_types()$help
  if (!sprt) {
    topic2 <- vcapply(topic, function(x) format_inline("{.code ?{x}}"))
    return(ifelse(text == topic, topic2, paste0(text, " (", topic2, ")")))
  }

  fmt <- get_hyperlink_format("help")
  style_hyperlink(text = text, url = glue(fmt))
}

# -- {.url} ---------------------------------------------------------------

make_link_url <- function(txt) {
  linked <- grepl("\007|\033\\\\", txt)
  if (all(linked)) return(txt)
  txt[!linked] <- style_hyperlink(txt[!linked], txt[!linked])
  txt
}

# -- {.vignette} ----------------------------------------------------------

make_link_vignette <- function(txt) {
  mch <- re_match(txt, "^\\[(?<text>.*)\\]\\((?<url>.*)\\)$")
  text <- ifelse(is.na(mch$text), txt, mch$text)
  vignette <- ifelse(is.na(mch$url), txt, mch$url)

  sprt <- ansi_hyperlink_types()$vignette
  if (!sprt) {
    vignette2 <- vcapply(
      vignette,
      function(x) format_inline("{.code vignette({x})}")
    )
    return(ifelse(
      text == vignette,
      vignette2,
      paste0(text, " (", vignette2, ")")
    ))
  }

  fmt <- get_hyperlink_format("vignette")
  style_hyperlink(text = text, url = glue(fmt))
}

#' Terminal Hyperlinks
#'
#' `ansi_hyperlink()` creates an ANSI hyperlink.
#'
#' @details
#' This function is currently experimental. In particular, many of the
#' `ansi_*()` functions do not support it properly.
#'
#' `ansi_has_hyperlink_support()` checks if the current `stdout()`
#' supports hyperlinks.
#'
#' See also
#' <https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda>.
#'
#' @param text Text to show. `text` and `url` are recycled to match their
#'   length, via a `paste0()` call.
#' @param url URL to link to.
#' @param params A named character vector of additional parameters, or `NULL`.
#' @return Styled `cli_ansi_string` for `style_hyperlink()`.
#'   Logical scalar for `ansi_has_hyperlink_support()`.
#'
#' @export
#' @examples
#' cat("This is an", style_hyperlink("R", "https://r-project.org"), "link.\n")

style_hyperlink <- function(text, url, params = NULL) {
  params <- if (length(params)) {
    paste(
      names(params),
      "=",
      params,
      collapse = ":"
    )
  }

  if (Sys.getenv("R_CLI_HYPERLINK_MODE") == "posix") {
    ST <- "\033\\"
  } else {
    ST <- "\u0007"
  }

  out <- if (ansi_has_hyperlink_support()) {
    paste0("\u001B]8;", params, ";", url, ST, text, "\u001B]8;;", ST)
  } else {
    text
  }

  class(out) <- c("cli_ansi_string", "ansi_string", "character")
  out
}

#' @export
#' @rdname style_hyperlink
#' @examples
#' ansi_has_hyperlink_support()

ansi_has_hyperlink_support <- function() {
  ## Hyperlinks forced?
  enabled <- getOption("cli.hyperlink", getOption("crayon.hyperlink"))
  if (!is.null(enabled)) {
    return(isTRUE(enabled))
  }

  ## forced by environment variable
  enabled <- Sys.getenv("R_CLI_HYPERLINKS", "")
  if (isTRUE(as.logical(enabled))) {
    return(TRUE)
  }

  ## If ANSI support is off, then this is off as well
  if (num_ansi_colors() == 1) return(FALSE)

  ## Are we in RStudio?
  rstudio <- rstudio_detect()
  if (rstudio$type != "not_rstudio") {
    return(rstudio$hyperlink)
  }

  ## Are we in a terminal? No?
  if (!isatty(stdout())) {
    return(FALSE)
  }

  ## Are we in a windows terminal?
  if (is_windows() && Sys.getenv("WT_SESSION") != "") {
    return(TRUE)
  }

  ## Better to avoid it in CIs
  if (
    nzchar(Sys.getenv("CI")) ||
      nzchar(Sys.getenv("TEAMCITY_VERSION"))
  ) {
    return(FALSE)
  }

  ## iTerm
  if (nzchar(TERM_PROGRAM <- Sys.getenv("TERM_PROGRAM"))) {
    version <- package_version(
      Sys.getenv("TERM_PROGRAM_VERSION"),
      strict = FALSE
    )

    if (TERM_PROGRAM == "iTerm.app") {
      if (!is.na(version) && version >= "3.1") return(TRUE)
    }
  }

  if (nzchar(VTE_VERSION <- Sys.getenv("VTE_VERSION"))) {
    # See #441 -- some apparent heterogeneity in how the version gets
    #   encoded to this env variable. Accept either form.
    if (grepl("^\\d{4}$", VTE_VERSION)) {
      VTE_VERSION <- sprintf("%2.02f", as.numeric(VTE_VERSION) / 100)
      VTE_VERSION <- package_version(list(major = "0", minor = VTE_VERSION))
    } else {
      VTE_VERSION <- package_version(VTE_VERSION, strict = FALSE)
      if (is.na(VTE_VERSION)) {
        VTE_VERSION <- package_version("0.1.0")
      }
    }
    if (VTE_VERSION >= "0.50.1") return(TRUE)
  }

  FALSE
}


#' @details
#' `ansi_hyperlink_types()` checks if current `stdout()` supports various
#' types of hyperlinks. It returns a list with entries `href`, `run`,
#' `help` and `vignettes`.
#'
#' @rdname style_hyperlink
#' @export

ansi_hyperlink_types <- function() {
  get_config <- function(x, default = NULL) {
    opt <- getOption(paste0("cli.", tolower(x)))
    if (!is.null(opt)) return(isTRUE(opt))

    env <- Sys.getenv(paste0("R_CLI_", toupper(x)), NA_character_)
    if (!is.na(env)) return(isTRUE(as.logical(env)))

    default
  }

  rs <- rstudio_detect()
  has <- ansi_has_hyperlink_support()

  # they are on by default in RStudio, but not otherwise
  run <- get_config("hyperlink_run", default = rs$hyperlink)
  hlp <- get_config("hyperlink_help", default = rs$hyperlink)
  vgn <- get_config("hyperlink_vignette", default = rs$hyperlink)

  if (!has) {
    list(
      href = FALSE,
      run = FALSE,
      help = FALSE,
      vignette = FALSE
    )
  } else if (isTRUE(rs$hyperlink)) {
    list(
      href = TRUE,
      run = structure(run, type = "rstudio"),
      help = structure(hlp, type = "rstudio"),
      vignette = structure(vgn, type = "rstudio")
    )
  } else {
    list(
      href = TRUE,
      run = structure(run, type = "standard"),
      help = structure(hlp, type = "standard"),
      vignette = structure(vgn, type = "standard")
    )
  }
}

get_hyperlink_format <- function(type = c("run", "help", "vignette")) {
  type <- match.arg(type)

  key <- glue("hyperlink_{type}_url_format")
  sprt <- ansi_hyperlink_types()[[type]]

  custom_fmt <- get_config_chr(key)
  if (is.null(custom_fmt)) {
    if (identical(attr(sprt, "type"), "rstudio")) {
      fmt_type <- "rstudio"
    } else {
      fmt_type <- "standard"
    }
  } else {
    fmt_type <- "custom"
  }

  variable <- c(run = "code", help = "topic", vignette = "vignette")
  fmt <- switch(
    fmt_type,
    custom = custom_fmt,
    rstudio = glue("ide:{type}:{{{variable[type]}}}"),
    standard = glue("x-r-{type}:{{{variable[type]}}}")
  )
  fmt
}

get_config_chr <- function(x, default = NULL) {
  opt <- getOption(paste0("cli.", tolower(x)))
  if (!is.null(opt)) {
    stopifnot(is_string(opt))
    return(opt)
  }

  env <- Sys.getenv(paste0("R_CLI_", toupper(x)), NA_character_)
  if (!is.na(env)) return(env)

  default
}
