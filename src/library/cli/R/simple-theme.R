#' A simple CLI theme
#'
#' To use this theme, you can set it as the `cli.theme` option.
#' Note that this is in addition to the builtin theme, which is still in
#' effect.
#'
#' ```r
#' options(cli.theme = cli::simple_theme())
#' ```
#'
#' and then CLI apps started after this will use it as the default theme.
#' You can also use it temporarily, in a div element:
#'
#' ```r
#' cli_div(theme = cli::simple_theme())
#' ```
#'
#' # Showcase
#'
#' ```{asciicast simple-theme}
#' show <- cli_div(theme = cli::simple_theme())
#'
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
#'   'mtcars |>',
#'   '  group_by(cyl) |>',
#'   '  mutate(rank = min_rank(desc(mpg)))')
#'
#' cli_end(show)
#' ```
#'
#' @param dark Whether the theme should be optimized for a dark
#'   background. If `"auto"`, then cli will try to detect this.
#'   Detection usually works in recent RStudio versions, and in iTerm
#'   on macOS, but not on other platforms.
#'
#' @seealso [themes], [builtin_theme()].
#' @export

simple_theme <- function(dark = getOption("cli.theme_dark", "auto")) {
  dark <- detect_dark_theme(dark)

  list(
    h1 = list(
      "margin-top" = 1,
      "margin-bottom" = 0,
      color = "cyan",
      fmt = function(x) cli::rule(x, line_col = "cyan")
    ),

    h2 = list(
      "margin-top" = 1,
      "margin-bottom" = 0,
      color = "cyan",
      fmt = function(x)
        paste0(symbol$line, " ", x, " ", symbol$line, symbol$line)
    ),

    h3 = list(
      "margin-top" = 1,
      "margin-bottom" = 0,
      color = "cyan"
    ),

    par = list("margin-top" = 0, "margin-bottom" = 1),

    ".alert-danger" = list(
      "background-color" = "red",
      color = "white",
      before = function() paste0(symbol$cross, " ")
    ),

    ".alert-warning" = list(
      color = "orange",
      "font-weight" = "bold",
      before = paste0("!", " ")
    ),

    ".alert-success" = list(
      before = function() paste0(col_green(symbol$tick), " ")
    ),
    ".alert-info" = list(
      before = function() paste0(col_cyan(symbol$info), " ")
    ),

    ".alert-start" = list(
      before = function() paste0(symbol$arrow_right, " ")
    ),

    span.pkg = list(
      color = "blue",
      "font-weight" = "bold"
    ),
    span.version = list(color = "blue"),

    span.emph = simple_theme_emph(),
    span.strong = list("font-weight" = "bold", "font-style" = "italic"),

    span.fun = utils::modifyList(simple_theme_code(dark), list(after = "()")),
    span.fn = utils::modifyList(simple_theme_code(dark), list(after = "()")),
    span.arg = simple_theme_code(dark),
    span.kbd = utils::modifyList(
      simple_theme_code(dark),
      list(before = "<", after = ">")
    ),
    span.key = utils::modifyList(
      simple_theme_code(dark),
      list(before = "<", after = ">")
    ),
    span.file = simple_theme_file(),
    span.path = simple_theme_file(),
    span.email = simple_theme_url(),
    span.url = utils::modifyList(
      simple_theme_url(),
      list(before = "<", after = ">")
    ),
    span.var = simple_theme_code(dark),
    span.envvar = simple_theme_code(dark),

    span.timestamp = list(before = "[", after = "]", color = "grey")
  )
}

simple_theme_emph <- function() {
  list("font-style" = "italic")
}

simple_theme_url <- function() {
  list(color = "blue")
}

simple_theme_code <- function(dark) {
  if (dark) {
    list("background-color" = "#232323", color = "#f0f0f0")
  } else {
    list("background-color" = "#f8f8f8", color = "#202020")
  }
}

simple_theme_file <- function() {
  list(color = "blue")
}

simple_theme_r_code <- function(dark) {
  dark <- dark
  style <- if (dark) {
    make_ansi_style("#f0f0f0")
  } else {
    make_ansi_style("#202020")
  }
  function(x) {
    x <- ansi_strip(x)
    lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
    fmd <- code_highlight(lines)
    style(fmd)
  }
}

is_iterm <- function() {
  isatty(stdout()) && Sys.getenv("TERM_PROGRAM", "") == "iTerm.app"
}

is_iterm_dark <- function() {
  if (is.null(clienv[["is_iterm_dark"]])) {
    clienv$is_iterm_dark <-
      tryCatch(
        error = function(x) FALSE,
        {
          osa <- '
            tell application "iTerm2"
              tell current session of current window
                get background color
              end tell
            end tell
          '
          out <- suppressWarnings(system2(
            "osascript",
            c("-e", shQuote(osa)),
            stdout = TRUE,
            stderr = TRUE
          ))
          nums <- scan(text = gsub(",", "", out, fixed = TRUE), quiet = TRUE)
          mean(nums) < 20000
        }
      )
  }
  clienv[["is_iterm_dark"]]
}
