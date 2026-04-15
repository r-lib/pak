`%||%` <- function(l, r) if (is.null(l)) r else l

new_class <- function(class_name, ...) {
  structure(as.environment(list(...)), class = class_name)
}

make_space <- function(len) {
  strrep(" ", len)
}

strrep <- function(x, times) {
  x <- as.character(x)
  if (length(x) == 0L) return(x)
  r <- .mapply(
    function(x, times) {
      if (is.na(x) || is.na(times)) return(NA_character_)
      if (times <= 0L) return("")
      paste0(replicate(times, x), collapse = "")
    },
    list(x = x, times = times),
    MoreArgs = list()
  )

  res <- unlist(r, use.names = FALSE)
  Encoding(res) <- Encoding(x)
  res
}

is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) return(FALSE)
  get("is_latex_output", asNamespace("knitr"))()
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

apply_style <- function(text, style, bg = FALSE) {
  if (identical(text, "")) return(text)

  if (is.function(style)) {
    style(text)
  } else if (is.character(style)) {
    make_ansi_style(style, bg = bg)(text)
  } else if (is.null(style)) {
    text
  } else {
    throw(cli_error(
      "{.arg style} must be a color name or an ANSI style function",
      "i" = "{.arg style} is {.typeof {style}}"
    ))
  }
}

vcapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, FUN.VALUE = character(1), ..., USE.NAMES = USE.NAMES)
}

viapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, FUN.VALUE = integer(1), ..., USE.NAMES = USE.NAMES)
}

vlapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, FUN.VALUE = logical(1), ..., USE.NAMES = USE.NAMES)
}

rpad <- function(x, width = NULL) {
  if (!length(x)) return(x)
  w <- nchar(x, type = "width")
  if (is.null(width)) width <- max(w)
  paste0(x, strrep(" ", pmax(width - w, 0)))
}

lpad <- function(x, width = NULL) {
  if (!length(x)) return(x)
  w <- nchar(x, type = "width")
  if (is.null(width)) width <- max(w)
  paste0(strrep(" ", pmax(width - w, 0)), x)
}

tail_na <- function(x, n = 1) {
  utils::tail(c(rep(NA, n), x), n)
}

dedent <- function(x, n = 2) {
  first_n_char <- strsplit(ansi_substr(x, 1, n), "", fixed = TRUE)[[1]]
  n_space <- cumsum(first_n_char == " ")
  d_n_space <- diff(c(0, n_space))
  first_not_space <- utils::head(c(which(d_n_space == 0), n + 1), 1)
  ansi_substr(x, first_not_space, nchar(x))
}

new_uuid <- (function() {
  cnt <- 0
  function() {
    cnt <<- cnt + 1
    paste0("cli-", clienv$pid, "-", cnt)
  }
})()

na.omit <- function(x) {
  if (is.atomic(x)) x[!is.na(x)] else x
}

last <- function(x) {
  utils::tail(x, 1)[[1]]
}

str_tail <- function(x) {
  substr(x, 2, nchar(x))
}

push <- function(l, el, name = NULL) {
  c(l, structure(list(el), names = name))
}

try_silently <- function(expr) {
  suppressWarnings(tryCatch(expr, error = function(x) x))
}

random_id <- local({
  i <- 0
  function() {
    i <<- i + 1
    paste0("FCkNXbE-", i)
  }
})

random_marker <- "ImzV8dciA4cn4POI"

str_trim <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}

last_character <- function(x) {
  substr(x, nchar(x), nchar(x))
}

first_character <- function(x) {
  substr(x, 1, 1)
}

second_character <- function(x) {
  substr(x, 2, 2)
}

is_alnum <- function(x, ok = "") {
  grepl(paste0("^[[:alnum:]/_.", ok, "]*$"), x)
}

os_type <- function() {
  .Platform$OS.type
}

leading_space <- function(x) {
  sub("^([\\s\u00a0]*).*$", "\\1", x, perl = TRUE)
}

trailing_space <- function(x) {
  sub("^.*[^\\s\u00a0]([\\s\u00a0]*)$", "\\1", x, perl = TRUE)
}

get_rstudio_theme <- function() {
  suppressWarnings(rstudioapi::getThemeInfo())
}

# ansi_strtrim might not support NAs

abbrev <- function(x, len = 10) {
  # this is better than strtrim() because it adds ...
  ansi_strtrim(x, len)
}
