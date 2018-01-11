
#' @importFrom crayon green
#' @importFrom cli symbol
green_tick <- function() green(symbol$tick)

#' @importFrom glue glue_data

format_msg <- function(txt, .x = parent.frame()) {
  vcapply(txt, glue_data, .x = .x)
}

msg_success <- function(txt, .x = parent.frame()) {
  msg <- format_msg(txt, .x = .x)
  cat(green_tick(), " ", msg, "\n", sep = "")
}

#' @importFrom crayon blue bold

fmt_pkg <- function(x) bold(blue(x))

fmt_ver <- function(x) blue(x)
