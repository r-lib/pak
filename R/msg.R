
green_tick <- function() crayon::green(cli::symbol$tick)

format_msg <- function(txt, .x = parent.frame()) {
  vcapply(txt, glue::glue_data, .x = .x)
}

msg_success <- function(txt, .x = parent.frame()) {
  msg <- format_msg(txt, .x = .x)
  cat(green_tick(), " ", msg, "\n", sep = "")
}

fmt_pkg <- function(x) crayon::bold(crayon::blue(x))

fmt_ver <- function(x) crayon::blue(x)
