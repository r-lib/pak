format_items <- function(x) {
  paste0(
    cli::ansi_collapse(backtick(x), sep = ", ", last = " and ")
  )
}
