# This is from https://github.com/r-lib/rcmdcheck/blob/7ee14764c2b17ee2c2f4131a9e19d1b56a66ed0f/R/styles.R
style <- function(..., sep = "") {
  args <- list(...)
  st <- names(args)

  styles <- list(
    "ok" = cli::col_green,
    "note" = cli::make_ansi_style("orange"),
    "warn" = function(x) cli::style_bold(cli::make_ansi_style("orange")(x)),
    "err" = cli::col_red,
    "pale" = cli::make_ansi_style("darkgrey"),
    "timing" = cli::make_ansi_style("cyan")
  )

  nms <- names(args)
  x <- lapply(seq_along(args), function(i) {
    if (nzchar(nms[i])) styles[[nms[i]]](args[[i]]) else args[[i]]
  })

  paste(unlist(x), collapse = sep)
}
