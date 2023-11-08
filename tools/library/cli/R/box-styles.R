
box_styles <- function() {

  styles <- list(
    single = list(
      top_left     = "\u250c",
      top_right    = "\u2510",
      bottom_right = "\u2518",
      bottom_left  = "\u2514",
      vertical    = "\u2502",
      horizontal  = "\u2500"
    ),
    double = list(
      top_left = "\u2554",
      top_right = "\u2557",
      bottom_right = "\u255d",
      bottom_left = "\u255a",
      vertical = "\u2551",
      horizontal = "\u2550"
    ),
    round= list(
      top_left = "\u256d",
      top_right = "\u256e",
      bottom_right = "\u256f",
      bottom_left = "\u2570",
      vertical = "\u2502",
      horizontal = "\u2500"
    ),
    "single-double" = list(
      top_left = "\u2553",
      top_right = "\u2556",
      bottom_right = "\u255c",
      bottom_left = "\u2559",
      vertical = "\u2551",
      horizontal = "\u2500"
    ),
    "double-single" = list(
      top_left = "\u2552",
      top_right = "\u2555",
      bottom_right = "\u255b",
      bottom_left = "\u2558",
      vertical = "\u2502",
      horizontal = "\u2550"
    ),
    classic = list(
      top_left = "+",
      top_right = "+",
      bottom_right = "+",
      bottom_left = "+",
      vertical = "|",
      horizontal = "-"
    ),
    none = list(
      top_left = " ",
      top_right = " ",
      bottom_right = " ",
      bottom_left = " ",
      vertical = " ",
      horizontal = " "
    )
  )

  ## If the platform is not UTF-8, then we replace the styles that have
  ## Unicode characters, with the classic style.

  if (!is_utf8_output()) {
    for (n in setdiff(names(styles), c("classic", "none"))) {
      styles[[n]] <- styles[["classic"]]
    }
  }

  do.call(rbind, styles)
}

#' @export
#' @rdname boxx

list_border_styles <- function() {
  rownames(box_styles())
}
