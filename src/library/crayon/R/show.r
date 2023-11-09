
#' @include style-var.r
NULL

#' Show the ANSI color table on the screen
#'
#' @param colors Number of colors to show, meaningful values
#'   are 8 and 256. It is automatically set to the number of
#'   supported colors, if not specified.
#' @return The printed string, invisibly.
#'
#' @export

show_ansi_colors <- function(colors = num_colors()) {
  if (colors < 8) {
    cat("Colors are not supported")
  } else if (colors < 256) {
    cat(ansi_colors_8(), sep = "")
    invisible(ansi_colors_8)
  } else {
    cat(ansi_colors_256(), sep = "")
    invisible(ansi_colors_256)
  }
}

#' @importFrom grDevices rgb

ansi_colors_256_col <- function() {
  sapply(0:5, function(r) {
    sapply(0:5, function(g) {
      c(sapply(0:5, function(b) {
        s <- paste0("r:", r, " g:", g, " b:", b, " ")
        style(s, as = "grey", bg = rgb(r, g, b, maxColorValue = 5))
      }), "\n")
    })
  })
}

#' @importFrom grDevices grey

ansi_colors_256_grey <- function() {
  sapply(0:23, function(g) {
    s <- paste0(" grey ", format(g, width = 2), "    ")
    style(s, as = "grey",
          bg = make_style(grey(g / 23), grey = TRUE, bg = TRUE)) %+%
      (if ((g + 1) %% 6) "" else "\n")
  })
}

ansi_colors_256 <- function() {
  c(ansi_colors_256_col(), "\n", ansi_colors_256_grey())
}

ansi_colors_8 <- function () {
  multicol(sapply(seq_along(builtin_styles), function(s) {
    st <- names(builtin_styles)[s]
    styled <- st %+% ": " %+% style("foobar", as = st) %+% " "
  }))
}
