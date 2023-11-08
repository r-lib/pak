

#' Draw a banner-like box in the console
#'
#' @details
#'
#' ## Defaults
#'
#' ```{asciicast box-default}
#' boxx("Hello there!")
#' ```
#'
#' ## Change border style
#'
#' ```{asciicast box-border}
#' boxx("Hello there!", border_style = "double")
#' ```
#'
#' ## Multiple lines
#'
#' ```{asciicast box-lines}
#' boxx(c("Hello", "there!"), padding = 1)
#' ```
#'
#' ## Padding
#'
#' ```{asciicast box-padding}
#' boxx("Hello there!", padding = 1)
#' boxx("Hello there!", padding = c(1, 5, 1, 5))
#' ```
#'
#' ## Floating
#'
#' ```{asciicast box-float}
#' boxx("Hello there!", padding = 1, float = "center")
#' boxx("Hello there!", padding = 1, float = "right")
#' ```
#'
#' ## Text color
#'
#' ```{asciicast box-text-color}
#' boxx(col_cyan("Hello there!"), padding = 1, float = "center")
#' ```
#'
#' ## Background color
#'
#' ```{asciicast box-bg-color}
#' boxx("Hello there!", padding = 1, background_col = "brown")
#' boxx("Hello there!", padding = 1, background_col = bg_red)
#' ```
#'
#' ## Border color
#'
#' ```{asciicast box-border-color}
#' boxx("Hello there!", padding = 1, border_col = "green")
#' boxx("Hello there!", padding = 1, border_col = col_red)
#' ```
#'
#' ## Label alignment
#'
#' ```{asciicast box-label-align}
#' boxx(c("Hi", "there", "you!"), padding = 1, align = "left")
#' boxx(c("Hi", "there", "you!"), padding = 1, align = "center")
#' boxx(c("Hi", "there", "you!"), padding = 1, align = "right")
#' ```
#'
#' ## A very customized box
#'
#' ```{asciicast box-custom}
#' star <- symbol$star
#' label <- c(paste(star, "Hello", star), "  there!")
#' boxx(
#'   col_white(label),
#'   border_style="round",
#'   padding = 1,
#'   float = "center",
#'   border_col = "tomato3",
#'   background_col="darkolivegreen"
#' )
#' ```
#'
#' @param label Label to show, a character vector. Each element will be
#'   in a new line. You can color it using the `col_*`, `bg_*` and
#'   `style_*` functions, see [ANSI styles][ansi-styles] and the examples
#'   below.
#' @param header Text to show on top border of the box. If too long,
#'   it will be cut.
#' @param footer Text to show on the bottom border of the box. If too long,
#'   it will be cut.
#' @param border_style String that specifies the border style.
#'   `list_border_styles` lists all current styles.
#' @param padding Padding within the box. Either an integer vector of
#'   four numbers (bottom, left, top, right), or a single number `x`, which
#'   is interpreted as `c(x, 3*x, x, 3*x)`.
#' @param margin Margin around the box. Either an integer vector of four
#'   numbers (bottom, left, top, right), or a single number `x`, which is
#'   interpreted as `c(x, 3*x, x, 3*x)`.
#' @param float Whether to display the box on the `"left"`, `"center"`, or
#'   the `"right"` of the screen.
#' @param background_col Background color of the inside of the box.
#'   Either a style function (see [ANSI styles][ansi-styles]), or a color
#'   name which will be used in [make_ansi_style()] to create a
#'   *background* style (i.e. `bg = TRUE` is used).
#' @param col Color of text, and default border color. Either a style
#'   function (see [ANSI styles][ansi-styles]) or a color name that is
#'   passed to [make_ansi_style()].
#' @param border_col Color of the border. Either a style function
#'   (see [ANSI styles][ansi-styles]) or a color name that is passed to
#'   [make_ansi_style()].
#' @param align Alignment of the label within the box: `"left"`,
#'   `"center"`, or `"right"`.
#' @param width Width of the screen, defaults to [console_width()].
#'
#' @section About fonts and terminal settings:
#' The boxes might or might not look great in your terminal, depending
#' on the box style you use and the font the terminal uses. We found that
#' the Menlo font looks nice in most terminals an also in Emacs.
#'
#' RStudio currently has a line height greater than one for console output,
#' which makes the boxes ugly.
#'
#' @export

boxx <- function(label, header = "", footer = "",
                 border_style = "single", padding = 1, margin = 0,
                 float = c("left", "center", "right"),
                 col = NULL, background_col = NULL, border_col = col,
                 align = c("left", "center", "right"),
                 width = console_width()) {

  label <- apply_style(as.character(label), col)
  widest <- max(ansi_nchar(label, "width"), 0)

  stopifnot(
    is_border_style(border_style),
    is_padding_or_margin(padding),
    is_padding_or_margin(margin)
  )
  float <- match.arg(float)
  align <- match.arg(align)

  if (length(padding) == 1) {
    padding <- c(padding, padding * 3, padding, padding * 3)
  }
  if (length(margin) == 1) {
    margin <- c(margin, margin * 3, margin, margin * 3)
  }

  label <- ansi_align(label, align = align, width = widest)
  content_width <- widest + padding[2] + padding[4]

  mar_left <- if (float == "center") {
    make_space((width - content_width) / 2)
  } else if (float == "right") {
    make_space(max(width - content_width - 2, 0))
  } else {
    make_space(margin[2])
  }

  color_border <- function(x) apply_style(x, border_col)
  color_content <- function(x) apply_style(x, background_col, bg = TRUE)

  label <- c(rep("", padding[3]), label, rep("", padding[1]))

  chars <- box_styles()[border_style, ]

  pad_left <- make_space(padding[2])
  pad_right <- make_space(
    content_width - ansi_nchar(label, "width") - padding[2]
  )

  if (header != "") {
    header <- paste0(" ", ansi_strtrim(header, content_width - 2), " ")
  }
  hdw <- ansi_nchar(header, "width")
  if (footer != "") {
    footer <- paste0(" ", ansi_strtrim(footer, content_width - 2), " ")
  }
  ftw <- ansi_nchar(footer, "width")

  hdline <- paste0(header, strrep(chars$horizontal, content_width - hdw))
  top <- color_border(paste0(
    strrep("\n", margin[3]),
    mar_left, chars$top_left, hdline, chars$top_right
  ))
  ftline <- paste0(strrep(chars$horizontal, content_width - ftw), footer)
  bottom <- color_border(paste0(
    mar_left, chars$bottom_left, ftline, chars$bottom_right,
    strrep("\n", margin[1])
  ))
  side <- color_border(chars$vertical)

  middle <- paste0(mar_left, side,
                   color_content(paste0(pad_left, label, pad_right)), side)

  box <- paste0(top, "\n", paste0(middle, collapse = "\n"), "\n", bottom)

  class(box) <- unique(c("cli_boxx", "boxx", class(box), "character"))
  box
}

methods::setOldClass(c("cli_boxx", "character"))

#' @export

print.cli_boxx <- function(x, ..., sep = "\n") {
  cat(x, ..., sep = sep)
  invisible(x)
}
