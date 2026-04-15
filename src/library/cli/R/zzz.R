#' ANSI colored text
#'
#' cli has a number of functions to color and style text at the command
#' line. They provide a more modern interface than the crayon package.
#'
#' The `col_*` functions change the (foreground) color to the text.
#' These are the eight original ANSI colors. Note that in some terminals,
#' they might actually look differently, as terminals have their own
#' settings for how to show them. `col_none()` is the default color, this
#' is useful in a substring of a colored string.
#'
#' The `col_br_*` functions are bright versions of the eight ANSI colors.
#' Note that on some terminal configurations and themes they might be the
#' same as the non-bright colors.
#'
#' The `bg_*` functions change the background color of the text.
#' These are the eight original ANSI background colors. These, too, can
#' vary in appearance, depending on terminal settings. `bg_none()` the
#' the default background color, this is useful in a substring of a
#' background-colored string.
#'
#' The `bg_br_*` functions are the bright versions of the eight ANSI
#' background colors. Note that on some terminal configurations and themes
#' they might be the same as the non-bright colors.
#'
#' The `style_*` functions apply other styling to the text. The currently
#' supported styling functions are:
#' * `style_reset()` to remove any style, including color,
#' * `style_bold()` for boldface / strong text, although some terminals
#'   show a bright, high intensity text instead,
#' * `style_dim()` (or `style_blurred()` reduced intensity text.
#' * `style_italic()` (not widely supported).
#' * `style_underline()`,
#' * `style_inverse()`,
#' * `style_hidden()`,
#' * `style_strikethrough()` (not widely supported).
#'
#' The style functions take any number of character vectors as arguments,
#' and they concatenate them using `paste0()` before adding the style.
#'
#' Styles can also be nested, and then inner style takes precedence, see
#' examples below.
#'
#' Sometimes you want to revert back to the default text color, in the
#' middle of colored text, or you want to have a normal font in the middle
#' of italic text. You can use the `style_no_*` functions for this. Every
#' `style_*()` function has a `style_no_*()` pair, which defends its
#' argument from taking on the style. See examples below.
#'
#' @param ... Character strings, they will be pasted together with
#'   `paste0()`, before applying the style function.
#' @return An ANSI string (class `cli_ansi_string`), that contains ANSI
#'   sequences, if the current platform supports them. You can simply
#'   use `cat()` to print them to the terminal.
#'
#' @family ANSI styling
#' @name ansi-styles
#' @examples
#' col_blue("Hello ", "world!")
#' cat(col_blue("Hello ", "world!"))
#'
#' cat("... to highlight the", col_red("search term"),
#'     "in a block of text\n")
#'
#' ## Style stack properly
#' cat(col_green(
#'  "I am a green line ",
#'  col_blue(style_underline(style_bold("with a blue substring"))),
#'  " that becomes green again!"
#' ))
#'
#' error <- combine_ansi_styles("red", "bold")
#' warn <- combine_ansi_styles("magenta", "underline")
#' note <- col_cyan
#' cat(error("Error: subscript out of bounds!\n"))
#' cat(warn("Warning: shorter argument was recycled.\n"))
#' cat(note("Note: no such directory.\n"))
#'
#' # style_no_* functions, note that the color is not removed
#' style_italic(col_green(paste0(
#'   "italic before, ",
#'   style_no_italic("normal here, "),
#'   "italic after"
#' )))
#'
#' # avoiding  color for substring
#' style_italic(col_red(paste(
#'   "red before",
#'   col_none("not red between"),
#'   "red after"
#' )))
NULL

#' @export
#' @name ansi-styles
bg_black <- create_ansi_style("bg_black")
#' @export
#' @name ansi-styles
bg_blue <- create_ansi_style("bg_blue")
#' @export
#' @name ansi-styles
bg_cyan <- create_ansi_style("bg_cyan")
#' @export
#' @name ansi-styles
bg_green <- create_ansi_style("bg_green")
#' @export
#' @name ansi-styles
bg_magenta <- create_ansi_style("bg_magenta")
#' @export
#' @name ansi-styles
bg_red <- create_ansi_style("bg_red")
#' @export
#' @name ansi-styles
bg_white <- create_ansi_style("bg_white")
#' @export
#' @name ansi-styles
bg_yellow <- create_ansi_style("bg_yellow")
#' @export
#' @name ansi-styles
bg_none <- create_ansi_style("no_bg_color")

#' @export
#' @name ansi-styles
bg_br_black <- create_ansi_style("bg_br_black")
#' @export
#' @name ansi-styles
bg_br_blue <- create_ansi_style("bg_br_blue")
#' @export
#' @name ansi-styles
bg_br_cyan <- create_ansi_style("bg_br_cyan")
#' @export
#' @name ansi-styles
bg_br_green <- create_ansi_style("bg_br_green")
#' @export
#' @name ansi-styles
bg_br_magenta <- create_ansi_style("bg_br_magenta")
#' @export
#' @name ansi-styles
bg_br_red <- create_ansi_style("bg_br_red")
#' @export
#' @name ansi-styles
bg_br_white <- create_ansi_style("bg_br_white")
#' @export
#' @name ansi-styles
bg_br_yellow <- create_ansi_style("bg_br_yellow")

#' @export
#' @name ansi-styles
col_black <- create_ansi_style("black")
#' @export
#' @name ansi-styles
col_blue <- create_ansi_style("blue")
#' @export
#' @name ansi-styles
col_cyan <- create_ansi_style("cyan")
#' @export
#' @name ansi-styles
col_green <- create_ansi_style("green")
#' @export
#' @name ansi-styles
col_magenta <- create_ansi_style("magenta")
#' @export
#' @name ansi-styles
col_red <- create_ansi_style("red")
#' @export
#' @name ansi-styles
col_white <- create_ansi_style("white")
#' @export
#' @name ansi-styles
col_yellow <- create_ansi_style("yellow")
#' @export
#' @name ansi-styles
col_grey <- create_ansi_style("silver")
#' @export
#' @name ansi-styles
col_silver <- create_ansi_style("silver")
#' @export
#' @name ansi-styles
col_none <- create_ansi_style("no_color")

#' @export
#' @name ansi-styles
col_br_black <- create_ansi_style("br_black")
#' @export
#' @name ansi-styles
col_br_blue <- create_ansi_style("br_blue")
#' @export
#' @name ansi-styles
col_br_cyan <- create_ansi_style("br_cyan")
#' @export
#' @name ansi-styles
col_br_green <- create_ansi_style("br_green")
#' @export
#' @name ansi-styles
col_br_magenta <- create_ansi_style("br_magenta")
#' @export
#' @name ansi-styles
col_br_red <- create_ansi_style("br_red")
#' @export
#' @name ansi-styles
col_br_white <- create_ansi_style("br_white")
#' @export
#' @name ansi-styles
col_br_yellow <- create_ansi_style("br_yellow")

#' @export
#' @name ansi-styles
style_dim <- create_ansi_style("blurred")
#' @export
#' @name ansi-styles
style_blurred <- create_ansi_style("blurred")
#' @export
#' @name ansi-styles
style_bold <- create_ansi_style("bold")
#' @export
#' @name ansi-styles
style_hidden <- create_ansi_style("hidden")
#' @export
#' @name ansi-styles
style_inverse <- create_ansi_style("inverse")
#' @export
#' @name ansi-styles
style_italic <- create_ansi_style("italic")
#' @export
#' @name ansi-styles
style_reset <- create_ansi_style("reset")
#' @export
#' @name ansi-styles
style_strikethrough <- create_ansi_style("strikethrough")
#' @export
#' @name ansi-styles
style_underline <- create_ansi_style("underline")

#' @export
#' @name ansi-styles
style_no_bold <- create_ansi_style("no_bold")
#' @export
#' @name ansi-styles
style_no_blurred <- create_ansi_style("no_blurred")
#' @export
#' @name ansi-styles
style_no_dim <- create_ansi_style("no_blurred")
#' @export
#' @name ansi-styles
style_no_italic <- create_ansi_style("no_italic")
#' @export
#' @name ansi-styles
style_no_underline <- create_ansi_style("no_underline")
#' @export
#' @name ansi-styles
style_no_inverse <- create_ansi_style("no_inverse")
#' @export
#' @name ansi-styles
style_no_hidden <- create_ansi_style("no_hidden")
#' @export
#' @name ansi-styles
style_no_strikethrough <- create_ansi_style("no_strikethrough")
#' @export
#' @name ansi-styles
style_no_color <- create_ansi_style("no_color")
#' @export
#' @name ansi-styles
style_no_bg_color <- create_ansi_style("no_bg_color")
