
## ----------------------------------------------------------------------

crayon_template <- function(...) {
  my_styles <- attr(sys.function(), "_styles")
  text <- mypaste(...)
  nc <- num_ansi_colors()
  if (nc > 1) {
    for (st in rev(my_styles)) {
      if (!is.null(st$palette)) st <- get_palette_color(st, nc)
      text <- st$open %+%
        gsub_(st$close, st$open, text, fixed = TRUE, useBytes = TRUE) %+%
        st$close
    }
  }
  text
}

hash_color_regex <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$"

is_builtin_style <- function(x) {
  is_string(x) && x %in% names(builtin_styles)
}

#' @importFrom grDevices colors

is_r_color <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    FALSE
  } else {
    x %in% grDevices::colors() || grepl(hash_color_regex, x)
  }
}

is_rgb_matrix <- function(x) {
  is.matrix(x) && is.numeric(x) && (nrow(x) == 3 || nrow(x) == 4)
}

#' @importFrom grDevices col2rgb

ansi_style_from_r_color <- function(color, bg, num_colors, grey) {
  style_from_rgb(col2rgb(color), bg, num_colors, grey)
}

# multicolor depends on this name, apparently

style_from_r_color <- ansi_style_from_r_color

style_8_from_rgb <- function(rgb, bg) {
  ansi_cols <- if (bg) ansi_bg_rgb else ansi_fg_rgb
  dist <- colSums((ansi_cols - as.vector(rgb)) ^ 2 )
  builtin_name <- names(which.min(dist))[1]
  builtin_styles[[builtin_name]]
}

style_from_rgb <- function(rgb, bg, num_colors, grey) {
  if (num_colors < 256) { return(style_8_from_rgb(rgb, bg)) }
  ansi256(rgb, bg, grey)
}

#' Create an ANSI color style
#'
#' Create a style, or a style function, or both. This function
#' is intended for those who wish to use 256 ANSI colors,
#' instead of the more widely supported eight colors.
#'
#' @details
#' The crayon package comes with predefined styles (see
#' [styles()] for a list) and functions for the basic eight-color
#' ANSI standard (`red`, `blue`, etc., see \link{crayon}).
#'
#' There are no predefined styles or style functions for the 256 color
#' ANSI mode, however, because we simply did not want to create that
#' many styles and functions. Instead, `make_style()` can be
#' used to create a style (or a style function, or both).
#'
#' There are two ways to use this function: \enumerate{
#'   \item If its first argument is not named, then it returns a function
#'     that can be used to color strings.
#'   \item If its first argument is named, then it also creates a
#'     style with the given name. This style can be used in
#'     [style()]. One can still use the return value
#'     of the function, to create a style function.
#' }
#'
#' The style (the code{...} argument) can be anything of the
#' following: \itemize{
#'   \item An R color name, see [colors()].
#'   \item A 6- or 8-digit hexa color string, e.g. `#ff0000` means
#'     red. Transparency (alpha channel) values are ignored.
#'   \item A one-column matrix with three rows for the red, green
#'     and blue channels, as returned by `col2rgb` (in the base
#'     grDevices package).
#' }
#'
#' `make_style()` detects the number of colors to use
#' automatically (this can be overridden using the `colors`
#' argument). If the number of colors is less than 256 (detected or given),
#' then it falls back to the color in the ANSI eight color mode that
#' is closest to the specified (RGB or R) color.
#'
#' See the examples below.
#'
#' @param ... The style to create. See details and examples below.
#' @param bg Whether the color applies to the background.
#' @param grey Whether to specifically create a grey color.
#'   This flag is included because ANSI 256 has a finer color scale
#'   for greys than the usual 0:5 scale for R, G and B components.
#'   It is only used for RGB color specifications (either numerically
#'   or via a hexa string) and is ignored on eigth color ANSI
#'   terminals.
#' @param colors Number of colors, detected automatically
#'   by default.
#' @return A function that can be used to color strings.
#'
#' @family styles
#' @export
#' @examples
#' ## Create a style function without creating a style
#' pink <- make_style("pink")
#' bgMaroon <- make_style(rgb(0.93, 0.19, 0.65), bg = TRUE)
#' cat(bgMaroon(pink("I am pink if your terminal wants it, too.\n")))
#'
#' ## Create a new style for pink and maroon background
#' make_style(pink = "pink")
#' make_style(bgMaroon = rgb(0.93, 0.19, 0.65), bg = TRUE)
#' "pink" %in% names(styles())
#' "bgMaroon" %in% names(styles())
#' cat(style("I am pink, too!\n", "pink", bg = "bgMaroon"))

make_style <- function(..., bg = FALSE, grey = FALSE,
                       colors = num_colors()) {

  args <- list(...)
  stopifnot(length(args) == 1)
  style <- args[[1]]
  orig_style_name <- style_name <- names(args)[1]

  stopifnot(is.character(style) && length(style) == 1 ||
            is_rgb_matrix(style) && ncol(style) == 1,
            is.logical(bg) && length(bg) == 1,
            is.numeric(colors) && length(colors) == 1)

  ansi_seqs <- if (is_builtin_style(style)) {
    if (bg && substr(style, 1, 2) != "bg") {
      style <- "bg" %+% capitalize(style)
    }
    if (is.null(style_name)) style_name <- style
    builtin_styles[[style]]

  } else if (is_r_color(style)) {
    if (is.null(style_name)) style_name <- style
    ansi_style_from_r_color(style, bg, colors, grey)

  } else if (is_rgb_matrix(style)) {
    style_from_rgb(style, bg, colors, grey)

  } else {
    stop("Unknown style specification: ", style)
  }

  if (!is.null(orig_style_name)) define_style(orig_style_name, ansi_seqs)

  make_crayon(structure(list(ansi_seqs), names = style_name))
}

make_crayon <- function(ansi_seq) {
  crayon <- crayon_template
  attr(crayon, "_styles") <- ansi_seq
  class(crayon) <- "crayon"
  crayon
}

#' @include styles.r
#'
#' @usage
#' ## Simple styles
#' red(...)
#' bold(...)
#' # ...
#'
#' ## See more styling below
#'
#' @param ... Strings to style.
#' @name crayon
#
#' @details
#'
#' Crayon defines several styles, that can be combined. Each style in the list
#' has a corresponding function with the same name.
#'
#' @section Genaral styles:
#'
#' \itemize{
#'   \item reset
#'   \item bold
#'   \item blurred (usually called \sQuote{dim}, renamed to avoid name clash)
#'   \item italic (not widely supported)
#'   \item underline
#'   \item inverse
#'   \item hidden
#'   \item strikethrough (not widely supported)
#' }
#'
#' @section Text colors:
#'
#' \itemize{
#'   \item black
#'   \item red
#'   \item green
#'   \item yellow
#'   \item blue
#'   \item magenta
#'   \item cyan
#'   \item white
#'   \item silver (usually called \sQuote{gray}, renamed to avoid name clash)
#' }
#'
#' @section Background colors:
#'
#' \itemize{
#'   \item bgBlack
#'   \item bgRed
#'   \item bgGreen
#'   \item bgYellow
#'   \item bgBlue
#'   \item bgMagenta
#'   \item bgCyan
#'   \item bgWhite
#' }
#'
#' @section Styling:
#'
#' The styling functions take any number of character vectors as arguments,
#' and they concatenate and style them: \preformatted{  library(crayon)
#'   cat(blue("Hello", "world!\n"))
#' }
#'
#' Crayon defines the \code{\%+\%} string concatenation operator, to make it easy
#' to assemble stings with different styles. \preformatted{  cat("... to highlight the " \%+\% red("search term") \%+\%
#'       " in a block of text\n")
#' }
#'
#' Styles can be combined using the `$` operator: \preformatted{  cat(yellow$bgMagenta$bold('Hello world!\n'))
#' } See also [combine_styles()].
#'
#' Styles can also be nested, and then inner style takes
#' precedence: \preformatted{  cat(green(
#'     'I am a green line ' \%+\%
#'     blue$underline$bold('with a blue substring') \%+\%
#'     ' that becomes green again!\n'
#'   ))
#' }
#'
#' It is easy to define your own themes: \preformatted{  error <- red $ bold
#'   warn <- magenta $ underline
#'   note <- cyan
#'   cat(error("Error: subscript out of bounds!\n"))
#'   cat(warn("Warning: shorter argument was recycled.\n"))
#'   cat(note("Note: no such directory.\n"))
#' }
#'
#' @aliases
#'    reset bold blurred italic underline inverse hidden strikethrough
#'    black red green yellow blue magenta cyan white silver
#'    bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite
#'
#' @export reset bold blurred italic underline inverse hidden strikethrough
#' @export black red green yellow blue magenta cyan white silver
#' @export bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite
#'
#' @seealso [make_style()] for using the 256 ANSI colors.
#' @examples
#' cat(blue("Hello", "world!"))
#'
#' cat("... to highlight the " %+% red("search term") %+%
#'     " in a block of text")
#'
#' cat(yellow$bgMagenta$bold('Hello world!'))
#'
#' cat(green(
#'  'I am a green line ' %+%
#'  blue$underline$bold('with a blue substring') %+%
#'  ' that becomes green again!'
#' ))
#'
#' error <- red $ bold
#' warn <- magenta $ underline
#' note <- cyan
#' cat(error("Error: subscript out of bounds!\n"))
#' cat(warn("Warning: shorter argument was recycled.\n"))
#' cat(note("Note: no such directory.\n"))
#'
NULL

#' ANSI escape sequences of crayon styles
#'
#' You can use this function to list all availables crayon styles,
#' via `names(styles())`, or to explicitly apply an ANSI
#' escape seauence to a string.
#'
#' @return A named list. Each list element is a list of two
#'   strings, named \sQuote{open} and \sQuote{close}.
#'
#' @seealso [crayon()] for the beginning of the crayon manual.
#' @export
#' @examples
#' names(styles())
#' cat(styles()[["bold"]]$close)

styles <- function() {
  data_env$my_styles
}

data_env <- new.env(parent = emptyenv())

data_env$my_styles <- structure(list(), names = character())

sapply(names(builtin_styles), function(style) {
  data_env$my_styles[[style]] <- builtin_styles[[style]]
  assign(style, make_style(style), envir = asNamespace("crayon"))
})

define_style <- function(name, ansi_seq) {
  data_env$my_styles[[name]] <- ansi_seq
}

#' Remove a style
#'
#' @param style The name of the style to remove. No error is given
#'   for non-existing names.
#' @return Nothing.
#'
#' @family styles
#' @export
#' @examples
#' make_style(new_style = "maroon", bg = TRUE)
#' cat(style("I am maroon", "new_style"), "\n")
#' drop_style("new_style")
#' "new_style" %in% names(styles())

drop_style <- function(style) {
  data_env$my_styles[[style]] <- NULL
  invisible()
}
