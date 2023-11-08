
get_palette_color <- function(style, colors = num_ansi_colors()) {
  opt <- getOption("cli.palette")
  if (is.null(opt) || colors < 256) return(style)
  cache_palette_color(opt, style$palette, colors)
}

palette_cache <- new.env(parent = emptyenv())

cache_palette_color <- function(pal, idx, colors = num_ansi_colors()) {
  if (is_string(pal)) {
    if (! pal %in% rownames(ansi_palettes)) {
      stop("Cannot find cli ANSI palette '", pal, "'.")
    }
    pal <- ansi_palettes[pal, ]
  }

  bg <- idx < 0
  idx <- abs(idx)
  col <- pal[[idx]]

  colkey <- as.character(colors)
  key <- paste0(col, bg)
  if (key %in% names(palette_cache[[colkey]])) {
    return(palette_cache[[colkey]][[key]])
  }

  val <- ansi_style_from_r_color(
    col,
    bg = bg,
    colors,
    grey = FALSE
  )

  if (is.null(palette_cache[[colkey]])) {
    palette_cache[[colkey]] <- new.env(parent = emptyenv())
  }
  palette_cache[[colkey]][[key]] <- val

  return(val)
}

#' @details
#' `truecolor` is an integer constant for the number of 24 bit ANSI colors.
#'
#' @format `truecolor` is an integer scalar.
#'
#' @noRd
#' @rdname ansi_palettes

truecolor <- as.integer(256 ^ 3)

#' ANSI colors palettes
#'
#' If your platform supports at least 256 colors, then you can configure
#' the colors that cli uses for the eight base and the eight bright colors.
#' (I.e. the colors of [col_black()], [col_red()], and [col_br_black()],
#' [col_br_red()], etc.
#'
#' To customize the default palette, set the `cli.palette` option to the
#' name of a built-in palette (see `ansi_palettes()`), or the list of
#' 16 colors. Colors can be specified with RGB colors strings:
#' `#rrggbb` or R color names (see the output of [grDevices::colors()]).
#'
#' For example, you can put this in your R profile:
#' ```r
#' options(cli.palette = "vscode")
#' ```
#'
#' It is currently not possible to configure the background colors
#' separately, these will be always the same as the foreground colors.
#'
#' If your platform only has 256 colors, then the colors specified in the
#' palette have to be interpolated. On true color platforms they RGB
#' values are used as-is.
#'
#' `ansi_palettes` is a data frame of the built-in palettes, each row
#' is one palette.
#'
#' `ansi_palette_show()` shows the colors of an ANSI palette on the screen.
#'
#' @format `ansi_palettes` is a data frame with one row for each palette,
#'   and one column for each base ANSI color. `attr(ansi_palettes, "info")`
#'   contains a list with information about each palette.
#'
#' @noRd
#' @examples
#' ansi_palettes
#' ansi_palette_show("dichro", colors = truecolor)

ansi_palettes <- rbind(
  utils::read.table(
    "tools/ansi-palettes.txt",
    comment.char = ";",
    stringsAsFactors = FALSE
  ),
  utils::read.table(
    "tools/ansi-iterm-palettes.txt",
    comment.char = ";",
    stringsAsFactors = FALSE
  )
)

attr(ansi_palettes, "info") <-
  list(
    dichro = paste(
      "Colorblind friendly palette, from",
      "https://github.com/romainl/vim-dichromatic#dichromatic."
    ),
    vga = paste(
      "Typical colors that are used when booting PCs and leaving them in",
      "text mode, which used a 16-entry color table. The colors are",
      "different in the EGA/VGA graphic modes.",
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    winxp = paste(
      "Windows XP Console. Seen in Windows XP through Windows 8.1.",
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    vscode = paste(
      "Visual Studio Debug console, 'Dark+' theme.",
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    win10 = paste0(
      "Campbell theme, used as of Windows 10 version 1709. Also used",
      "by PowerShell 6.",
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    macos = paste0(
      "Terminal.app in macOS",
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    putty = paste0(
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    mirc = paste0(
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    xterm = paste0(
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    ubuntu = paste0(
      "For virtual terminals, from /etc/vtrgb.",
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    eclipse = paste0(
      "From https://en.wikipedia.org/wiki/ANSI_escape_code#SGR."
    ),
    iterm = "Built-in iTerm2 theme.",
    "iterm-pastel" = "Built-In iTerm2 theme.",
    "iterm-smoooooth" = "Built-In iTerm2 theme.",
    "iterm-snazzy" = "From https://github.com/sindresorhus/iterm2-snazzy.",
    "iterm-solarized" = "Built-In iTerm2 theme.",
    "iterm-tango" = "Built-In iTerm2 theme."
  )
