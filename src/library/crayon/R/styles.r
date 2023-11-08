
palette_idx <- function(id) {
  ifelse(
    id < 38,
    id - (30 - 1),
  ifelse(
    id < 48,
    -(id - (40 - 1)),
  ifelse(
    id < 98,
    id - (90 - 9),
    -(id - (100 - 9))
  )))
}

palette_color <- function(x) {
  c(x, palette = palette_idx(x[[1]]))
}

## ----------------------------------------------------------------------
## Styles

codes <- list(
  reset = list(0, c(0, 22, 23, 24, 27, 28, 29, 39, 49)),
  bold = list(1, 22), # 21 isn't widely supported and 22 does the same thing
  blurred = list(2, 22),
  italic = list(3, 23),
  underline = list(4, 24),
  inverse = list(7, 27),
  hidden = list(8, 28),
  strikethrough = list(9, 29),

  black = palette_color(list(30, 39)),
  red = palette_color(list(31, 39)),
  green = palette_color(list(32, 39)),
  yellow = palette_color(list(33, 39)),
  blue = palette_color(list(34, 39)),
  magenta = palette_color(list(35, 39)),
  cyan = palette_color(list(36, 39)),
  white = palette_color(list(37, 39)),
  silver = palette_color(list(90, 39)),

  bgBlack = palette_color(list(40, 49)),
  bgRed = palette_color(list(41, 49)),
  bgGreen = palette_color(list(42, 49)),
  bgYellow = palette_color(list(43, 49)),
  bgBlue = palette_color(list(44, 49)),
  bgMagenta = palette_color(list(45, 49)),
  bgCyan = palette_color(list(46, 49)),
  bgWhite = palette_color(list(47, 49))
)

## ANSI fg color -> R color

ansi_fg_r <- c(
  "black" = "black",
  "red" = "red",
  "green" = "green",
  "yellow" = "yellow",
  "blue" = "blue",
  "magenta" = "magenta",
  "cyan" = "cyan",
  "white" = "white",
  "silver" = "grey"
)

ansi_fg_rgb <- col2rgb(ansi_fg_r)

ansi_bg_r <- c(
  "bgBlack" = "black",
  "bgRed" = "red",
  "bgGreen" = "green",
  "bgYellow" = "yellow",
  "bgBlue" = "blue",
  "bgMagenta" = "magenta",
  "bgCyan" = "cyan",
  "bgWhite" = "white"
)

ansi_bg_rgb <- col2rgb(ansi_bg_r)

# code can have length > 1, used to generate the closing tags for reset

make_chr_ansi_tag <- function(code)
  paste0('\u001b[', chr(code), 'm', collapse="")

make_chr_style <- function(code) {
  list(
    open = make_chr_ansi_tag(codes[[code]][[1]]),
    close = make_chr_ansi_tag(codes[[code]][[2]]),
    palette = if (length(codes[[code]]) >= 3) codes[[code]][[3]]
  )
}

builtin_styles <- lapply(names(codes), make_chr_style)
names(builtin_styles) <- names(codes)
