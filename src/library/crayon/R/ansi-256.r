
# nocov start
fgcodes <- c(paste0('\x1b[38;5;', 0:255, 'm'), '\x1b[39m')
bgcodes <- c(paste0('\x1b[48;5;', 0:255, 'm'), '\x1b[49m')

rgb_index <- 17:232
gray_index <- 233:256
reset_index <- 257
#nocov end

ansi256 <- function(rgb, bg = FALSE, grey = FALSE) {
  codes <- if (bg) bgcodes else fgcodes
  if (grey) {
    ## Gray
    list(
      open = codes[gray_index][scale(rgb[1], to = c(0, 23)) + 1],
      close = codes[reset_index]
    )
    
  } else {
    ## Not gray
    list(
      open = codes[ansi256_rgb_index(rgb[1L], rgb[2L], rgb[3L])],
      close = codes[reset_index]
    )
  }
}

## This is based off the algorithm in the ruby "paint" gem, as
## implemented in rainbowrite.
ansi256_rgb_index <- function(red, green, blue) {
  gray_possible <- TRUE
  sep <- 42.5
  while (gray_possible) {
    if (red < sep || green < sep || blue < sep) {
      gray <- red < sep && green < sep && blue < sep
      gray_possible <- FALSE
    }
    sep <- sep + 42.5
  }

  ## NOTE: The +1 here translates from base0 to base1 for the index
  ## that does the same.  Not ideal, but that does get the escape
  ## characters in nicely.
  if (gray) {
    232 + round((red + green + blue) / 33) + 1
  } else {
    16 + sum(floor(6 * c(red, green, blue) / 256) * c(36, 6, 1)) + 1
  }
}
