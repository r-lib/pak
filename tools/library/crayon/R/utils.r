
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

check_string <- function(x) {
  stopifnot(is_string(x))
}

mypaste <- function(..., sep = " ") {
  args <- lapply(list(...), as.character)
  len <- setdiff(sapply(args, length), 1)
  if (length(len) > 1) {
    stop("All character vectors must have the same length (or length 1)")
  }

  paste(..., sep = sep)
}

scale <- function(x, from = c(0, 255), to = c(0, 5), round = TRUE) {
  y <- (x - from[1]) /
    (from[2] - from[1]) *
    (to[2] - to[1]) +
    to[1]

  if (round) {
    round(y)
  } else {
    y
  }
}

capitalize <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

multicol <- function(x) {
  xs <- strip_style(x)
  max_len <- max(nchar(xs))
  to_add <- max_len - nchar(xs)
  x <- paste0(x, substring("            ", 1, to_add))
  screen_width <- getOption("width")
  num_cols <- trunc(screen_width / max_len)
  num_rows <- ceiling(length(x) / num_cols)
  x <- c(x, rep("", num_cols * num_rows - length(x)))
  xm <- matrix(x, ncol = num_cols, byrow = TRUE)
  apply(xm, 1, paste, collapse = "") %+% "\n"
}

re_table <- function(...) {
  lapply(gregexpr(...), function(x) {
    res <- cbind(
      start = x,
      end = x + attr(x, "match.length") - 1,
      length = attr(x, "match.length")
    )
    res <- res[res[, "start"] != -1, , drop=FALSE]
  })
}

## Create the non-matching table from the matching table

non_matching <- function(table, str, empty = FALSE) {
  mapply(table, str, SIMPLIFY = FALSE, FUN = function(t, s) {
    if (! nrow(t)) {
      cbind(start = 1, end = base::nchar(s), length = base::nchar(s))
    } else {
      start <- c(1, t[, "end"] + 1)
      end <- c(t[, "start"] - 1, base::nchar(s))
      res <- cbind(start = start, end = end, length = end - start + 1)
      if (!empty) res[ res[, "length"] != 0, , drop = FALSE ] else res
    }
  })
}

myseq <- function(from, to, by = 1) {
  stopifnot(by != 0)
  if (by > 0) {
    if (to < from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  } else {
    if (to > from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  }
}

`%:%` <- myseq

emacs_version <- function() {
  ver <- Sys.getenv("INSIDE_EMACS")
  ver <- gsub("[^0-9\\.]+", "", ver)
  if (ver == "") return(NA_integer_)
  ver <- strsplit(ver, ".", fixed = TRUE)[[1]]
  as.numeric(ver)
}

inside_emacs <- function() {
    Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") != ""
}

rstudio_with_ansi_support <- function() {
  if (Sys.getenv("RSTUDIO", "") == "") return(FALSE)

  ## This is set *before* the rstudio initialization, in 1.1 and above
  if ((cols <- Sys.getenv("RSTUDIO_CONSOLE_COLOR", "")) != "" &&
      !is.na(as.numeric(cols))) {
    return(TRUE)
  }

  ## This only works if the initialization is complete
  requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable() &&
    rstudioapi::hasFun("getConsoleHasColor")
}

rstudio_initialized <- function() {
  ## Not in RStudio, so no worries
  rs <- Sys.getenv("RSTUDIO")
  if (rs == "" || rs == "0") return(TRUE)

  ## Otherwise check
  requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
}

os_type <- function() {
  .Platform$OS.type
}

rstudio_detect <- function() {
  rstudio$detect()
}

is_count <- function(x) {
  is.numeric(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    as.integer(x) == x &&
    x >= 0
}
