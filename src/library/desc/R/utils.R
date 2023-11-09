
`%||%` <- function(l, r) if (is.null(l)) r else l

str_trim <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x, useBytes = TRUE), useBytes = TRUE)
}

str_squish <- function(x) {
  gsub("\\s+", " ", x)
}

is_ascii <- function(x) {
  vapply(
    as.character(x),
    function(txt) all(charToRaw(txt) <= as.raw(127)),
    TRUE,
    USE.NAMES = FALSE
  )
}

## This is from tools/R/QC.R
## We do not calculate code coverage for this, as
## it is run at install time
##
## nocov start
RFC_2822_email_regexp <- (function() {

  ## Local part consists of ASCII letters and digits, the characters
  ##   ! # $ % * / ? | ^ { } ` ~ & ' + = _ -
  ## and . provided it is not leading or trailing or repeated, or must
  ## be a quoted string.
  ## Domain part consists of dot-separated elements consisting of
  ## ASCII letters, digits and hyphen.
  ## We could also check that the local and domain parts are no longer
  ## than 64 and 255 characters, respectively.
  ## See http://en.wikipedia.org/wiki/Email_address.

  ASCII_letters_and_digits <-
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  l <- sprintf("[%s%s]", ASCII_letters_and_digits, "!#$%*/?|^{}`~&'+=_-")
  d <- sprintf("[%s%s]", ASCII_letters_and_digits, "-")
  ## Be careful to arrange the hyphens to come last in the range spec.
  sprintf("(\\\".+\\\"|(%s+\\.)*%s+)@(%s+\\.)*%s+", l, l, d, d)
})()
## nocov end


is_url <- function(x) {
  grepl("^(https?|ftp)://\\S+$", str_trim(x))
}


is_url_list <- function(x) {
  xx <- parse_url_list(x)
  all(vapply(xx, is_url, TRUE))
}


parse_url_list <- function(x) {
  xx <- strsplit(x, ",", fixed = TRUE)[[1]]
  str_trim(xx)
}


all_true <- function(x) {
  all(vapply(x, identical, TRUE, TRUE))
}


flatten <- function(x) {
  if (is.list(x)) {
    x <- lapply(
      x,
      function(e) if (is.null(e)) "" else paste(e, collapse = ",")
    )
    x <- unlist(x)
  }
  x
}

ngrepl <- function(pattern, x, ...) {
  if (is.null(pattern)) pattern <- ""
  x <- flatten(x)
  grepl(pattern, x, ...)
}

check_for_package <- function(pkg, msg = paste0("Package '", pkg,
                                     "' is needed.")) {

  has <- requireNamespace(pkg, quietly = TRUE)
  if (!has) stop(msg, call. = FALSE)
  has
}

is_dir <- function(path) {
  file.info(path)$isdir
}

postprocess_trailing_ws <- function(file, notws) {
  lines <- readLines(file)

  for (n in notws) {
    lines <- sub(paste0("^", n, ": "), paste0(n, ":"), lines)
  }
  writeLines(lines, file)
}

# We both import and qualify, this is not a mistake.
# We import for work around a jetpack revdep failure.
# We qualify to make desc work in (patched) pkgload

#' @importFrom rprojroot find_root

find_description <- function(dir) {
  pkg_root <- rprojroot::find_root(rprojroot::is_r_package, dir)
  file.path(pkg_root, "DESCRIPTION")
}

mark_continuation_lines <- function(x) {
  x <- gsub("\n[ \t]*\n", "\n .\n ", x, perl = TRUE, useBytes = TRUE)
  gsub("\n \\.([^\n])", "\n  .\\1", x, perl = TRUE, useBytes = TRUE)
}

parse_full_name <- function(x) {
  given <- paste(as.person(x)$given,
                  collapse = " ")
  family <- paste(as.person(x)$family,
                   collapse = " ")

  return(list(given = given,
              family = family))
}

# It is currently not possible to deparse UTF-8 objects to UTF-8 strings
# without converting them to the local encoding. `deparse()` either converts
# the UTF-8 characters to <U+xxxx> escapes or \ooo escapes. So it is better
# if we convert them to <U+xxxx> with iconv, and then convert back the
# <U+xxxx> strings after the deparsing.
#
# We cannot do the conversion to <U+> with iconv() because it only supports
# this kind of escaping from R 4.0.x.
#
# We also need to do the conversion on UTF-8 systems, because `deparse()`
# on older R versions escapes some Unicode characters.
#
# This function only works for character vectors, obviously. Related:
# https://stat.ethz.ch/pipermail/r-devel/2022-February/081485.html

fixed_deparse1 <- function(x, ...) {
  x <- unicode_encode(x)
  out <- paste(deparse(x, width.cutoff = 500L, ...), collapse = " ")
  out <- unicode_decode(out)
  out
}

unicode_encode <- function(x) {
  x[] <- vapply(x, unicode_encode1, character(1), USE.NAMES = FALSE)
  x
}

unicode_encode1 <- function(x) {
  x <- enc2utf8(x)
  nm <- utf8ToInt(x)
  lt <- intToUtf8(nm, multiple = TRUE)
  lt[nm > 127] <- paste0("<U+", as.hexmode(nm[nm > 127]), ">")
  paste(lt, collapse = "")
}

unicode_decode <- function(x) {
  mch <- gregexpr("<U\\+([0-9a-fA-F]+)>", x, perl = TRUE)
  uni <- regmatches(x, mch)
  rep <- lapply(uni, parse_escaped_unicode)
  regmatches(x, mch) <- rep
  x <- enc2utf8(x)
  Encoding(x) <- "UTF-8"
  x
}

parse_escaped_unicode <- function(x) {
  vapply(x, parse_escaped_unicode1, character(1))
}

parse_escaped_unicode1 <- function(x) {
  x <- gsub("<U\\+([0-9a-fA-F]+)>", "'\\\\U{\\1}'", x, perl = TRUE)
  eval(parse(text = x, encoding = "UTF-8"))
}

desc_message <- function(...) {
  msg <- simpleMessage(paste0(..., "\n"), sys.call())
  class(msg) <- c("descMessage", class(msg))
  message(msg)

}

write_dcf <- function(...) {
  if (getRversion() >= "3.5.0") {
    write.dcf(..., useBytes = TRUE)
  } else {
    write.dcf(...)
  }
}
