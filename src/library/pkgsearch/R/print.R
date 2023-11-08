print_header <- function(result) {
  left <- "-" %+% ' "' %+% meta(result)$query %+% '" '
  pkg <- if (meta(result)$total == 1) "package" else "packages"
  right <- " " %+% as.character(meta(result)$total) %+% " " %+% pkg %+%
    " in " %+% as.character(round(meta(result)$took / 1000, 3)) %+%
    " seconds " %+% "-"

  cat(left_right(left, right, fill = "-"), sep = "\n")
}

#' @export
#' @rdname pkg_search
#' @param object Object to summarize.
#' @param ... Additional arguments, ignored by `summary()`.
#' @importFrom utils capture.output

summary.pkg_search_result <- function(object, ...) {
  print_header(object)

  if (meta(object)$total > 0) {
    pkgs <- data.frame(
      stringsAsFactors = FALSE,
      check.names = FALSE,
      " #" = meta(object)$from + seq_len(nrow(object)) - 1,
      " " = round(object$score / meta(object)$max_score * 100),
      package = object$package,
      version = object$version,
      by = object$maintainer_name,
      "  @" = time_ago(object$date, format = "terse")
    )

    w <- max(nchar(capture.output(print(pkgs, row.names = FALSE))))
    tw <- getOption("width") - w - 2
    if (tw >= 5) {
      title <- gsub(object$title, pattern = "\\s+", replacement = " ")
      pkgs$title <- ifelse(
        nchar(title) <= tw, title,
        paste0(substr(title, 1, tw - 3), "..."))
    }

    print(pkgs, row.names = FALSE, right = FALSE)
  }

  invisible(object)
}

#' @export
#' @param x Object to print.
#' @param ... Additional arguments, ignored currently.
#' @rdname pkg_search

print.pkg_search_result <- function(x, ...) {
  if (meta(x)$format == "short") {
    return(summary(x, ...))
  } else {
    print_header(x)
    for (i in seq_len(nrow(x))) cat_hit(x, i)
    invisible(x)
  }
}

#' @importFrom prettyunits time_ago

cat_hit <- function(x, no) {

  cat("\n")

  pkg <- x[no, ]

  ## Header
  ago <- time_ago(pkg$date)
  pkg_ver <- as.character(meta(x)$from + no - 1) %+% " " %+%
    pkg$package %+% " @ " %+% as.character(pkg$version)
  cat(left_right(pkg_ver, pkg$maintainer_name %+% ", " %+% ago), sep = "\n")

  cat_line(nchar(pkg_ver), "\n")

  ## Title
  ttl <- strwrap(
    paste("#", sub("\\s+", " ", pkg$title, perl = TRUE)),
    indent = 2, exdent = 4)
  cat(ttl, sep = "\n")

  ## Description
  dsc <- strwrap(
    sub("\\s+", " ", pkg$description, perl = TRUE),
    indent = 2, exdent = 2)
  cat(dsc, sep = "\n")

  ## URL(s)
  if (!is.na(pkg$url)) {
    url <- paste(" ", strsplit(pkg$url, "[,\\s]+", perl = TRUE)[[1]])
    cat(url, sep = "\n")
  }
}

cat_line <- function(length, ...) {
  cat(paste0(rep("-", length), collapse = ""))
  cat(...)
}

right_align <- function(str, width = default_width(), fill = " ") {

  nc <- nchar(str)
  no_sp <- max(width - nc, 0)
  paste0(paste(rep(fill, no_sp), collapse = ""), str)
}

left_right <- function(left, right, width = default_width(), fill = " ") {

  nc <- nchar(right)
  left_width <- width - nc
  if (left_width <= 20) {
    ## They do not fit in one line
    paste0(
      right_align(right, width = width, fill = fill), "\n",
      paste0(strwrap(left, width = width))
    )
  } else {
    ## They do fit in one line
    left <- strwrap(
      paste(left, collapse = " "),
      width = left_width - 1, simplify = FALSE)[[1]]
    right <- right_align(
      right, width = width - nchar(left[1]) - 1, fill = fill)
    left[1] <- paste(left[1], right)
    paste(left, collapse = "\n")
  }
}

default_width <- function() {
  min(getOption("width"), 121) - 1
}


## ----------------------------------------------------------------------

#' @export
#' @param object Object to summarize.
#' @param ... Additional arguments are ignored currently.
#' @rdname cran_events

summary.cran_event_list <- function(object, ...) {
  cat("CRAN events (" %+% attr(object, "mode") %+% ")\n")
  out <- paste0(
    sapply(object, "[[", "name"),
    "@",
    sapply(object, function(xx) xx$package$Version),
    ifelse(sapply(object, "[[", "event") == "released", "", "-")
  )
  print(out)
  invisible(object)
}

#' @export
#' @rdname cran_events
#' @param x Object to print.
#' @param ... Additional arguments are ignored currently.
#' @importFrom prettyunits time_ago
#' @importFrom parsedate parse_date

print.cran_event_list <- function(x, ...) {
  cat_fill("CRAN events (" %+% attr(x, "mode") %+% ")")
  when <- time_ago(format = "short", parse_date(sapply(x, "[[", "date")))
  pkgs <- data.frame(
    stringsAsFactors = FALSE, check.names = FALSE,
    "." = ifelse(sapply(x, "[[", "event") == "released", "+", "-"),
    When = when,
    Package = sapply(x, "[[", "name"),
    Version = sapply(x, function(xx) xx$package$Version),
    RTitle = gsub("\\s+", " ", sapply(x, function(xx) xx$package$Title))
  )
  
  tw <- getOption("width") - 7 - 3 -
    max(nchar("When"), max(nchar(pkgs$When))) -
    max(nchar("Package"), max(nchar(pkgs$Package))) -
    max(nchar("Version"), max(nchar(pkgs$Version)))
  pkgs$Title <- substring(pkgs$RTitle, 1, tw)
  pkgs$Title <- ifelse(pkgs$Title == pkgs$RTitle, pkgs$Title,
                       paste0(pkgs$Title, "..."))
  pkgs$RTitle <- NULL
  
  print.data.frame(pkgs, row.names = FALSE, right = FALSE)
  
  invisible(x)
}

## ----------------------------------------------------------------------
## Utilities

cat_fill <- function(text) {
  assert_that(is_string(text))
  width <- getOption("width") - nchar(text) - 1
  cat(text, paste(rep("-", width, collapse = "")), sep = "", "\n")
}
