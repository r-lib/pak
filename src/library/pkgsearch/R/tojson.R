tojson <- local({
  map2 <- function(x, y, fn, ...) {
    mapply(fn, x, y, ..., SIMPLIFY = FALSE)
  }

  jq <- function(x) {
    encodeString(x, quote = "\"", justify = "none")
  }

  comma <- function(x, key = NULL) {
    len <- length(x)
    stopifnot(len >= 1)

    if (!is.null(key)) {
      nokey <- is.na(key) | key == ""
      key[nokey] <- seq_along(x)[nokey]
      x <- map2(jq(key), x, function(k, el) {
        el[1] <- paste0(k, ": ", el[1])
        el
      })
    }

    # No commans needed for scalars
    if (len == 1) return(x)

    x2 <- lapply(x, function(el) {
      el[length(el)] <- paste0(el[length(el)], ",")
      el
    })
    x2[[len]] <- x[[len]]
    x2
  }

  j_null <- function(x) {
    "{}"
  }

  j_list <- function(x) {
    if (length(x) == 0L) {
      if (is.null(names(x))) "[]" else "{}"

    } else if (is.null(names(x))) {
      c("[", paste0("  ", unlist(comma(lapply(x, j)))), "]")

    } else {
      c("{", paste0("  ", unlist(comma(lapply(x, j), names(x)))), "}")
    }
  }

  j_atomic <- function(x) {
    if (! typeof(x) %in% c("logical", "integer", "double", "character")) {
      stop("Cannot convert atomic ", typeof(x), " vectors to JSON.")
    }
    len <- length(x)

    if (len == 0) {
      return("[]")
    }

    if (is.character(x)) {
      x <- jq(enc2utf8(x))
    }

    if (is.logical(x)) {
      x <- tolower(x)
    }

    if (len == 1L) {
      if (is.na(x) || x == "NA") "null" else paste0(x)

    } else {
      x[is.na(x) | x == "NA"] <- "null"
      paste0("[", paste(comma(x), collapse = " "), "]")
    }
}

  j <- function(x) {
    if (is.null(x)) {
      j_null(x)
    } else if (is.list(x)) {
      j_list(x)
    } else if (is.atomic(x)) {
      j_atomic(x)
    } else {
      stop("Cannot convert type ", typeof(x), " to JSON.")
    }
}

  write_str <- function(x) {
    paste0(j(x), collapse = "\n")
  }

  write_file <- function(x, file) {
    writeLines(j(x), file)
  }

  write_lines <- function(x) {
    j(x)
  }

  list(
    .envir = environment(),
    write_str = write_str,
    write_file = write_file,
    write_lines = write_lines
  )
})
