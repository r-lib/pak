tojson <- local({
  map2 <- function(x, y, fn, ...) {
    mapply(fn, x, y, ..., SIMPLIFY = FALSE)
  }

  filter <- function(v, fn) {
    keep <- vapply(v, fn, logical(1))
    v[keep]
  }

  # FIXME: is this escaping the right things?
  jq <- function(x) {
    encodeString(x, quote = "\"", justify = "none")
  }

  # 1. add a "key": at the begining of each element, unless is.null(key)
  # 2. add a comma after each elelemt, except the last one
  # Each element can be a character vector, so `key` is added to the first
  # element of the character vectors, and comma to the last ones.
  comma <- function(x, opts, key = NULL) {
    len <- length(x)
    stopifnot(len >= 1)

    if (!is.null(key)) {
      nokey <- is.na(key) | key == ""
      key[nokey] <- seq_along(x)[nokey]
      x <- map2(jq(key), x, function(k, el) {
        el[1] <- paste0(k, if (opts$pretty) ": " else ":", el[1])
        el
      })
    }

    # No commas needed for scalars
    if (len == 1) {
      return(x)
    }

    x2 <- lapply(x, function(el) {
      el[length(el)] <- paste0(el[length(el)], ",")
      el
    })

    # Keep the last list element as is
    x2[[len]] <- x[[len]]
    x2
  }

  j_null <- function(x, opts) {
    "{}"
  }

  # Data frames are done row-wise.
  # Atomic columns are unboxed. Atomic NA values are omitted.
  # List columns remove the extra wrapping list.
  j_df <- function(x, opts) {
    sub <- unlist(comma(
      lapply(seq_len(nrow(x)), function(i) {
        row <- as.list(x[i, ])
        row <- filter(row, function(v) !(is.atomic(v) && is.na(v)))
        row[] <- lapply(row, function(v) {
          if (is.atomic(v)) unbox(v) else if (is.list(v)) v[[1]] else v
        })
        j_list(row, opts)
      })
    ))
    if (opts$pretty) {
      c("[", paste0("  ", sub), "]")
    } else {
      paste0(c("[", sub, "]"), collapse = "")
    }
  }

  # Returns a character vector. Named lists are dictionaries, unnnamed
  # ones are lists. Missing dictionary keys are filled in.
  # Keys do _NOT_ need to be unique.
  j_list <- function(x, opts) {
    if (length(x) == 0L) {
      if (is.null(names(x))) "[]" else "{}"
    } else if (is.null(names(x))) {
      sub <- unlist(comma(lapply(x, j, opts), opts))
      if (opts$pretty) {
        c("[", paste0("  ", sub), "]")
      } else {
        paste(c("[", sub, "]"), collapse = "")
      }
    } else {
      sub <- unlist(comma(lapply(x, j, opts), opts, names(x)))
      if (opts$pretty) {
        c("{", paste0("  ", sub), "}")
      } else {
        paste(c("{", sub, "}"), collapse = "")
      }
    }
  }

  # Atomic vectors are converted to lists, even if they have names.
  # The names are lost. Pretty formatting keeps a vector in one line
  # currently. NA is converted to null.
  j_atomic <- function(x, opts) {
    if (!typeof(x) %in% c("logical", "integer", "double", "character")) {
      stop("Cannot convert atomic ", typeof(x), " vectors to JSON.")
    }
    len <- length(x)

    if (len == 0) {
      return("[]")
    }

    unbox <- (opts$auto_unbox && len == 1) || "unbox" %in% class(x)

    if (is.character(x)) {
      x <- jq(enc2utf8(x))
    }

    if (is.logical(x)) {
      # tolower() keeps NAs, we'll sub them later
      x <- tolower(x)
    }

    if (unbox) {
      if (is.na(x) || x == "NA") "null" else paste0(x)
    } else {
      x[is.na(x) | x == "NA"] <- "null"
      sep <- if (opts$pretty) " " else ""
      paste0("[", paste(comma(x), collapse = sep), "]")
    }
  }

  j <- function(x, opts) {
    if (is.null(x)) {
      j_null(x, opts)
    } else if (is.data.frame(x)) {
      j_df(x, opts)
    } else if (is.list(x)) {
      j_list(x, opts)
    } else if (is.atomic(x)) {
      j_atomic(x, opts)
    } else {
      stop("Cannot convert type ", typeof(x), " to JSON.")
    }
  }

  write_str <- function(x, opts = NULL) {
    paste0(write_lines(x, opts), collapse = "\n")
  }

  write_file <- function(x, file, opts = NULL) {
    writeLines(write_lines(x, opts), file)
  }

  write_lines <- function(x, opts = NULL) {
    opts <- list(
      auto_unbox = opts$auto_unbox %||% FALSE,
      pretty = opts$pretty %||% FALSE
    )
    j(x, opts)
  }

  unbox <- function(x) {
    if (!is.atomic(x)) {
      stop("Can only unbox atomic scalar, not ", typeof(x), ".")
    }
    if (length(x) != 1) {
      stop("Cannot unbox vector of length ", length(x), ".")
    }
    class(x) <- c("unbox", class(x))
    x
  }

  list(
    .envir = environment(),
    write_str = write_str,
    write_file = write_file,
    write_lines = write_lines,
    unbox = unbox
  )
})
