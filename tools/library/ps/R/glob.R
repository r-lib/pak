
glob <- local({

  to_regex <- function(glob) {
    restr <- new.env(parent = emptyenv(), size = 1003)
    idx <- 0L
    chr <- strsplit(glob, "", fixed = TRUE)[[1]]
    in_group <- FALSE

    for (c in chr) {
      if (c %in% c("/", "$", "^", "+", ".", "(", ")", "=", "!", "|")) {
        idx <- idx + 1L
        restr[[as.character(idx)]] <- paste0("\\", c)

      } else if (c == "?") {
        idx <- idx + 1L
        restr[[as.character(idx)]] <- "."

      } else if (c == "[" || c == "]") {
        idx <- idx + 1L
        restr[[as.character(idx)]] <- c

      } else if (c == "{") {
        idx <- idx + 1L
        restr[[as.character(idx)]] <- "("
        in_group <- TRUE

      } else if (c == "}") {
        idx <- idx + 1L
        restr[[as.character(idx)]] <- ")"
        in_group <- FALSE

      } else if (c ==",") {
        idx <- idx + 1L
        restr[[as.character(idx)]] <- if (in_group) "|" else paste0("\\", c)

      } else if (c == "*") {
        idx <- idx + 1L
        restr[[as.character(idx)]] <- ".*"

      } else {
        idx <- idx + 1L
        restr[[as.character(idx)]] <- c
      }
    }

    paste0(
      "^",
      paste(mget(as.character(seq_len(idx)), restr), collapse = ""),
      "$")
  }

  test <- function(glob, paths) {
    re <- to_regex(glob)
    grepl(re, paths)
  }

  test_any <- function(globs, paths) {
    if (!length(paths)) return(logical())
    res <- vapply(globs, to_regex, character(1))
    m <- matrix(
      as.logical(unlist(lapply(res, grepl, x = paths))),
      nrow = length(paths))
    apply(m, 1, any)
  }

  structure(
    list(
      .internal = environment(),
      to_regex = to_regex,
      test = test,
      test_any = test_any
    ),
    class = c("standalone_glob", "standalone")
  )
})
