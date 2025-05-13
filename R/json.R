# Standalone JSON parser
#
# The purpose of this file is to provide a standalone JSON parser.
# It is quite slow and bare. If you need a proper parser please use the
# jsonlite package.
#
# The canonical location of this file is in the remotes package:
# https://github.com/r-lib/remotes/blob/HEAD/R/json.R
#
# API:
# parse(text)
# parse_file(filename)
#
# NEWS:
# - 2019/05/15 First standalone version

json <- local({
  tokenize_json <- function(text) {
    text <- paste(text, collapse = "\n")

    ESCAPE <- '(\\\\[^u[:cntrl:]]|\\\\u[0-9a-fA-F]{4})'
    CHAR <- '[^[:cntrl:]"\\\\]'

    STRING <- paste0('"', CHAR, '*(', ESCAPE, CHAR, '*)*"')
    NUMBER <- "-?(0|[1-9][0-9]*)([.][0-9]*)?([eE][+-]?[0-9]*)?"
    KEYWORD <- 'null|false|true'
    SPACE <- '[[:space:]]+'

    match <- gregexpr(
      pattern = paste0(
        STRING,
        "|",
        NUMBER,
        "|",
        KEYWORD,
        "|",
        SPACE,
        "|",
        "."
      ),
      text = text,
      perl = TRUE
    )

    grep("^\\s+$", regmatches(text, match)[[1]], value = TRUE, invert = TRUE)
  }

  throw <- function(...) {
    stop("JSON: ", ..., call. = FALSE)
  }

  # Parse a JSON file
  #
  # @param filename Path to the JSON file.
  # @return R objects corresponding to the JSON file.

  parse_file <- function(filename) {
    parse(readLines(filename, warn = FALSE))
  }

  # Parse a JSON string
  #
  # @param text JSON string.
  # @return R object corresponding to the JSON string.

  parse <- function(text) {
    tokens <- tokenize_json(text)
    token <- NULL
    ptr <- 1

    read_token <- function() {
      if (ptr <= length(tokens)) {
        token <<- tokens[ptr]
        ptr <<- ptr + 1
      } else {
        token <<- 'EOF'
      }
    }

    parse_value <- function(name = "") {
      if (token == "{") {
        parse_object()
      } else if (token == "[") {
        parse_array()
      } else if (token == "EOF" || (nchar(token) == 1 && !token %in% 0:9)) {
        throw("EXPECTED value GOT ", token)
      } else {
        j2r(token)
      }
    }

    parse_object <- function() {
      res <- structure(list(), names = character())

      read_token()

      ## Invariant: we are at the beginning of an element
      while (token != "}") {
        ## "key"
        if (grepl('^".*"$', token)) {
          key <- j2r(token)
        } else {
          throw("EXPECTED string GOT ", token)
        }

        ## :
        read_token()
        if (token != ":") {
          throw("EXPECTED : GOT ", token)
        }

        ## value
        read_token()
        res[key] <- list(parse_value())

        ## } or ,
        read_token()
        if (token == "}") {
          break
        } else if (token != ",") {
          throw("EXPECTED , or } GOT ", token)
        }
        read_token()
      }

      res
    }

    parse_array <- function() {
      res <- list()

      read_token()

      ## Invariant: we are at the beginning of an element
      while (token != "]") {
        ## value
        res <- c(res, list(parse_value()))

        ## ] or ,
        read_token()
        if (token == "]") {
          break
        } else if (token != ",") {
          throw("EXPECTED , GOT ", token)
        }
        read_token()
      }

      res
    }

    read_token()
    parse_value(tokens)
  }

  j2r <- function(token) {
    if (token == "null") {
      NULL
    } else if (token == "true") {
      TRUE
    } else if (token == "false") {
      FALSE
    } else if (grepl('^".*"$', token)) {
      trimq(token)
    } else {
      as.numeric(token)
    }
  }

  trimq <- function(x) {
    sub('^"(.*)"$', "\\1", x)
  }

  structure(
    list(
      .internal = environment(),
      parse = parse,
      parse_file = parse_file
    ),
    class = c("standalone_json", "standalone")
  )
})
