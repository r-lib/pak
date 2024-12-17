
operator_tokens <- function() {
  c(
    "'-'", "'+'", "'!'", "'~'", "'?'", "':'", "'*'", "'/'", "'^'",
    "SPECIAL", "LT", "GT", "EQ", "GE", "LE", "AND", "AND2", "OR", "OR2",
    "LEFT_ASSIGN", "RIGHT_ASSIGN", "'$'", "'@'", "EQ_ASSIGN", "PIPE"
  )
}

reserved_words <- function() {
  c("FUNCTION", "'\\\\'", "IF", "ELSE",
    "REPEAT", "WHILE", "FOR", "IN", "NEXT", "BREAK")
}


#' Syntax highlight R code
#'
#' @details
#' See [code_theme_list()] for the default syntax highlighting theme and
#' how to change it.
#'
#' If `code` does not parse, then it is returned unchanged and a
#' `cli_parse_failure` condition is thrown. Note that this is not an error,
#' and the condition is ignored, unless explicitly caught.
#'
#' @param code Character vector, each element is one line of code.
#' @param code_theme Theme see [code_theme_list()].
#' @param envir Environment to look up function calls for hyperlinks.
#'   If `NULL`, then the global search path is used.
#' @return Character vector, the highlighted code.
#'
#' @family syntax highlighting
#' @importFrom utils getSrcref getParseData
#' @export
#' @examples
#' code_highlight(deparse(ls))
#' cat(code_highlight(deparse(ls)), sep = "\n")

code_highlight <- function(code, code_theme = NULL, envir = NULL) {

  code_theme <- code_theme %||% code_theme_default()

  parsed <- tryCatch(
    parse(text = code, keep.source = TRUE),
    error = function(e) e
  )

  if (inherits(parsed, "error")) {
    cnd <- structure(
      list(message = conditionMessage(parsed), code = code),
      class = c("cli_parse_failure", "condition")
    )
    signalCondition(cnd)
    return(code)
  }

  theme <- code_theme_make(code_theme)
  data <- getParseData(parsed, includeText = NA)

  hitext <- data$text

  cnv <- function(x) do.call(combine_ansi_styles, as.list(x))

  brackettheme <- lapply(theme$bracket, cnv)
  theme <- theme[names(theme) != "bracket"]
  theme <- structure(lapply(theme, cnv), names = names(theme))

  ## Reserved words if else repeat while function for in next break
  if (!is.null(theme$reserved)) {
    reserved <- data$token %in% reserved_words()
    hitext[reserved] <- theme$reserved(data$text[reserved])
  }

  ## Numeric constants, including NAs, NaN and Inf
  if (!is.null(theme$number)) {
    num_const <- data$token == "NUM_CONST"
    hitext[num_const] <- theme$number(data$text[num_const])
  }

  ## NULL
  if (!is.null(theme$null)) {
    null <- data$token == "NULL_CONST"
    hitext[null] <- theme$null(data$text[null])
  }

  ## Operators
  if (!is.null(theme$operator)) {
    operator <- data$token %in% operator_tokens()
    hitext[operator] <- theme$operator(data$text[operator])
  }

  ## Function calls
  fun_call <- data$token == "SYMBOL_FUNCTION_CALL"
  if (ansi_hyperlink_types()$help) {
    hitext[fun_call] <- pretty_fun_link(data, fun_call, envir)
  }
  if (!is.null(theme$call)) {
    hitext[fun_call] <- theme$call(hitext[fun_call])
  }

  ## Strings
  if (!is.null(theme$string)) {
    string <- data$token == "STR_CONST"
    reserved <- theme$reserved %||% function(x) x
    raw <- substr(data$text[string], 1, 1) == "r"
    hitext[string][raw] <- paste0(
      rep(reserved("r"), sum(raw)),
      theme$string(substr(data$text[string][raw], 2, nchar(data$text[string][raw])))
    )
    hitext[string][!raw] <- theme$string(data$text[string][!raw])
  }

  ## Comments
  if (!is.null(theme$comment)) {
    comment <- data$token == "COMMENT"
    hitext[comment] <- theme$comment(data$text[comment])
  }

  ## Brackets
  if (length(brackettheme)) {
    bracket <- data$token %in% bracket_tokens()
    hitext[bracket] <- color_brackets(data$text[bracket], brackettheme)
  }

  do_subst(code, data, hitext)
}

do_subst <- function(code, pdata, hitext) {

  pdata$hitext <- hitext

  ## Need to do this line by line. TODO: multiline stuff might be broken
  vapply(seq_along(code), FUN.VALUE = character(1), function(no) {
    my <- pdata[pdata$line1 == no & pdata$line2 == no,, drop = FALSE]
    replace_in_place(code[no], my$col1, my$col2, my$hitext)
  })
}

open_brackets <- function() {
  c("(", "{", "[")
}

close_brackets <- function(){
  c(")", "}", "]")
}

bracket_tokens <- function() {
  s <- c(open_brackets(), close_brackets())
  c(paste0("'", s, "'"), "LBB")
}

apply_color <- function(x, lvl, l){
  k <- (lvl - 1) %% length(l) + 1
  l[[k]](x)
}

#' Colored brackets
#'
#' Add color to brackets. Brackets will be coloured consecutively with the
#' colors provided in \code{color_seq} by scope.
#'
#' @param x a character vector of brackets consisting of a valid sequence of any
#'   of the following: \code{'[[', '[', ']', '(', ')', '{', '}'}
#' @param color_seq a list of functions that take and return a character scalar. The
#' ordering defines the sequence of color functions to apply to a given scope level.
#' Color functions are recycled when the scope level exceeds the length of \code{color_seq}
#'
#' @details Meant for coloring brackets encountered within \code{highlight}.
#'   Note that occurrences of 'orphan' brackets are not taken into account
#'   mainly due to the fact that cases such as
#'
#'   \code{foo <- function(x){ `[[`(x, 1) }}
#'
#'   will either be converted to
#'
#'   \code{foo <- function(x){ x[[1]] }}
#'
#'   before the brackets are coloured if passed in as
#'   \code{highlight(deparse(foo))} or will be identified as a
#'   'SYMBOL_FUNCTION_CALL' token instead of 'LBB' if passed in as
#'
#'   \code{highlight("foo <- function(x){ `[[`(x, 1) }")}
#'
#'   Similarly, invalid code that would lead to orphaned brackets is not taken
#'   into account as this would be caught before hand in \code{highlight}.
#'
#' @noRd

color_brackets <- function(x, color_seq = list(col_yellow, col_blue, col_cyan)) {
  stopifnot(vapply(color_seq, is.function, logical(1)))
  open <- c(open_brackets(), "[[")
  o <- character()
  lvl <- 0
  i <- 1
  while (i <= length(x)) {

    if (x[i] %in% open) {
      o[length(o) + 1] <- x[i]
      lvl <- lvl + 1
      x[i] <- apply_color(x[i], lvl, color_seq)
      i <- i + 1
      next
    }

    j <- nchar(o[length(o)])
    x[i:(i + j - 1)] <-
      apply_color(x[i:(i + j - 1)], lvl, color_seq)
    i <- i + j
    lvl <- lvl - 1
    o <- o[-length(o)]
  }
  x
}

replace_in_place <- function(str, start, end, replacement) {

  stopifnot(
    length(str) == 1,
    length(start) == length(end),
    length(end) == length(replacement)
  )

  keep <- substring(str, c(1, end + 1), c(start - 1, nchar(str)))

  pieces <- character(length(replacement) * 2 + 1)

  even <- seq_along(replacement) * 2
  odd <- c(1, even + 1)
  pieces[even] <- replacement
  pieces[odd] <- keep

  paste0(pieces, collapse = "")
}

code_theme_default <- function() {
  opt <- code_theme_opt("cli.code_theme")
  if (!is.null(opt)) return(opt)

  rs <- rstudio_detect()
  if (rs$type %in% c("rstudio_console", "rstudio_console_starting")) {
    opt <- code_theme_opt("cli.code_theme_rstudio")
    if (!is.null(opt)) return(opt)
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      return(code_theme_default_rstudio())
    }
  }

  opt <- code_theme_opt("cli.code_theme_terminal")
  if (!is.null(opt)) return(opt)
  code_theme_default_term()
}

code_theme_opt <- function(option) {
  theme <- getOption(option)
  if (is.null(theme)) return(NULL)

  code_theme_make(theme)
}

code_theme_make <- function(theme) {
  if (is.list(theme)) return(theme)
  if (is_string(theme)) {
    if (theme %in% names(rstudio_themes)) return(rstudio_themes[[theme]])
    lcs <- gsub(" ", "_", tolower(names(rstudio_themes)), fixed = TRUE)
    if (theme %in% lcs) return(rstudio_themes[[ match(theme, lcs)[1] ]])
    warning("Unknown cli code theme: `", theme, "`.")
    return(NULL)
  }
  warning("Invalid cli code theme, see documentation")
  NULL
}

code_theme_default_rstudio <- function() {
  theme <- get_rstudio_theme()$editor
  if (! theme %in% names(rstudio_themes)) {
    if (!getOption("cli.ignore_unknown_rstudio_theme", FALSE)) {
      warning(
        "cli does not know this RStudio theme: '", theme, "'.",
        "\nSet `options(cli.ignore_unknown_rstudio_theme = TRUE)` ",
        "to suppress this warning"
      )
    }
    return(code_theme_default_term())
  }
  rstudio_themes[[theme]]
}

code_theme_default_term <- function() {
  "Solarized Dark"
}

#' Syntax highlighting themes
#'
#' @description
#' `code_theme_list()` lists the built-in code themes.
#'
#' # Code themes
#' A theme is a list of character vectors, except for `bracket`, see below.
#' Each character vector must contain RGB colors (e.g. `"#a9a9a9"`),
#' and cli styles, e.g. `"bold"`. Entries in the list:
#' * `reserved`: reserved words
#' * `number`: numeric literals
#' * `null`: the `NULL` constant
#' * `operator`: operators, including assignment
#' * `call`: function calls
#' * `string`: character literals
#' * `comment`: comments
#' * `bracket`: brackets: \code{(){}[]} This is a list of character vectors,
#'   to create "rainbow" brackets. It is recycled for deeply nested lists.
#'
#' # The default code theme
#'
#' In RStudio, it matches the current theme of the IDE.
#'
#' You can use three options to customize the code theme:
#' * If `cli.code_theme` is set, it is used.
#' * Otherwise if R is running in RStudio and `cli.code_theme_rstudio` is
#'   set, then it is used.
#' * Otherwise if T is not running in RStudio and `cli.code_theme_terminal`
#'   is set, then it is used.
#'
#' You can set these options to the name of a built-in theme, or to list
#' that specifies a custom theme. See [code_theme_list()] for the list
#' of the built-in themes.
#'
#' @return Character vector of the built-in code theme names.
#'
#' @family syntax highlighting
#' @export
#' @examples
#' code_theme_list()
#' code_highlight(deparse(get), code_theme = "Solarized Dark")

code_theme_list <- function() {
  names(rstudio_themes)
}

pretty_print_function <- function(x, useSource = TRUE, code_theme = NULL, ...) {
  if (num_ansi_colors() == 1L) return(base::print.function(x, useSource))

  srcref <- getSrcref(x)
  src <- if (useSource && ! is.null(srcref)) {
    as.character(srcref)
  } else {
    deparse(x)
  }

  err <- FALSE
  hisrc <- tryCatch(
    code_highlight(src, code_theme = code_theme, envir = environment(x)),
    error = function(e) err <<- TRUE)
  if (err) return(base::print.function(x, useSource))

  ## Environment of the function
  hisrc <- c(hisrc, utils::capture.output(print(environment(x))))

  cat(hisrc, sep = "\n")
  invisible(x)
}

#' Turn on pretty-printing functions at the R console
#'
#' Defines a print method for functions, in the current session, that supports
#' syntax highlighting.
#'
#' The new print method takes priority over the built-in one. Use
#' [base::suppressMessages()] to suppress the alert message.
#'
#' @export

pretty_print_code <- function() {
  registerS3method("print", "function", pretty_print_function, asNamespace("cli"))
  cli::cli_alert_success("Registered pretty printing function method")
}

pretty_fun_link <- function(data, fun_call, envir) {
  sprt <- ansi_hyperlink_types()$help
  wch <- which(fun_call)
  txt <- data$text[wch]
  if (! sprt || length(wch) == 0) return(txt)

  scheme <- if (identical(attr(sprt, "type"), "rstudio")) {
    "ide:help"
  } else {
    "x-r-help"
  }

  pkg <- vcapply(wch, function(idx) {
    prt <- data$parent[idx]
    sgs <- which(data$parent == prt)
    # not a pkg::fun call?
    if (length(sgs) != 3 || data$token[sgs[1]] != "SYMBOL_PACKAGE" ||
        data$token[sgs[2]] != "NS_GET") {
      # note: we do not process ::: which would be NS_GET_INT
      find_function_symbol(data$text[idx], envir %||% .GlobalEnv)
    } else {
      data$text[sgs[1]]
    }
  })

  wlnk <- which(!is.na(pkg))
  txt[wlnk] <- style_hyperlink(
    text = txt[wlnk],
    url = paste0(scheme, ":", pkg[wlnk], "::", txt[wlnk])
  )

  txt
}

find_function_symbol <- function(name, envir = .GlobalEnv) {
  empty <- emptyenv()
  while (!identical(envir, empty)) {
    if (exists(name, envir = envir, inherits = FALSE, mode = "function")) {
      env_name <- environmentName(envir)
      if (grepl("package:", env_name, fixed = TRUE)) {
        env_name <- sub("^package:", "", env_name)
      }
      if (grepl("imports:", env_name, fixed = TRUE)) {
        env_name <- environmentName(environment(get(name, envir)))
      }
      if (grepl("package:", env_name, fixed = TRUE)) {
        env_name <- sub("^package:", "", env_name)
      }
      if (env_name %in% c("", "R_GlobalEnv")) {
        env_name <- NA_character_
      }
      return(env_name)
    } else {
      envir <- parent.env(envir)
    }
  }
  NA_character_
}
