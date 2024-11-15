
#' About cli pluralization
#'
#' @name pluralization
#' @family pluralization
#' @includeRmd man/chunks/pluralization.Rmd
NULL

make_quantity <- function(object) {
  val <- if (is.numeric(object)) {
    stopifnot(length(object) == 1)

    if (is.finite(object))
      as.integer(object)
    else
      object
  } else {
    length(object)
  }
}

#' Pluralization helper functions
#'
#' @rdname pluralization-helpers
#' @param expr For `no()` it is an expression that is printed as "no" in
#'   cli expressions, it is interpreted as a zero quantity. For `qty()`
#'   an expression that sets the pluralization quantity without printing
#'   anything. See examples below.
#'
#' @examples
#' nfile <- 0; cli_text("Found {no(nfile)} file{?s}.")
#'
#' #> Found no files.
#'
#' nfile <- 1; cli_text("Found {no(nfile)} file{?s}.")
#'
#' #> Found 1 file.
#'
#' nfile <- 2; cli_text("Found {no(nfile)} file{?s}.")
#'
#' #> Found 2 files.
#'
#' @export
#' @family pluralization

no <- function(expr) {
  stopifnot(is.numeric(expr), length(expr) == 1, !is.na(expr))
  structure(
    expr,
    class = "cli_no"
  )
}

#' @export

as.character.cli_no <- function(x, ...) {
  if (make_quantity(x) == 0) "no" else as.character(unclass(x))
}

#' @rdname pluralization-helpers
#' @export

qty <- function(expr) {
  structure(
    make_quantity(expr),
    class = "cli_noprint"
  )
}

#' @export

as.character.cli_noprint <- function(x, ...) {
  ""
}

parse_plural <- function(code, values) {
  # If we have the quantity already, then process it now.
  # Otherwise we put in a marker for it, and request post-processing.
  qty <- make_quantity(values$qty)
  if (!is.na(qty)) {
    process_plural(qty, code)
  } else {
    values$postprocess <- TRUE
    id <- random_id()
    values$pmarkers[[id]] <- code
    id
  }
}

process_plural <- function(qty, code) {
  parts <- strsplit(str_tail(code), "/", fixed = TRUE)[[1]]
  if (last_character(code) == "/") parts <- c(parts, "")
  if (length(parts) == 1) {
    if (is.finite(qty) & qty == 1) "" else parts[1]
  } else if (length(parts) == 2) {
    if (is.finite(qty) & qty == 1)
      parts[1]
    else
      parts[2]
  } else if (length(parts) == 3) {
    if (is.finite(qty) & qty == 0) {
      parts[1]
    } else if (is.finite(qty) & qty == 1) {
      parts[2]
    } else {
      parts[3]
    }
  } else {
    stop("Invalid pluralization directive: `", code, "`")
  }
}

post_process_plurals <- function(str, values) {
  if (!values$postprocess) return(str)
  if (values$num_subst == 0) {
    stop("Cannot pluralize without a quantity")
  }
  if (values$num_subst != 1) {
    stop("Multiple quantities for pluralization")
  }

  qty <- make_quantity(values$qty)
  for (i in seq_along(values$pmarkers)) {
    mark <- values$pmarkers[i]
    str <- sub(names(mark), process_plural(qty, mark[[1]]), str)
  }

  str
}

#' String templating with pluralization
#'
#' `pluralize()` is similar to [glue::glue()], with two differences:
#' * It supports cli's [pluralization] syntax, using `{?}` markers.
#' * It collapses substituted vectors into a comma separated string.
#'
#' See [pluralization] and some examples below.
#'
#' You need to install the glue package to use this function.
#'
#' @param ...,.envir,.transformer All arguments are passed to [glue::glue()].
#'
#' @export
#' @family pluralization
#' @examplesIf requireNamespace("glue", quietly = TRUE)
#' # Regular plurals
#' nfile <- 0; pluralize("Found {nfile} file{?s}.")
#' nfile <- 1; pluralize("Found {nfile} file{?s}.")
#' nfile <- 2; pluralize("Found {nfile} file{?s}.")
#'
#' # Irregular plurals
#' ndir <- 1; pluralize("Found {ndir} director{?y/ies}.")
#' ndir <- 5; pluralize("Found {ndir} director{?y/ies}.")
#'
#' # Use 'no' instead of zero
#' nfile <- 0; pluralize("Found {no(nfile)} file{?s}.")
#' nfile <- 1; pluralize("Found {no(nfile)} file{?s}.")
#' nfile <- 2; pluralize("Found {no(nfile)} file{?s}.")
#'
#' # Use the length of character vectors
#' pkgs <- "pkg1"
#' pluralize("Will remove the {pkgs} package{?s}.")
#' pkgs <- c("pkg1", "pkg2", "pkg3")
#' pluralize("Will remove the {pkgs} package{?s}.")
#'
#' pkgs <- character()
#' pluralize("Will remove {?no/the/the} {pkgs} package{?s}.")
#' pkgs <- c("pkg1", "pkg2", "pkg3")
#' pluralize("Will remove {?no/the/the} {pkgs} package{?s}.")
#'
#' # Multiple quantities
#' nfiles <- 3; ndirs <- 1
#' pluralize("Found {nfiles} file{?s} and {ndirs} director{?y/ies}")
#'
#' # Explicit quantities
#' nupd <- 3; ntotal <- 10
#' cli_text("{nupd}/{ntotal} {qty(nupd)} file{?s} {?needs/need} updates")

pluralize <- function(..., .envir = parent.frame(),
                      .transformer = glue::identity_transformer) {

  values <- new.env(parent = emptyenv())
  values$empty <- random_id()
  values$qty <- values$empty
  values$num_subst <- 0L
  values$postprocess <- FALSE
  values$pmarkers <- list()

  tf <- function(text, envir) {
    if (substr(text, 1, 1) == "?") {
      if (identical(values$qty, values$empty)) {
        values$postprocess <- TRUE
        id <- random_id()
        values$pmarkers[[id]] <- text
        return(id)
      } else {
        return(process_plural(make_quantity(values$qty), text))
      }

    } else {
      values$num_subst <- values$num_subst + 1
      qty <- .transformer(text, envir)
      values$qty <- qty
      return(inline_collapse(qty))
    }
  }

  raw <- glue::glue(..., .envir = .envir, .transformer = tf, .comment = "")
  post_process_plurals(raw, values)
}
