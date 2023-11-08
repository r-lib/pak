make_find_root_file <- function(criterion) {
  force(criterion)
  eval(bquote(function(..., path = ".") {
    find_root_file(..., criterion = criterion, path = path)
  }))
}

make_fix_root_file <- function(criterion, path, subdir = NULL) {
  root <- find_root(criterion = criterion, path = path)
  if (!is.null(subdir)) {
    root <- file.path(root, subdir)
  }
  eval(bquote(function(...) {
    if (!missing(..1)) {
      abs <- is_absolute_path(..1)
      if (all(abs)) {
        return(path(...))
      }
      if (any(abs)) {
        stop("Combination of absolute and relative paths not supported.", call. = FALSE)
      }
    }

    path(.(root), ...)
  }))
}

#' Is a directory the project root?
#'
#' Objects of the `root_criterion` class decide if a
#' given directory is a project root.
#'
#' Construct criteria using `root_criterion` in a very general fashion
#' by specifying a function with a `path` argument, and a description.
#'
#' @param testfun `[function|list(function)]`\cr
#'   A function with one parameter that returns `TRUE`
#'   if the directory specified by this parameter is the project root,
#'   and `FALSE` otherwise. Can also be a list of such functions.
#' @param desc `[character]`\cr
#'   A textual description of the test criterion, of the same length
#'   as `testfun`.
#' @param subdir `[character]`\cr
#'   Subdirectories to start the search in, if found
#'
#' @return
#' An S3 object of class `root_criterion` wit the following members:
#'
#' @export
#'
#' @examples
#' root_criterion(function(path) file.exists(file.path(path, "somefile")), "has somefile")
#' has_file("DESCRIPTION")
#' is_r_package
#' \dontrun{
#' is_r_package$find_file
#' is_r_package$make_fix_file(".")
#' }
root_criterion <- function(testfun, desc, subdir = NULL) {
  testfun <- check_testfun(testfun)

  stopifnot(length(desc) == length(testfun))

  full_desc <- paste0(
    desc,
    if (!is.null(subdir)) {
      paste0(
        " (also look in subdirectories: ",
        paste0("`", subdir, "`", collapse = ", "),
        ")"
      )
    }
  )

  criterion <- structure(
    list(
      #' @return
      #' \describe{
      #'   \item{`testfun`}{The `testfun` argument}
      testfun = testfun,
      #'   \item{`desc`}{The `desc` argument}
      desc = full_desc,
      #'   \item{`subdir`}{The `subdir` argument}
      subdir = subdir
    ),
    class = "root_criterion"
  )

  #'   \item{`find_file`}{A function with `...` and `path` arguments
  #'     that returns a path relative to the root,
  #'     as specified by this criterion.
  #'     The optional `path` argument specifies the starting directory,
  #'     which defaults to `"."`.
  #'     The function forwards to [find_root_file()],
  #'     which passes `...` directly to `file.path()`
  #'     if the first argument is an absolute path.
  #'   }
  criterion$find_file <- make_find_root_file(criterion)
  #'   \item{`make_fix_file`}{A function with a `path` argument that
  #'      returns a function that finds paths relative to the root.  For a
  #'      criterion `cr`, the result of `cr$make_fix_file(".")(...)`
  #'      is identical to `cr$find_file(...)`. The function created by
  #'      `make_fix_file()` can be saved to a variable to be more independent
  #'      of the current working directory.
  #'   }
  #' }
  criterion$make_fix_file <-
    function(path = getwd(), subdir = NULL) {
      make_fix_root_file(criterion, path, subdir)
    }

  criterion
}

check_testfun <- function(testfun) {
  if (is.function(testfun)) {
    testfun <- list(testfun)
  }

  for (f in testfun) {
    if (!isTRUE(all.equal(names(formals(f)), "path"))) {
      stop("All functions in testfun must have exactly one argument 'path'", call. = FALSE)
    }
  }

  testfun
}
