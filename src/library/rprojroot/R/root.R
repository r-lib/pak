#' @rdname root_criterion
#' @param x `[object]`\cr
#'   An object.
#' @export
is_root_criterion <- function(x) {
  inherits(x, "root_criterion")
}

#' @rdname root_criterion
#' @export
as_root_criterion <- function(x) UseMethod("as_root_criterion", x)

#' @details
#' The `as_root_criterion()` function accepts objects of class
#' `root_criterion`, and character values; the latter will be
#' converted to criteria using `has_file`.
#'
#' @rdname root_criterion
#' @export
as_root_criterion.character <- function(x) {
  has_file(x)
}

#' @rdname root_criterion
#' @export
as_root_criterion.root_criterion <- identity

#' @export
as_root_criterion.default <- function(x) {
  stop("Cannot coerce ", x, " to type root_criterion.", call. = FALSE)
}

#' @export
format.root_criterion <- function(x, ...) {
  if (length(x$desc) > 1) {
    c("Root criterion: one of", paste0("- ", x$desc))
  } else {
    paste0("Root criterion: ", x$desc)
  }
}

#' @export
print.root_criterion <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
#' @rdname root_criterion
#' @details Root criteria can be combined with the `|` operator. The result is a
#'   composite root criterion that requires either of the original criteria to
#'   match.
#' @param y `[object]`\cr
#'   An object.
`|.root_criterion` <- function(x, y) {
  stopifnot(is_root_criterion(y))

  root_criterion(
    c(x$testfun, y$testfun),
    c(x$desc, y$desc)
  )
}

#' Find the root of a directory hierarchy
#'
#' A \emph{root} is defined as a directory that contains a regular file
#' whose name matches a given pattern and which optionally contains a given text.
#' The search for a root starts at a given directory (the working directory
#' by default), and proceeds up the directory hierarchy.
#'
#' Starting from the working directory, the `find_root()` function searches
#' for the root.
#' If a root is found, the `...` arguments are used to construct a path;
#' thus, if no extra arguments are given, the root is returned.
#' If no root is found, an error is thrown.
#'
#' @inheritParams find_root_file
#' @return The normalized path of the root as specified by the search criterion.
#'   Throws an error if no root is found
#'
#' @examples
#' \dontrun{
#' find_root(glob2rx("DESCRIPTION"), "^Package: ")
#' }
#'
#' @seealso [utils::glob2rx()] [file.path()]
#'
#' @export
find_root <- function(criterion, path = ".") {
  criterion <- as_root_criterion(criterion)

  start_path <- get_start_path(path, criterion$subdir)
  path <- start_path

  for (i in seq_len(.MAX_DEPTH)) {
    for (f in criterion$testfun) {
      if (f(path)) {
        return(path)
      }
    }

    if (is_root(path)) {
      stop("No root directory found in ", start_path, " or its parent directories. ",
        paste(format(criterion), collapse = "\n"),
        call. = FALSE
      )
    }

    path <- dirname(path)
  }

  stop("Maximum search of ", .MAX_DEPTH, " exceeded. Last path: ", path, call. = FALSE)
}

.MAX_DEPTH <- 100L

get_start_path <- function(path, subdirs) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  for (subdir in subdirs) {
    subdir_path <- file.path(path, subdir)
    if (dir.exists(subdir_path)) {
      return(subdir_path)
    }
  }

  path
}

# Borrowed from devtools
is_root <- function(path) {
  identical(
    normalizePath(path, winslash = "/"),
    normalizePath(dirname(path), winslash = "/")
  )
}

#' @rdname find_root
#' @description `get_root_desc()` returns the description of the criterion
#'   for a root path. This is especially useful for composite root criteria
#'   created with [|.root_criterion()].
#' @export
get_root_desc <- function(criterion, path) {
  for (i in seq_along(criterion$testfun)) {
    if (criterion$testfun[[i]](path)) {
      return(criterion$desc[[i]])
    }
  }

  stop("path is not a root. ",
    paste(format(criterion), collapse = "\n"),
    call. = FALSE
  )
}


format_lines <- function(n) {
  if (n == 1) "line" else paste0(n, " lines")
}

#' @details
#' The `has_file()` function constructs a criterion that checks for the
#' existence of a specific file (which itself can be in a subdirectory of the
#' root) with specific contents.
#'
#' @rdname root_criterion
#' @param filepath `[character(1)]`\cr
#'   File path (can contain directories).
#' @param contents,fixed `[character(1)]`\cr
#'   If `contents` is `NULL` (the default), file contents are not checked.
#'   Otherwise, `contents` is a regular expression
#'   (if `fixed` is `FALSE`) or a search string (if `fixed` is `TRUE`), and
#'   file contents are checked matching lines.
#' @param n `[integerish(1)]`\cr
#'   Maximum number of lines to read to check file contents.
#' @export
has_file <- function(filepath, contents = NULL, n = -1L, fixed = FALSE) {
  force(filepath)
  stopifnot(is.character(filepath), length(filepath) == 1)
  force(contents)
  if (!is.null(contents)) {
    stopifnot(is.character(contents), length(contents) == 1)
  }
  force(n)
  stopifnot(length(n) == 1)

  check_relative(filepath)

  testfun <- eval(bquote(function(path) {
    testfile <- file.path(path, .(filepath))
    if (!file.exists(testfile)) {
      return(FALSE)
    }
    if (dir.exists(testfile)) {
      return(FALSE)
    }
    match_contents(testfile, .(contents), .(n), .(fixed))
  }))

  desc <- paste0(
    'contains a file "', filepath, '"',
    if (!is.null(contents)) {
      paste0(
        " with contents ",
        if (!fixed) "matching ",
        '"', contents, '"',
        if (n >= 0L) paste0(" in the first ", format_lines(n))
      )
    }
  )

  root_criterion(testfun, desc)
}

#' @details
#' The `has_dir()` function constructs a criterion that checks for the
#' existence of a specific directory.
#'
#' @rdname root_criterion
#' @export
has_dir <- function(filepath) {
  force(filepath)
  stopifnot(is.character(filepath), length(filepath) == 1)

  check_relative(filepath)

  testfun <- eval(bquote(function(path) {
    testfile <- file.path(path, .(filepath))
    dir.exists(testfile)
  }))

  desc <- paste0('contains a directory "', filepath, '"')

  root_criterion(testfun, desc)
}

check_relative <- function(filepath) {
  if (is_absolute_path(filepath)) {
    stop("filepath must be a file or a relative path, not an absolute path.", call. = FALSE)
  }
}

#' @details
#' The `has_file_pattern()` function constructs a criterion that checks for the
#' existence of a file that matches a pattern, with specific contents.
#'
#' @rdname root_criterion
#' @param pattern `[character(1)]`\cr
#'   Regular expression to match the file name against.
#' @export
has_file_pattern <- function(pattern, contents = NULL, n = -1L, fixed = FALSE) {
  force(pattern)
  stopifnot(is.character(pattern), length(pattern) == 1)
  force(contents)
  if (!is.null(contents)) {
    stopifnot(is.character(contents), length(contents) == 1)
  }
  force(n)
  stopifnot(length(n) == 1)

  testfun <- eval(bquote(function(path) {
    files <- list_files(path, .(pattern))
    for (f in files) {
      if (!match_contents(f, .(contents), .(n), .(fixed))) {
        next
      }
      return(TRUE)
    }
    return(FALSE)
  }))

  desc <- paste0(
    'contains a file matching "', pattern, '"',
    if (!is.null(contents)) {
      paste0(
        " with contents ",
        if (!fixed) "matching ",
        '"', contents, '"',
        if (n >= 0L) paste0(" in the first ", format_lines(n))
      )
    }
  )

  root_criterion(testfun, desc)
}

#' @details
#' The `has_basename()` function constructs a criterion that checks if the
#' [base::basename()] of the root directory has a specific name,
#' with support for case-insensitive file systems.
#'
#' @rdname root_criterion
#' @param basename `[character(1)]`\cr
#'   The required name of the root directory.
#' @export
has_basename <- function(basename, subdir = NULL) {
  force(basename)

  testfun <- eval(bquote(function(path) {
    # Support case insensitive file systems.
    tolower(basename(path)) == tolower(.(basename)) && dir.exists(file.path(dirname(path), .(basename)))
  }))

  desc <- paste0('directory name is "', basename, '"')

  root_criterion(testfun, desc, subdir = subdir)
}

#' @export
is_rstudio_project <- has_file_pattern("[.]Rproj$", contents = "^Version: ", n = 1L)

#' @export
is_r_package <- has_file("DESCRIPTION", contents = "^Package: ")

#' @export
is_remake_project <- has_file("remake.yml")

#' @export
is_drake_project <- has_dir(".drake")

#' @export
is_pkgdown_project <- has_file("_pkgdown.yml") | has_file("_pkgdown.yaml") | has_file("pkgdown/_pkgdown.yml") | has_file("inst/_pkgdown.yml")

#' @export
is_renv_project <- has_file("renv.lock", contents = '"Packages":\\s*\\{')

#' @export
is_projectile_project <- has_file(".projectile")

#' @export
is_quarto_project <- has_file("_quarto.yml")

#' @export
is_git_root <- has_dir(".git") | has_file(".git", contents = "^gitdir: ")

#' @export
is_svn_root <- has_dir(".svn")

#' @export
is_vcs_root <- is_git_root | is_svn_root

#' @export
is_testthat <- has_basename("testthat", c("tests/testthat", "testthat"))

#' @export
from_wd <- root_criterion(function(path) TRUE, "from current working directory")

#' Prespecified criteria
#'
#' This is a collection of commonly used root criteria.
#'
#' @format NULL
#'
#' @export
criteria <- structure(
  list(
    is_rstudio_project = is_rstudio_project,
    is_r_package = is_r_package,
    is_remake_project = is_remake_project,
    is_pkgdown_project = is_pkgdown_project,
    is_renv_project = is_renv_project,
    is_projectile_project = is_projectile_project,
    is_quarto_project = is_quarto_project,
    is_git_root = is_git_root,
    is_svn_root = is_svn_root,
    is_vcs_root = is_vcs_root,
    is_testthat = is_testthat,
    from_wd = from_wd
  ),
  class = "root_criteria"
)

#' @export
#' @importFrom utils str
str.root_criteria <- function(object, ...) {
  str(lapply(object, format))
}

#' @details
#' `is_rstudio_project` looks for a file with extension `.Rproj`.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_rstudio_project"

#' @details
#' `is_r_package` looks for a `DESCRIPTION` file.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_r_package"

#' @details
#' `is_remake_project` looks for a `remake.yml` file.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_remake_project"

#' @details
#' `is_drake_project` looks for a `.drake` directory.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_drake_project"

#' @details
#' `is_pkgdown_project` looks for a `_pkgdown.yml`, `_pkgdown.yaml`, `pkgdown/_pkgdown.yml` and/or `inst/_pkgdown.yml` file.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_pkgdown_project"

#' @details
#' `is_renv_project` looks for an `renv.lock` file.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_renv_project"

#' @details
#' `is_projectile_project` looks for a `.projectile` file.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_projectile_project"

#' @details
#' `is_quarto_project` looks for a `_quarto.yml` file.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_quarto_project"

#' @details
#' `is_git_root` looks for a `.git` directory.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_git_root"

#' @details
#' `is_svn_root` looks for a `.svn` directory.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_svn_root"

#' @details
#' `is_vcs_root` looks for the root of a version control
#' system, currently only Git and SVN are supported.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_vcs_root"

#' @details
#' `is_testthat` looks for the `testthat` directory, works when
#'   developing, testing, and checking a package.
#'
#' @format NULL
#' @rdname criteria
#' @export
"is_testthat"

#' @details
#' `from_wd` uses the current working directory.
#'
#' @format NULL
#' @rdname criteria
#' @export
"from_wd"
