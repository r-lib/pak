#' Check whether a package name is valid
#'
#' @param nm Potential package name, string of length 1.
#' @return Logical flag. If `FALSE`, then the `reason` attribute
#'   contains a character string, the explanation why the package name
#'   is invalid. See examples below.
#'
#' @export
#' @examples
#' is_valid_package_name("pak")
#' is_valid_package_name("pkg")
#' is_valid_package_name("pak\u00e1ge")
#' is_valid_package_name("good-package")
#' is_valid_package_name("x")
#' is_valid_package_name("1stpackage")
#' is_valid_package_name("dots.")

is_valid_package_name <- function(nm) {
  assert_that(is_string(nm))

  # CRAN did not like these names
  forbidden_names <- c("pkg", "description")

  # package_name_rx is in parse-remotes.R
  if (
    grepl(paste0("^", package_name_rx(), "$"), nm) &&
      !tolower(nm) %in% forbidden_names
  ) {
    return(TRUE)
  }

  # From WRE:
  # The mandatory ‘Package’ field gives the name of the package.
  # This should contain only (ASCII) letters, numbers and dot, have at
  # least two characters and start with a letter and not end in a dot.

  why <- if (tolower(nm) %in% forbidden_names) {
    "Package name forbidden by CRAN."
  } else if (!is_ascii(nm)) {
    "It can only contain ASCII characters."
  } else if (grepl("[^a-zA-Z0-9.]", nm)) {
    "It can only contain letters, numbers and dot."
  } else if (nchar(nm) < 2) {
    "It must have at least two characters."
  } else if (!grepl("^[a-zA-Z]", nm)) {
    "It must start with a letter."
  } else if (grepl("[.]$", nm)) {
    "It must not end with a dot."
  } else {
    "Package name is invalid." # nocov
  }

  structure(FALSE, reason = why)
}

is_ascii <- function(x) {
  all(charToRaw(x) < 128)
}
