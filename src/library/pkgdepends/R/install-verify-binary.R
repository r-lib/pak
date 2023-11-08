
#' @importFrom desc desc

verify_extracted_package <- function(filename, parent_path) {

  pkg_name <- dir(parent_path)
  pkg_path <- file.path(parent_path, pkg_name)

  if (length(pkg_name) == 0) {
    throw(new_input_error(
      "{filename} is not a valid R package, it is an empty archive"))

  } else if (length(pkg_name) > 1) {
    throw(new_input_error(
      "{filename} is not a valid R package, it should contain a
      single directory"))
  }

  rel_package_files <- c(
    file.path(pkg_name, "Meta", "package.rds"),
    file.path(pkg_name, "DESCRIPTION")
  )
  package_files <- file.path(parent_path, rel_package_files)

  has_files <- file.exists(package_files)
  if (!all(has_files)) {
    miss <- rel_package_files[! has_files]
    throw(new_input_error(
      "{filename} is not a valid binary, it does not contain {miss*}.",
      package = pkg_name))
  }

  rel_dsc_file <- file.path(pkg_name, "DESCRIPTION")
  dsc_file <- file.path(pkg_path, "DESCRIPTION")
  dsc <- tryCatch(
    desc(dsc_file),
    error = function(e) {
      throw(new_input_error(
        "{filename} is not a valid binary, invalid {rel_dsc_file}.",
        package = pkg_name))
    }
  )

  if (!length(dsc$fields())) {
    throw(new_input_error(
      "{filename} is not a valid binary, {rel_dsc_file} is empty.",
      package = pkg_name))
  }

  dsc_pkg <- dsc$get("Package")
  if (is.na(dsc_pkg)) {
    throw(new_input_error(
      "{filename} has no `Package` entry in {rel_dsc_file}",
      package = pkg_name))
  }

  if (pkg_name != str_trim(dsc_pkg[[1]])) {
    throw(new_input_error(
      "{filename} is not a valid binary, package name mismatch in
      archive and in {rel_dsc_file}",
      package = pkg_name))
  }

  if (is.na(dsc$get("Built"))) {
    throw(new_input_error(
      "{filename} is not a valid binary, no 'Built' entry in {rel_dsc_file}.",
      package = pkg_name))
  }

  list(name = pkg_name, path = pkg_path, desc = dsc)
}
