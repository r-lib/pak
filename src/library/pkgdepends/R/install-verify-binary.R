verify_extracted_package <- function(filename, parent_path) {
  pkg_name <- dir(parent_path)
  pkg_path <- file.path(parent_path, pkg_name)

  if (length(pkg_name) == 0) {
    throw(pkg_error(
      "{.path {filename}} is not a valid R package, it is an empty archive.",
      .class = "install_input_error"
    ))
  } else if (length(pkg_name) > 1) {
    throw(pkg_error(
      "{.path {filename}} is not a valid R package, it should contain a
      single directory",
      .class = "install_input_error"
    ))
  }

  rel_package_files <- c(
    file.path(pkg_name, "Meta", "package.rds"),
    file.path(pkg_name, "DESCRIPTION")
  )
  package_files <- file.path(parent_path, rel_package_files)

  has_files <- file.exists(package_files)
  if (!all(has_files)) {
    miss <- rel_package_files[!has_files]
    throw(pkg_error(
      "{.path {filename}} is not a valid binary, it is missing {miss}.",
      .data = list(package = pkg_name),
      .class = "install_input_error"
    ))
  }

  rel_dsc_file <- file.path(pkg_name, "DESCRIPTION")
  dsc_file <- file.path(pkg_path, "DESCRIPTION")
  dsc <- tryCatch(
    desc::desc(dsc_file),
    error = function(e) {
      throw(pkg_error(
        "{.path {filename}} is not a valid binary, invalid DESCRIPTION
        file at {.path {rel_dsc_file}}.",
        .data = list(package = pkg_name),
        .class = "install_input_error"
      ))
    }
  )

  if (!length(dsc$fields())) {
    throw(pkg_error(
      "{.path {filename}} is not a valid binary, empty DESCRIPTION file
      at {.path {rel_dsc_file}}.",
      .data = list(package = pkg_name),
      .class = "install_input_error"
    ))
  }

  dsc_pkg <- dsc$get("Package")
  if (is.na(dsc_pkg)) {
    throw(pkg_error(
      "{.path {filename}} has no `Package` entry in DESCRIPTION at
      {.path {rel_dsc_file}}.",
      .data = list(package = pkg_name),
      .class = "install_input_error"
    ))
  }

  if (pkg_name != str_trim(dsc_pkg[[1]])) {
    throw(pkg_error(
      "{.path {filename}} is not a valid binary, package name mismatch in
      archive and in {.path {rel_dsc_file}}.",
      .data = list(package = pkg_name),
      .class = "install_input_error"
    ))
  }

  if (is.na(dsc$get("Built"))) {
    throw(pkg_error(
      "{.path {filename}} is not a valid binary, no 'Built' entry in
      {.path {rel_dsc_file}}.",
      .data = list(package = pkg_name),
      .class = "install_input_error"
    ))
  }

  list(name = pkg_name, path = pkg_path, desc = dsc)
}
