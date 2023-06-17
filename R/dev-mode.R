
# These functions are only used for pak development

create_dev_lib <- function(lib = NULL) {
  lib <- lib %||% private_lib_dir()

  if (identical(names(lib), "embedded")) stop("Private lib is embedded")

  rimraf(lib)
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(pkg_data$remote)) pkg_data$remote$kill()
  pkg_dirs <- private_lib_packages()

  pkg_names <- basename(pkg_dirs)
  cli::cli_alert_info(
    "Copying {length(pkg_dirs)} package{?s} into private lib: {.pkg {pkg_names}}.",
    wrap = TRUE
  )

  for(i in seq_along(pkg_dirs)) copy_package(pkg_dirs[i], lib)

  invisible()
}

copy_package <- function(from, lib) {
  file.copy(from, lib, recursive = TRUE)
}

private_lib_packages <- function(lib = .libPaths()) {
  path <- getNamespaceInfo(asNamespace(.packageName), "path")
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  top_deps <- parse_dep_fields(dcf[, "Config/needs/dependencies"])
  unique(unlist(lapply(top_deps, lookup_deps, lib)))
}

lookup_deps <- function(pkg, lib_path = .libPaths()) {
  lib_pkgs <- lapply(lib_path, dir)

  ## TODO: check for version requirements
  find_lib <- function(pkg) {
    w <- utils::head(which(vlapply(lib_pkgs, `%in%`, x = pkg)), 1)
    if (!length(w)) {
      stop("Required package `", pkg, "` is not available")
    }
    file.path(lib_path[w], pkg)
  }

  result <- find_lib(pkg)
  todo <- find_lib(pkg)
  done <- character()

  while (length(todo)) {
    new <- unlist(lapply(todo, extract_deps))
    new <- setdiff(new, done)
    new_paths <- vcapply(new, find_lib)
    result <- unique(c(result, new_paths))
    done <- unique(done, new)
    todo <- new_paths
  }

  result
}

extract_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  deps <- paste(c(
    if ("Imports" %in% colnames(dcf)) dcf[, "Imports"],
    if ("Depends" %in% colnames(dcf)) dcf[, "Depends"]
  ), collapse = ", ")

  na_omit(parse_dep_fields(deps))
}

parse_dep_fields <- function(deps) {
  deps <- str_trim(strsplit(deps, ",")[[1]])
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  deps <- setdiff(vapply(deps, "[", "", 1), c("R", base_packages()))
  # Edit GH remotes
  deps <- sub("^[^/]*/", "", deps)
  deps <- sub("@[^@]*$", "", deps)
  deps
}
