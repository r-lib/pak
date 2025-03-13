
install_extracted_binary <- function(filename, lib_cache, pkg_cache, lib,
                                     metadata, now) {

  pkg <- verify_extracted_package(filename, pkg_cache)
  add_metadata(pkg$path, metadata)
  pkg_name <- pkg$name

  lockfile <- lock_cache(lib_cache, pkg_name, getOption("install.lock"))
  if (!is.null(lockfile)) {
    on.exit(filelock::unlock(lockfile), add = TRUE)
  }

  installed_path <- file.path(lib, pkg_name)
  if (file.exists(installed_path)) {
    if (is_windows()) {
      # First move the existing library (which still works even if a process has
      # the DLL open), then try to delete it, which may fail if another process
      # has the file open. Some points:
      # - the <lib_cache> / <pkg_name> directory might exist with the leftovers
      #   of a previous installation, typically because the DLL file was/is
      #   locked, so we could not delete it after the move.
      # - so we create a random path component to avoid interference
      # - we also unlink() the whole package-specific cache directory,
      #   to avoid accumulating junk there. This is safe, well, if we are
      #   locking, which is strongly suggested.
      move_to <- file.path(lib_cache, pkg_name, basename(tempfile()))
      unlink(dirname(move_to), recursive = TRUE, force = TRUE)
      dir.create(dirname(move_to), showWarnings = FALSE, recursive = TRUE)
      ret <- file.rename(installed_path, move_to)
      if (!ret) {
        throw(pkg_error(
          "Failed to move installed package {.pkg {pkg_name}} at
          {.path {installed_path}}.",
          .data = list(package = pkg_name),
          .class = "install_filesystem_error"
        ))
      }
      ret <- unlink(move_to, recursive = TRUE, force = TRUE)
      if (ret != 0) {
        throw(pkg_warning(
          "Failed to remove installed package at {.path {move_to}}.",
          .data = list(package = pkg_name),
          .class = "install_filesystem_warning"
        ))
      }
    } else {
      # On Unix we are fine with just deleting the old package
      ret <- unlink(installed_path, recursive = TRUE, force = TRUE)
      if (ret != 0) {
        throw(pkg_warning(
          "Failed to remove installed package at {.path {installed_path}}.",
          .data = list(package = pkg_name),
          .class = "install_filesystem_warning"
        ))
      }
    }
  }
  ret <- file.rename(pkg$path, installed_path)
  if (!ret) {
    throw(pkg_error(
      "Unable to move package from {.path {pkg$path}} to
      {.path {installed_path}}",
      .data = list(package = pkg_name),
      .class = "install_filesystem_error"
    ))
  }

  installed_path
}

#' @importFrom utils modifyList
add_metadata <- function(pkg_path, metadata) {
  if (!length(metadata)) return()

  ## During installation, the DESCRIPTION file is read and an package.rds
  ## file created with most of the information from the DESCRIPTION file.
  ## Functions that read package metadata may use either the DESCRIPTION
  ## file or the package.rds file, therefore we attempt to modify both of
  ## them, and return an error if neither one exists.

  source_desc <- file.path(pkg_path, "DESCRIPTION")
  binary_desc <- file.path(pkg_path, "Meta", "package.rds")
  if (file.exists(source_desc)) {
    do.call(
      desc::desc_set,
      c(as.list(metadata), list(file = source_desc, check = FALSE))
    )
  }

  if (file.exists(binary_desc)) {
    pkg_desc <- base::readRDS(binary_desc)
    desc <- as.list(pkg_desc$DESCRIPTION)
    desc <- modifyList(desc, as.list(metadata))
    pkg_desc$DESCRIPTION <- stats::setNames(as.character(desc), names(desc))
    base::saveRDS(pkg_desc, binary_desc)
  }

  if (!file.exists(source_desc) && !file.exists(binary_desc)) {
    throw(pkg_error(
      "Could not find {.file DESCRIPTION} file when installing package
       into {.path {pkg_path}}.",
      i = msg_internal_error()
    ))
  }

  md5 <- file.path(pkg_path, "MD5")
  if (file.exists(md5)) {
    lines <- readLines(md5)
    f1 <- grep("^[a-fA-F0-9]*[ |*][ |*]*DESCRIPTION", lines)[1]
    f2 <- grep("^[a-fA-F0-9]*[ |*][ |*]*Meta[/\\\\]package.rds", lines)[1]
    if (!is.na(f1)) {
      h1 <- safe_md5sum(source_desc)
      lines[f1] <- sub("^[a-fA-F0-9]*", h1, lines[f1])
    }
    if (!is.na(f2)) {
      h2 <- safe_md5sum(binary_desc)
      lines[f2] <- sub("[a-fA-F0-9]*", h2, lines[f2])
    }
    if (!is.na(f1) || !is.na(f2)) cat(lines, file = md5, sep = "\n")
  }
}

make_install_process <- function(filename, lib = .libPaths()[[1L]],
                                 metadata = NULL) {
  filename; lib; metadata

  now <- Sys.time()

  type <- detect_package_archive_type(filename)
  if (type == "unknown") {
    throw(pkg_error(
      "Cannot extract {.path {filename}}, unknown archive type.",
      .class = "install_input_error"
    ))
  }

  lib_cache <- library_cache(lib)
  mkdirp(pkg_cache <- tempfile(tmpdir = lib_cache))

  ppfun <- function() {
   install_extracted_binary(filename, lib_cache, pkg_cache, lib,
                            metadata, now)
  }

  p <- if (type == "zip") {
    make_unzip_process(filename, exdir = pkg_cache, post_process = ppfun)
  } else {
    ## TODO: we already know the package type, no need to detect again
    make_untar_process(filename, exdir = pkg_cache, post_process = ppfun)
  }

  reg.finalizer(p, function(...) unlink(pkg_cache, recursive = TRUE),
                onexit = TRUE)

  p
}
