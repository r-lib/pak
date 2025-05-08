#' Build package
#'
#' Building converts a package source directory into a single bundled file.
#' If `binary = FALSE` this creates a `tar.gz` package that can
#' be installed on any platform, provided they have a full development
#' environment (although packages without source code can typically be
#' installed out of the box). If `binary = TRUE`, the package will have
#' a platform specific extension (e.g. `.zip` for windows), and will
#' only be installable on the current platform, but no development
#' environment is needed.
#'
#' ## Configuration
#'
#' ### `DESCRIPTION` entries
#'
#' * `Config/build/clean-inst-doc` can be set to `FALSE` to avoid cleaning up
#'   `inst/doc` when building a source package. Set it to `TRUE` to force a
#'   cleanup. See the `clean_doc` argument.
#'
#' * `Config/build/copy-method` can be used to avoid copying large
#'   directories in `R CMD build`. It works by copying (or linking) the
#'   files of the package to a temporary directory, leaving out the
#'   (possibly large) files that are not part of the package. Possible
#'   values:
#'
#'   - `none`: pkgbuild does not copy the package tree. This is the default.
#'   - `copy`: the package files are copied to a temporary directory before
#'     ` R CMD build`.
#'   - `link`: the package files are symbolic linked to a temporary
#'     directory before `R CMD build`. Windows does not have symbolic
#'     links, so on Windows this is equivalent to `copy`.
#'
#'   You can also use the `pkg.build_copy_method` option or the
#'   `PKG_BUILD_COPY_METHOD` environment variable to set the copy method.
#'   The option is consulted first, then the `DESCRIPTION` entry, then the
#'   environment variable.
#'
#' * `Config/build/extra-sources` can be used to define extra source files
#'   for pkgbuild to decide whether a package DLL needs to be recompiled in
#'   `needs_compile()`. The syntax is a comma separated list of file names,
#'   or globs. (See [utils::glob2rx()].) E.g. `src/rust/src/*.rs` or
#'   `configure*`.
#'
#' * `Config/build/bootstrap` can be set to `TRUE` to run
#'   `Rscript bootstrap.R` in the source directory prior to running subsequent
#'   build steps.
#'
#' * `Config/build/never-clean` can be set to `TRUE` to never add `--preclean`
#'   to `R CMD INSTALL`, e.g., when header files have changed.
#'   This helps avoiding rebuilds that can take long for very large C/C++ codebases
#'   and can lead to build failures if object files are out of sync with header files.
#'   Control the dependencies between object files and header files
#'   by adding `include file.d` to `Makevars` for each `file.c` or `file.cpp` source file.
#'
#' ### Options
#'
#' * `pkg.build_copy_method`: use this option to avoid copying large
#'   directories when building a package. See possible values above, at the
#'   `Config/build/copy-method` `DESCRIPTION` entry.
#'
#' * `pkg.build_stop_for_warnings`: if it is set to `TRUE`, then pkgbuild
#'   will stop for `R CMD build` errors. It takes precedence over the
#'   `PKG_BUILD_STOP_FOR_WARNINGS` environment variable.
#'
#' ### Environment variables
#'
#' * `PKG_BUILD_COLOR_DIAGNOSTICS`: set it to `false` to opt out of colored
#'  compiler diagnostics. Set it to `true` to force colored compiler
#'  diagnostics.
#'
#' * `PKG_BUILD_COPY_METHOD`: use this environment variable to avoid copying
#'   large directories when building a package. See possible values above,
#'   at the `Config/build/copy-method` `DESCRIPTION` entry.
#'
#' will stop for `R CMD build` errors. The `pkg.build_stop_for_warnings`
#' option takes precedence over this environment variable.
#'
#' @param path Path to a package, or within a package.
#' @param dest_path path in which to produce package. If it is an existing
#'   directory, then the output file is placed in `dest_path` and named
#'   according to the current R conversions (e.g. `.zip` for Windows binary
#'   packages, `.tgz` for macOS binary packages, etc).
#'   If it is an existing file, then it will be overwritten.
#'   If `dest_path` does not exist, then it is used as a file name.
#'   If `NULL`, it defaults to the parent directory of the package.
#' @param binary Produce a binary (`--binary`) or source (
#'   `--no-manual --no-resave-data`) version of the package.
#' @param vignettes,manual For source packages: if `FALSE`, don't build PDF
#'   vignettes (`--no-build-vignettes`) or manual (`--no-manual`).
#' @param args An optional character vector of additional command
#'   line arguments to be passed to `R CMD build` if `binary = FALSE`,
#'   or `R CMD install` if `binary = TRUE`.
#' @param quiet if `TRUE` suppresses output from this function.
#' @param needs_compilation Usually only needed if the packages has
#'   C/C++/Fortran code. By default this is autodetected.
#' @param compile_attributes if `TRUE` and the package uses Rcpp, call
#'   [Rcpp::compileAttributes()] before building the package. It is ignored
#'   if package does not need compilation.
#' @param register_routines if `TRUE` and the package does not use Rcpp, call
#'   register routines with
#'   `tools::package_native_routine_registration_skeleton()` before building
#'   the package. It is ignored if package does not need compilation.
#' @param clean_doc If `TRUE`, clean the files in `inst/doc` before building
#'   the package. If `NULL` and the `Config/build/clean-inst-doc` entry is
#'   present in `DESCRIPTION`, then that is used. Otherwise, if `NULL`,
#'   and interactive, ask to remove the files prior to cleaning. In most
#'   cases cleaning the files is the correct behavior to avoid stale
#'   vignette outputs in the built package.
#' @export
#' @return a string giving the location (including file name) of the built
#'  package
build <- function(
  path = ".",
  dest_path = NULL,
  binary = FALSE,
  vignettes = TRUE,
  manual = FALSE,
  clean_doc = NULL,
  args = NULL,
  quiet = FALSE,
  needs_compilation = pkg_has_src(path),
  compile_attributes = FALSE,
  register_routines = FALSE
) {
  options <- build_setup(
    path,
    dest_path,
    binary,
    vignettes,
    manual,
    clean_doc,
    args,
    needs_compilation,
    compile_attributes,
    register_routines,
    quiet
  )
  on.exit(unlink(options$out_dir, recursive = TRUE), add = TRUE)

  withr_with_makevars(
    compiler_flags(debug = FALSE),
    {
      output <- withr_with_temp_libpaths(
        rcmd_build_tools(
          options$cmd,
          c(options$path, options$args),
          wd = options$out_dir,
          fail_on_status = TRUE,
          required = FALSE, # already checked in setup
          quiet = quiet
        )
      )

      if (
        should_stop_for_warnings() &&
          grepl("\n\\s*warning:", output$stdout, ignore.case = TRUE)
      ) {
        cli::cli_alert_warning(
          "Stopping as requested for a warning during {.code R CMD build}."
        )
        if (quiet) {
          cli::cli_alert_warning("The full output is printed below.")
          cli::cli_verbatim(output$stdout)
        }
        stop("converted from `R CMD build` warning.")
      }

      out_file <- dir(options$out_dir)
      file.copy(
        file.path(options$out_dir, out_file),
        options$dest_path,
        overwrite = TRUE
      )

      if (is_dir(options$dest_path)) {
        file.path(options$dest_path, out_file)
      } else {
        options$dest_path
      }
    }
  )
}

build_setup <- function(
  path,
  dest_path,
  binary,
  vignettes,
  manual,
  clean_doc,
  args,
  needs_compilation,
  compile_attributes,
  register_routines,
  quiet
) {
  if (!file.exists(path)) {
    stop("`path` must exist", call. = FALSE)
  }
  if (!is_dir(path)) {
    if (!binary) stop("`binary` must be TRUE for package files", call. = FALSE)
    if (compile_attributes) {
      stop(
        "`compile_attributes` must be FALSE for package files",
        call. = FALSE
      )
    }
    if (register_routines) {
      stop("`register_routines` must be FALSE for package files", call. = FALSE)
    }
  } else {
    path <- pkg_path(path)
  }

  if (is.null(dest_path)) {
    dest_path <- dirname(path)
  }

  if (binary) {
    build_setup_binary(path, dest_path, args, needs_compilation)
  } else {
    build_setup_source(
      path,
      dest_path,
      vignettes,
      manual,
      clean_doc,
      args,
      needs_compilation,
      compile_attributes,
      register_routines,
      quiet
    )
  }
}

build_setup_binary <- function(path, dest_path, args, needs_compilation) {
  if (needs_compilation) {
    check_build_tools(quiet = TRUE)
  }

  # Build in temporary directory and then copy to final location
  out_dir <- tempfile()
  dir.create(out_dir)

  list(
    cmd = "INSTALL",
    path = normalizePath(path),
    args = c("--build", args),
    out_dir = out_dir,
    dest_path = dest_path
  )
}

build_setup_source <- function(
  path,
  dest_path,
  vignettes,
  manual,
  clean_doc,
  args,
  needs_compilation,
  compile_attributes,
  register_routines,
  quiet
) {
  bootstrap_file <- file.path(path, "bootstrap.R")
  run_bootstrap <- isTRUE(get_desc_config_flag(path, "bootstrap"))
  if (file.exists(bootstrap_file) && run_bootstrap) {
    if (!quiet) message("Running bootstrap.R...")

    callr::rscript(
      bootstrap_file,
      wd = path,
      stderr = "2>&1",
      show = !quiet
    )
  }

  if (needs_compilation) {
    update_registration(path, compile_attributes, register_routines, quiet)
  }

  if (!("--resave-data" %in% args)) {
    args <- c(args, "--no-resave-data")
  }

  if (!manual) {
    args <- unique(c(args, "--no-manual"))
  }

  if (!vignettes) {
    args <- unique(c(args, "--no-build-vignettes"))
  }

  no_manual <- "--no-manual" %in% args
  if (!no_manual && !has_latex()) {
    message("pdflatex not found! Not building PDF manual.")
    manual <- FALSE
  }

  if (needs_compilation && (vignettes || manual)) {
    check_build_tools(quiet = TRUE)
  }

  build_vignettes <- !("--no-build-vignettes" %in% args)
  if (is.null(clean_doc)) {
    clean_doc <- get_desc_config_flag(path, "clean-inst-doc")
  }
  if (build_vignettes && (is.null(clean_doc) || isTRUE(clean_doc))) {
    doc_dir <- file.path(path, "inst", "doc")
    if (dir.exists(doc_dir)) {
      if (is.null(clean_doc) && interactive()) {
        message(
          "Building the package will delete...\n  '",
          doc_dir,
          "'\nAre you sure?"
        )
        res <- utils::menu(c("Yes", "No"))
        if (res == 2) {
          return()
        }
      }
      unlink(doc_dir, recursive = TRUE)
    }
  }

  # Build in temporary directory and then copy to final location
  out_dir <- tempfile()
  dir.create(out_dir)

  copy_method <- get_copy_method(path)

  if (copy_method != "none") {
    pkgname <- desc::desc_get("Package", path)
    tmppath <- tempfile("build-")
    copy_package_tree(path, tmppath, pkgname)
    path <- file.path(tmppath, pkgname)
  }

  list(
    cmd = "build",
    path = normalizePath(path),
    args = args,
    out_dir = out_dir,
    dest_path = dest_path
  )
}
