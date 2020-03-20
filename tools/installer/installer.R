
get_os <- function () {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    "mac"
  } else if (Sys.info()[["sysname"]] == "Linux") {
    "linux"
  } else {
    stop("Unknown OS")
  }
}

build_installer <- function() {
  os <- get_os()

  dir.create(lib <- tempfile())
  on.exit(rimraf(lib), add = TRUE)

  dsc <- desc::desc()

  # Install pak without dependencies
  cli::cli_h2("Installing pak")
  me <- pkgdepends::new_pkg_installation_proposal(
    "local::.",
    config = list(
      dependencies = FALSE,
      library = lib,
      `build-vignettes` = FALSE
    )
  )
  me$resolve()
  me$solve()
  me$download()
  me$install()

  dir.create(tmp <- tempfile())
  on.exit(rimraf(tmp), add = TRUE)
  dsc$del_dep("covr")
  dsc$del_dep("mockery")
  dsc$del_dep("withr")
  dsc$del_dep("testthat")
  dsc$del_dep("pingr")

  deps <- dsc$get_deps()
  deps$type[deps$type == "Suggests"] <- "Imports"
  dsc$set_deps(deps)
  dsc$write(file.path(tmp, "DESCRIPTION"))

  cli::cli_h2("Installing dependencies")
  privlib <- file.path(lib, "pak", "library")
  deps <- pkgdepends::new_pkg_installation_proposal(
    paste0("deps::", tmp),
    config = list(
      library = privlib,
      `build-vignettes` = FALSE
    )
  )
  deps$resolve()
  deps$solve()

  # Warn for non-standard dependencies
  sol <- deps$get_solution()$data
  if (any(bad <- (! sol$type %in% c("deps", "standard")))) {
    cli::cli_alert_warning("Development dependencies: {sol$ref[bad]}")
  }

  deps$download()
  deps$install()

  minimize_library(lib)

  ver <- dsc$get("Version")

  withr::with_dir(lib, {
    if (os == "mac") {
      pkg_file <- paste0("pak_", ver, ".tgz")
      utils::tar(
        pkg_file, files = "pak", tar = "internal",
        compression = "gzip")
    } else if (os == "win") {
      pkg_file <- paste0("pak_", ver, ".zip")
      zip::zipr(pkg_file, files = "pak")
    } else {
      # curl_4.3_R_x86_64-pc-linux-gnu.tar.gz
      pkg_file <- paste0("pak_", ver, "_R_x86_64-pc-linux-gnu.tar.gz")
      utils::tar(
        pkg_file, files = "pak", tar = "internal",
        compression = "gzip")
    }
  })

  arch <- R.version$platform
  rver <- getRversion()
  major <- paste0(rver$major, ".", rver$minor)

  local <- file.path("bin", arch, major, pkg_file)
  mkdirp(dirname(local))

  file.copy(file.path(lib, pkg_file), local, overwrite = TRUE)

  pkg_file
}

rimraf <- function(x) {
  if ("~" %in% x) stop("Cowardly refusing to delete `~`")
  unlink(x, recursive = TRUE, force = TRUE)
}

mkdirp <- function(x) {
  dir.create(x, showWarnings = FALSE, recursive = TRUE)
}

minimize_library <- function(lib) {
  pkgs <- dir(file.path(lib, "pak", "library"))

  # Remove docs
  rimraf(file.path(lib, "pak", "library", pkgs, "help"))
  rimraf(file.path(lib, "pak", "library", pkgs, "doc"))

  # Remove .so.dSYM junk files
  rimraf(dir(
    lib,
    full.names = TRUE,
    recursive = TRUE,
    pattern = "\\.so\\.dSYM$",
    include.dirs = TRUE
  ))

  # Strip shared libs
  shlibs <- dir(
    lib,
    full.names = TRUE,
    recursive = TRUE,
    pattern = "\\.so$",
    include.dirs = TRUE
  )
  cmd <- strsplit(Sys.getenv("R_STRIP_SHARED_LIB", "strip -x"), "\\s+")[[1]]
  for (shl in shlibs) processx::run(cmd[1], c(cmd[-1], shl))

  # Remove pak/library/_cache
  rimraf(file.path(lib, "pak", "library", "_cache"))
}

main <- function() {
  build_installer()
}

if (is.null(sys.calls())) main()
