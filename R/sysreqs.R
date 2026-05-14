sysreqs_is_supported <- function(sysreqs_platform = NULL) {
  remote(
    function(...) pkgdepends::sysreqs_is_supported(...),
    list(sysreqs_platform = sysreqs_platform)
  )
}

sysreqs_platforms <- function() {
  remote(
    function() pkgdepends::sysreqs_platforms()
  )
}

sysreqs_list_system_packages <- function() {
  load_extra("pillar")
  remote(
    function() pkgdepends::sysreqs_list_system_packages()
  )
}

sysreqs_db_match <- function(specs, sysreqs_platform = NULL) {
  load_extra("pillar")
  remote(
    function(...) pkgdepends::sysreqs_db_match(...),
    list(specs = specs, sysreqs_platform = sysreqs_platform)
  )
}

sysreqs_db_update <- function() {
  invisible(remote(
    function() pkgdepends::sysreqs_db_update()
  ))
}

sysreqs_db_list <- function(sysreqs_platform = NULL) {
  load_extra("pillar")
  remote(
    function(...) pkgdepends::sysreqs_db_list(...),
    list(sysreqs_platform = sysreqs_platform)
  )
}

check_sysreqs_enabled <- function() {
  sysreqs_enabled <- remote(
    function() {
      config <- pkgdepends::current_config()
      config$get("sysreqs")
    }
  )

  if (!sysreqs_enabled) {
    cli::cli_alert_info(
      "System requirements checking is disabled in the config."
    )
    cli::cli_alert_info(
      "Use {.code Sys.setenv('PKG_SYSREQS' = 'TRUE')} 
      or {.code options(pkg.sysreqs = TRUE)}."
    )

    # Return an empty data frame with the expected structure
    result <- data.frame(
      system_package = character(0),
      installed = logical(0),
      packages = I(list()),
      pre_install = I(list()),
      post_install = I(list())
    )
    class(result) <- c("pkg_sysreqs_check_result", class(result))
    return(pak_preformat(result))
  }

  return(NULL)
}

sysreqs_check_installed <- function(packages = NULL, library = .libPaths()[1]) {
  load_extra("pillar")

  # Check if sysreqs is enabled, return early if disabled
  disabled_result <- check_sysreqs_enabled()
  if (!is.null(disabled_result)) {
    return(invisible(disabled_result))
  }

  result <- remote(
    function(...) {
      ret <- pkgdepends::sysreqs_check_installed(...)
      asNamespace("pak")$pak_preformat(ret)
    },
    list(packages = packages, library = library)
  )

  # Inform about detection method
  if (nrow(result) > 0) {
    cli::cli_alert_info(
      "System packages checked via system package manager only."
    )
    cli::cli_alert_info(
      "Software in non-standard locations may not be detected."
    )
  }

  result
}

sysreqs_fix_installed <- function(packages = NULL, library = .libPaths()[1]) {
  load_extra("pillar")

  # Check if sysreqs is enabled, return early if disabled
  disabled_result <- check_sysreqs_enabled()
  if (!is.null(disabled_result)) {
    return(invisible(disabled_result))
  }

  invisible(remote(
    function(...) {
      ret <- pkgdepends::sysreqs_fix_installed(...)
      asNamespace("pak")$pak_preformat(ret)
    },
    list(packages = packages, library = library)
  ))
}


#' Calculate system requirements of one of more packages
#'
#' @inheritParams pkg_install
#' @param sysreqs_platform System requirements platform.
#'
#'   If `NULL`, then the `sysreqs_platform`
#'   \eval{man_config_link("configuration option")} is used, which defaults to
#'   the current platform.
#'
#'   Set this option if to one of \eval{platforms()} if \eval{.packageName}
#'   fails to correctly detect your platform or if you want to see the system
#'   requirements for a different platform.
#'
#' @return List with entries:
#'   * `os`: character string. Operating system.
#'   * `distribution`: character string. Linux distribution, `NA` if the
#'     OS is not Linux.
#'   * `version`: character string. Distribution version, `NA` is the OS
#'     is not Linux.
#'   * `pre_install`: character vector. Commands to run before the
#'     installation of system packages.
#'   * `install_scripts`: character vector. Commands to run to install the
#'     system packages.
#'   * `post_install`: character vector. Commands to run after the
#'     installation of system packages.
#'   * `packages`: data frame. Information about the system packages that
#'     are needed. It has columns:
#'     * `sysreq`: string, cross-platform name of the system requirement.
#'     * `packages`: list column of character vectors. The names of the R
#'       packages that have this system requirement.
#'     * `pre_install`: list column of character vectors. Commands run
#'       before the package installation for this system requirement.
#'     * `system_packages`: list column of character vectors. Names of
#'       system packages to install.
#'     * `post_install`: list column of character vectors. Commands run
#'       after the package installation for this system requirement.
#'
#' @family package functions
#' @family system requirements functions
#' @export

pkg_sysreqs <- function(
  pkg,
  upgrade = TRUE,
  dependencies = NA,
  sysreqs_platform = NULL
) {
  load_extra("pillar")
  # Check if sysreqs support is enabled
  sysreqs_enabled <- remote(
    function() {
      config <- pkgdepends::current_config()
      config$get("sysreqs")
    }
  )

  result <- remote(
    function(...) {
      get("pkg_sysreqs_internal", asNamespace("pak"))(...)
    },
    list(
      pkg = pkg,
      upgrade = upgrade,
      dependencies = dependencies,
      sysreqs_platform = sysreqs_platform
    )
  )

  # Add informational message if results are shown
  if (sysreqs_enabled && length(result$install_scripts) > 0) {
    cli::cli_alert_info(
      "System packages detected via system package manager only."
    )
    cli::cli_alert_info(
      "Software in non-standard locations may require manual verification."
    )
  } else if (!sysreqs_enabled) {
    cli::cli_alert_info(
      "System requirements lookup is disabled."
    )
    cli::cli_alert_info(
      "Enable with {.code Sys.setenv('PKG_SYSREQS' = 'TRUE')}
      or {.code options(pkg.sysreqs = TRUE)}."
    )
  }

  result
}

pkg_sysreqs_internal <- function(
  pkg,
  upgrade = TRUE,
  dependencies = NA,
  sysreqs_platform = NULL
) {
  dir.create(lib <- tempfile())
  on.exit(rimraf(lib), add = TRUE)
  config <- list(library = lib)
  if (!is.null(sysreqs_platform)) {
    config[["sysreqs_platform"]] <- sysreqs_platform
  }
  if (!is.null(dependencies)) {
    config[["dependencies"]] <- dependencies
  }
  srq <- pkgdepends::sysreqs_install_plan(
    pkg,
    upgrade = upgrade,
    config = config
  )
  class(srq) <- "pak_sysreqs"
  srq
}

os_label <- function(x) {
  dist <- if (x$distribution != tolower(x$distribution)) {
    x$distribution
  } else {
    paste0(
      toupper(substr(x$distribution, 1, 1)),
      substr(x$distribution, 2, nchar(x$distribution))
    )
  }
  paste0(dist, " ", x$version)
}

#' @export

format.pak_sysreqs <- function(x, ...) {
  cli <- load_private_cli()
  label <- os_label(x)
  pkgs <- cisort(unique(unlist(x$packages$packages)))
  pkgs <- structure(vector("list", length(pkgs)), names = pkgs)
  for (i in seq_len(nrow(x$packages))) {
    for (p in x$packages$packages[[i]]) {
      pkgs[[p]] <- c(pkgs[[p]], x$packages$system_packages[[i]])
    }
  }

  sh_wrap <- function(x) {
    out <- cli$ansi_strwrap(x, exdent = 2, width = cli$console_width() - 2)
    if (length(out) > 1) {
      out[-length(out)] <- paste0(out[-length(out)], " \\")
    }
    out
  }

  # Add warning message about detection limitations
  warning_msg <- c(
    "",
    cli$rule(left = "System package detection", col = "yellow"),
    paste(
      cli$col_yellow(cli$symbol$warning),
      "System packages are detected via the system package manager only."
    ),
    "Software installed in non-standard locations may not be detected."
  )

  c(
    cli$rule(left = "Install scripts", right = label),
    x$pre_install,
    sh_wrap(x$install_scripts),
    x$post_install,
    "",
    cli$rule(left = "Packages and their system dependencies"),
    if (length(pkgs)) {
      paste0(
        format(names(pkgs)),
        " ",
        cli$symbol$en_dash,
        " ",
        vcapply(pkgs, function(x) paste(cisort(x), collapse = ", "))
      )
    },
    warning_msg
  )
}

#' @export

print.pak_sysreqs <- function(x, ...) {
  writeLines(format(x, ...))
}

#' @export

`[.pak_sysreqs` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pak_sysreqs")
  NextMethod("[")
}


platforms <- function() {
  paste0(
    '\\code{"',
    sort(sysreqs_platforms()$distribution),
    '"}',
    collapse = ", "
  )
}
