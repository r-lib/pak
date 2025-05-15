# -------------------------------------------------------------------------
# Public API
# -------------------------------------------------------------------------

sysreqs_platforms <- function() {
  as_data_frame(sysreqs2_cmds)
}

sysreqs_is_supported <- function(sysreqs_platform = NULL) {
  sysreqs_platform <- sysreqs_platform %||%
    current_config()$get("sysreqs_platform")
  !is.na(find_sysreqs_platform(sysreqs_platform))
}

sysreqs_db_list <- function(sysreqs_platform = NULL) {
  sysreqs_platform <- sysreqs_platform %||%
    current_config()$get("sysreqs_platform")
  plt <- parse_sysreqs_platform(sysreqs_platform)

  sysreqs_db_update()
  rule_files <- sysreqs2_list_rules()
  rules <- lapply(rule_files, jsonlite::fromJSON, simplifyVector = FALSE)

  matching_deps <- function(rule) {
    for (dep in rule$dependencies) {
      appl <- FALSE
      for (const in dep$constraints) {
        if (
          identical(const$os, plt$os) &&
            identical(const$distribution, plt$distribution) &&
            (is.null(const$versions) || plt$version %in% const$versions)
        ) {
          appl <- TRUE
          break
        }
      }
      if (appl) {
        return(list(
          packages = unname(unlist(dep$packages)),
          pre_install = unname(unlist(dep$pre_install)),
          post_install = unname(unlist(dep$post_install))
        ))
      }
    }
    list(
      packages = NULL,
      pre_install = NULL,
      post_install = NULL
    )
  }

  dependencies <- lapply(rules, matching_deps)
  data_frame(
    name = tools::file_path_sans_ext(basename(rule_files)),
    patterns = lapply(rules, function(r) unlist(r$patterns)),
    packages = lapply(dependencies, "[[", "packages"),
    pre_install = lapply(dependencies, "[[", "pre_install"),
    post_install = lapply(dependencies, "[[", "post_install")
  )
}

sysreqs_db_match <- function(specs, sysreqs_platform = NULL) {
  sysreqs_db_update()
  recs <- sysreqs2_match(specs, sysreqs_platform = sysreqs_platform)
  mapply(
    specs,
    recs,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    FUN = function(spec, rec) {
      data_frame(
        spec = spec,
        sysreq = vcapply(rec, "[[", "sysreq"),
        packages = lapply(rec, "[[", "packages"),
        pre_install = lapply(rec, "[[", "pre_install"),
        post_install = lapply(rec, "[[", "post_install")
      )
    }
  )
}

sysreqs_db_update <- function() {
  invisible(sysreqs2_update_metadata())
}

#' Create an installation plan for system requirements
#'
#' This function uses [new_pkg_installation_proposal()] and its methods
#' to create an installation plan for one or more packages, and then print
#' their system requirements.
#'
#' @param refs Packages to install.
#' @param upgrade If `TRUE`, pkgdepends will choose the latest available
#'   versions of packages, instead of preferring binary packages over
#'   source packages.
#' @param config Configuration options. See
#'   ['Configuration'][pkgdepends-config]. If it does not include
#'   `library`, then a temporary library is used, which is equivalent to
#'   not assuming any preinstalled packages. Pass `sysreqs_platform` here
#'   if you want a different platform than the one R is running on.
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
#' @export
#' @family system requirements functions
#' @seealso [new_pkg_installation_proposal()] to actually install
#' packages, and potentially system requirements.
#' @examplesIf !pkgdepends:::is_rcmd_check()
#' sysreqs_install_plan(
#'   "tidyverse",
#'   config = list(sysreqs_platform = "ubuntu-22.04")
#' )

sysreqs_install_plan <- function(refs, upgrade = TRUE, config = list()) {
  if (!"library" %in% names(config)) {
    dir.create(lib <- tempfile())
    on.exit(unlink(lib, recursive = TRUE), add = TRUE)
    config$library <- lib
  }
  config$sysreqs <- TRUE
  config$sysreqs_lookup_system <- FALSE

  prop <- new_pkg_installation_proposal(refs, config = config)
  if (upgrade) {
    prop$set_solve_policy("upgrade")
  } else {
    prop$set_solve_policy("lazy")
  }
  prop$solve()
  prop$stop_for_solution_error()
  sol <- prop$get_solution()

  sysreqs_platform <- prop$get_config()$get("sysreqs_platform")
  scripts <- sysreqs2_scripts(sol$data$sysreqs_packages, sysreqs_platform)
  res <- scripts[c(
    "os",
    "distribution",
    "version",
    "pre_install",
    "install_scripts",
    "post_install"
  )]

  res$pre_install <- as.character(res$pre_install)
  res$install_scripts <- as.character(res$install_scripts)
  res$post_install <- as.character(res$post_install)

  sysreqs <- lapply(sol$data$sysreqs_packages, sapply, "[[", "sysreq")
  names <- as.character(sort(unique(unlist(sysreqs))))
  if (length(names)) {
    records <- unlist(sol$data$sysreqs_packages, recursive = FALSE)
    names(records) <- vcapply(records, "[[", "sysreq")
    records <- records[!duplicated(names(records))]
  }

  spkgs <- data_frame(
    sysreq = names,
    packages = lapply(names, function(n) {
      sol$data$package[vlapply(sysreqs, function(s) n %in% s)]
    }),
    pre_install = lapply(
      names,
      function(n) as.character(records[[n]]$pre_install)
    ),
    system_packages = lapply(
      names,
      function(n) as.character(records[[n]]$packages)
    ),
    post_install = lapply(
      names,
      function(n) as.character(records[[n]]$post_install)
    )
  )

  res$packages <- spkgs
  res
}

sysreqs_check_installed <- function(packages = NULL, library = .libPaths()[1]) {
  data <- synchronize(when_all(
    sysreqs2_async_update_metadata(),
    async_system_list_packages(),
    async_parse_installed(library = library, packages = packages)
  ))

  spkgs <- data[[2]]
  spkgs <- spkgs[grepl("^.i$", spkgs$status), ]
  rpkgs <- data[[3]]
  if (is.null(rpkgs$SystemRequirements)) {
    rpkgs$SystemRequirements <- rep(NA, nrow(rpkgs))
  }
  sys <- sysreqs2_match(rpkgs$SystemRequirements)
  sys <- sysreqs_update_state(sys, spkgs)
  rpkgs$sysreqs_packages <- sys
  rpkgs <- rpkgs[!vlapply(rpkgs$sysreqs_packages, is.null), ]
  rpkgs$sys_package_name <- lapply(
    rpkgs$sysreqs_packages,
    function(s) as.character(unlist(lapply(s, "[[", "packages")))
  )
  rpkgs <- rpkgs[lengths(rpkgs$sys_package_name) > 0, ]

  upkgs <- unique(sort(unlist(rpkgs$sys_package_name)))
  provided <- c(spkgs$package, unlist(spkgs$provides))

  pre <- vector("list", length(upkgs))
  post <- vector("list", length(upkgs))
  for (pi in seq_along(upkgs)) {
    p <- upkgs[pi]
    for (s in sys) {
      sp <- unlist(lapply(s, "[[", "packages"))
      if (p %in% sp) {
        newpre <- unlist(lapply(s, "[[", "pre_install"))
        if (length(newpre) > 0) pre[[pi]] <- c(pre[[pi]], newpre)
        newpost <- unlist(lapply(s, "[[", "post_install"))
        if (length(newpost) > 0) post[[pi]] <- c(post[[pi]], newpost)
      }
    }
  }

  res <- data_frame(
    system_package = upkgs,
    installed = upkgs %in% provided,
    packages = lapply(upkgs, function(p) {
      rpkgs$Package[vlapply(rpkgs$sys_package_name, function(s) p %in% s)]
    }),
    pre_install = pre,
    post_install = post
  )

  attr(res, "sysreqs_records") <- sys
  attr(res, "system_packages") <- spkgs

  class(res) <- c("pkg_sysreqs_check_result", class(res))
  res
}

#' @export

format.pkg_sysreqs_check_result <- function(x, ...) {
  ok <- cli::col_green(cli::symbol$tick)
  notok <- cli::col_red(cli::symbol$cross)
  req <- vcapply(x$packages, paste, collapse = ", ")
  paste(
    ansi_align_width(c("system package", "--------------", x$system_package)),
    ansi_align_width(c("installed", "--", ifelse(x$installed, ok, notok))),
    ansi_align_width(c("required by", "-----------", req))
  )
}

#' @export

print.pkg_sysreqs_check_result <- function(x, ...) {
  writeLines(format(x, ...))
}

#' @export

`[.pkg_sysreqs_check_result` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pkg_sysreqs_check_result")
  NextMethod("[")
}

sysreqs_fix_installed <- function(packages = NULL, library = .libPaths()[1]) {
  chk <- sysreqs_check_installed(packages = packages, library = library)
  if (nrow(chk) == 0) {
    cli::cli_alert("No system requirements.")
  } else if (all(chk$installed)) {
    cli::cli_alert_success("All system requirements are installed.")
  } else {
    cli::cli_alert_info(
      "Need to install {sum(!chk$installed)} system package{?s}."
    )
    config <- current_config()
    cmds <- sysreqs2_scripts(
      attr(chk, "sysreqs_records"),
      sysreqs_platform = config$get("sysreqs_platform"),
      missing = TRUE
    )
    sysreqs_install(cmds, config)
  }
  invisible(chk)
}

async_parse_installed <- function(library, packages) {
  pkgs <- pkgcache::parse_installed(library = library, packages = packages)
  miss <- unique(setdiff(packages, pkgs$Package))
  if (length(miss)) {
    warning(cli::format_warning(
      "Ignored {length(miss)} package{?s} that {?is/are} not
       installed: {.pkg {miss}}."
    ))
  }

  async_constant(pkgs)
}

sysreqs_list_system_packages <- function() {
  synchronize(async_system_list_packages()) # nocov
}

# -------------------------------------------------------------------------
# Implementation
# -------------------------------------------------------------------------

parse_sysreqs_platform <- function(x) {
  stopifnot(length(x) == 1)

  # full form or only distro [+ version]
  if (
    sum(strsplit(x, "")[[1]] == "-") >= 2 &&
      !grepl("opensuse-leap", x) &&
      !grepl("opensuse-tumbleweed", x)
  ) {
    osplt <- parse_platform(x)
    if (startsWith(osplt$os, "linux-")) {
      rest <- sub(
        "^linux[-]((dietlibc|gnu|musl|uclibc|unknown)[-])?",
        "",
        osplt$os
      )
      osplt$os <- "linux"
    } else {
      rest <- ""
    }
  } else {
    osplt <- data_frame(
      cpu = NA_character_,
      vendor = NA_character_,
      os = "linux"
    )
    rest <- x
  }
  osplt$distribution <- NA_character_
  osplt$version <- NA_character_

  if (nchar(rest) == 0) {
    return(osplt)
  }

  if (grepl("^opensuse-leap-", rest)) {
    osplt$distribution <- "opensuse-leap"
    osplt$version <- sub("^opensuse-leap-", "", rest)
  } else if (grepl("^opensuse-tumbleweed-", rest)) {
    osplt$distribution <- "opensuse-tumbleweed"
    osplt$version <- sub("^opensuse-tumbleweed-", "", rest)
  } else {
    restpcs <- strsplit(rest, "-", fixed = TRUE)[[1]]
    if (length(restpcs) == 1) {
      osplt$distribution <- restpcs
    } else if (length(restpcs) == 2) {
      osplt$distribution <- restpcs[1]
      osplt$version <- restpcs[2]
    } else {
      osplt$distribution <- restpcs[1]
      osplt$version <- paste0(restpcs[-1], collapse = "-")
    }
  }

  osplt
}

sysreqs_resolve <- function(
  sysreqs,
  sysreqs_platform = NULL,
  config = NULL,
  ...
) {
  if (tolower(Sys.getenv("R_PKG_SYSREQS2")) != "false") {
    synchronize(sysreqs2_async_resolve(sysreqs, sysreqs_platform, config, ...))
  } else {
    synchronise(sysreqs_async_resolve(sysreqs, sysreqs_platform, config, ...))
  }
}

sysreqs_async_resolve <- function(sysreqs, sysreqs_platform, config) {
  sysreqs
  sysreqs_platform
  config
  sysreqs_async_resolve_query(sysreqs, sysreqs_platform, config)$then(function(
    resp
  ) {
    if (resp$status_code < 400) return(resp)
    throw(pkg_error(
      call. = FALSE,
      "Failed to look up system requirements for OS {sysreqs_platform}.",
      i = "HTTP error {resp$status_code} for {.url {resp$url}}.",
      i = "Response: {.val {rawToChar(resp$content)}}."
    ))
  })$then(
    function(resp) sysreqs_resolve_process(sysreqs, sysreqs_platform, resp)
  )$then(function(res) add_class(res, "pkg_sysreqs_result"))
}

sysreqs_async_resolve_query <- function(sysreqs, sysreqs_platform, config) {
  config <- config %||% current_config()
  sysreqs_platform <- sysreqs_platform %||% config$get("sysreqs_platform")
  rspm <- config$get("sysreqs_rspm_url")
  rspm_repo_id <- config$get("sysreqs_rspm_repo_id")
  rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)

  plt <- parse_sysreqs_platform(sysreqs_platform)
  req_url <- sprintf(
    "%s/sysreqs?distribution=%s&release=%s",
    rspm_repo_url,
    plt$distribution,
    plt$version
  )

  headers <- c("Content-Type" = "text/plain")

  data <- sysreqs_resolve_make_data(sysreqs)

  http_post(req_url, data = data, headers = headers)
}

sysreqs_resolve_process <- function(sysreqs, sysreqs_platform, resp) {
  hdr <- curl::parse_headers_list(resp$headers)
  cnt <- rawToChar(resp$content)
  Encoding(cnt) <- "UTF-8"

  data <- jsonlite::fromJSON(cnt, simplifyVector = FALSE)

  pre_install <- unique(as.character(unlist(c(
    data[["pre_install"]],
    lapply(data[["dependencies"]], `[[`, "pre_install")
  ))))
  install_scripts <- unique(as.character(unlist(c(
    data[["install_scripts"]],
    lapply(data[["dependencies"]], `[[`, "install_scripts")
  ))))
  post_install <- unique(as.character(unlist(c(
    data[["post_install"]],
    lapply(data[["dependencies"]], `[[`, "post_install")
  ))))

  plt <- parse_sysreqs_platform(sysreqs_platform)
  list(
    os = plt$os,
    distribution = plt$distribution,
    version = plt$version,
    url = resp$url,
    total = resp$times["total"],
    pre_install = pre_install,
    install_scripts = install_scripts,
    post_install = post_install
  )
}

sysreqs_canonise_query <- function(sysreqs) {
  sysreqs <- str_trim(sysreqs)
  sysreqs <- sort(unique(sysreqs[!is.na(sysreqs) & sysreqs != ""]))
  sysreqs <- gsub("\n", "\n ", sysreqs)
  sysreqs
}

sysreqs_resolve_make_data <- function(sysreqs) {
  sysreqs <- sysreqs_canonise_query(sysreqs)
  paste(
    collapse = "\n",
    c(
      "Package: pkgdependssysreqs",
      "Version: 1.0.0",
      "SystemRequirements: ",
      paste0("    ", sysreqs),
      "Note: and thank you!",
      ""
    )
  )
}

sysreqs_install <- function(sysreqs_cmds, config = NULL) {
  config <- config %||% current_config()
  sudo <- config$get("sysreqs_sudo")
  verbose <- config$get("sysreqs_verbose")
  dry_run <- config$get("sysreqs_dry_run")

  cmds <- unlist(sysreqs_cmds[c(
    "pre_install",
    "install_scripts",
    "post_install"
  )])
  if (length(cmds) == 0) return()

  cli::cli_alert_info("Installing system requirements")

  # TODO: fix 'R' commands (e.g. `R CMD javareconf`) to call the current
  # version of R and not the one on the PATH

  cmds <- compact_cmds(cmds)

  if (dry_run) cmds <- paste("echo", cmds)

  if (verbose) {
    callback <- function(x, ...) {
      x <- str_trim(x)
      if (nchar(x)) cli::cli_verbatim(x)
    }
  } else {
    callback <- function(x, ...) invisible()
  }

  output <- lapply(cmds, function(cmd) {
    if (sudo) {
      sh <- "sudo" # nocov
      cmdline <- c("sh", "-c", cmd) # nocov
    } else {
      sh <- "sh"
      cmdline <- c("-c", cmd)
    }
    fullcmd <- paste(c(sh, cmdline), collapse = " ")
    cli::cli_alert_info("Executing {.code {fullcmd}}")
    processx::run(
      sh,
      cmdline,
      stdout_callback = callback,
      stderr_to_stdout = TRUE
    )
  })

  invisible(output)
}

compact_cmds <- function(x) {
  rx <- "^apt-get install -y ([a-z0-9-]+)$"
  if (length(x) == 0 || !all(grepl(rx, x))) {
    return(x)
  }

  paste0(
    "apt-get install -y ",
    paste(gsub(rx, "\\1", x), collapse = " ")
  )
}

is_root <- function() {
  if (os_type() != "unix") return(FALSE)
  ps::ps_uids()[["effective"]] == 0
}

can_sudo_without_pw <- function() {
  if (os_type() != "unix") return(FALSE)
  tryCatch(
    {
      processx::run("sudo", c("-s", "id"))
      TRUE
    },
    error = function(err) FALSE
  )
}
