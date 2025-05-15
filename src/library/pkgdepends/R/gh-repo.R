ghrepo <- local({
  ghrepo_update <- function(
    repo,
    subdir,
    release_org = "cran",
    source_repo = "https://cran.r-project.org",
    ignore_build_errors = TRUE,
    packages = NULL
  ) {
    subdir <- sub("/+$", "", subdir)
    if (endsWith(subdir, "src/contrib")) {
      subdir <- sub("src/contrib$", "", subdir)
    }

    cli::cli_h2("Collect information")
    source_pkgs <- get_cran_packages(source_repo)
    mirror_pkgs <- get_mirror_packages(repo, subdir)

    to_build <- get_outdated_packages(source_pkgs, mirror_pkgs, packages)
    if (length(to_build) == 0) return(invisible(character()))

    cli::cli_h2("Install packages")
    dir.create(lib <- tempfile("pkgdepends-lib-"))
    inst <- install_pkgs(
      c(to_build, if (ignore_build_errors) "*=?ignore-build-errors"),
      library = lib,
      source_repo = source_repo,
      repo = repo,
      subdir = subdir
    )
    inst <- keep_updated(inst, mirror_pkgs)
    # I don't think this can happen....
    if (nrow(inst) == 0) return(invisible(character()))

    cli::cli_h2("Build binary packages")
    inst <- build_pkgs(inst, lib)
    # drop the ones that failed to build
    inst <- inst[!is.na(inst$built_path), ]

    cli::cli_par()
    cli::cli_h2("Update package mirror at GH (if needed)")
    # make sure these packages are mirrored on GH
    for (i in seq_along(inst$package)) {
      ghmirror$update(inst$package[[i]], source_repo = source_repo)
    }

    if (nrow(inst) > 0) {
      cli::cli_par()
      cli::cli_h2("Upload new release assets to GH")
      upload_releases(inst, release_org)
    }

    if (nrow(inst) > 0) {
      cli::cli_par()
      cli::cli_h2("Update repository metadata at GH")
      # unfortunate name collision
      repo_slug <- repo
      rm(repo)
      contrib_url <- utils::contrib.url(repos = "", type = .Platform$pkgType)
      repo$update_gh(
        repo_slug,
        paste0(subdir, contrib_url),
        inst$built_path,
        release_org = release_org
      )
    }

    cli::cli_par()
    cli::cli_h2("Build report")
    print_report(inst, mirror_pkgs)

    invisible(inst)
  }

  # -----------------------------------------------------------------------
  # Internal functions

  parse_build_number <- function(x) {
    x <- basename(x)
    mch <- re_match(x, "_b(?<build>[0-9]+)_")
    build <- as.integer(mch$build) + 1L
    build[is.na(build)] <- 1L
    build
  }

  get_cran_packages <- function(repo) {
    proc <- cli::cli_process_start("Getting packages from {.url {repo}}.")
    r_source <- suppressMessages(pkgcache::cranlike_metadata_cache$new(
      platforms = "source",
      bioc = FALSE,
      cran_mirror = repo,
      repos = NULL,
      update_after = as.difftime(30, units = "mins")
    ))
    suppressMessages(r_source$update())
    res <- r_source$list()

    cli::cli_process_done(proc)
    res
  }

  get_mirror_repo_url <- function(repo, subdir) {
    subdir <- sub("/src/contrib/?$", "", subdir)
    paste0(
      "https://raw.githubusercontent.com/",
      repo,
      "/main/",
      subdir
    )
  }

  get_mirror_packages <- function(repo, subdir) {
    proc <- cli::cli_process_start(
      "Getting packages from {.emph {repo}/{subdir}}."
    )
    platform <- if (.Platform$pkgType == "source") "source" else
      pkgcache::current_r_platform()
    r_mirror <- suppressMessages(pkgcache::cranlike_metadata_cache$new(
      platforms = platform,
      bioc = FALSE,
      cran_mirror = get_mirror_repo_url(repo, subdir),
      repos = NULL,
      update_after = as.difftime(1, units = "mins")
    ))
    suppressMessages(r_mirror$update())
    res <- r_mirror$list()

    cli::cli_process_done(proc)
    res
  }

  get_outdated_packages <- function(source_pkgs, mirror_pkgs, extra) {
    # these are included already
    extra <- setdiff(extra, mirror_pkgs$package)

    # TODO: update for breaking changes in R as well
    to_build <- intersect(mirror_pkgs$package, source_pkgs$package)
    r_mirror_ver <- mirror_pkgs$version[match(to_build, mirror_pkgs$package)]
    r_source_ver <- source_pkgs$version[match(to_build, source_pkgs$package)]
    to_build <- to_build[package_version(r_mirror_ver) < r_source_ver]

    res <- c(to_build, extra)
    if (length(res) == 0) {
      cli::cli_alert_info("All packages are current, no updates are needed")
    } else {
      cli::cli_alert_info(
        "Need to build {length(res)} package{?s}: {.pkg {res}}."
      )
    }
    res
  }

  install_pkgs <- function(pkgs, library, source_repo, repo, subdir) {
    opt <- options(
      repos = c(
        RHUB = get_mirror_repo_url(repo, subdir),
        CRAN = source_repo
      )
    )
    on.exit(options(opt), add = TRUE)
    prop <- new_pkg_installation_proposal(
      pkgs,
      config = list(
        library = library,
        platforms = c(current_r_platform(), "source"),
        use_bioconductor = FALSE,
        metadata_update_after = as.difftime(5, units = "mins")
      )
    )
    prop$set_solve_policy("upgrade")
    prop$solve()
    prop$show_solution()
    prop$download()
    prop$install_sysreqs()
    inst <- prop$install()

    # Drop packages that were kept in a different library. These are
    # recommended packages currently
    was_inst <- inst$package[inst$type == "installed"]
    drop <- setdiff(was_inst, dir(library))
    if (length(drop) > 0) {
      inst <- inst[!inst$package %in% drop, , drop = FALSE]
    }
    inst
  }

  keep_updated <- function(inst, mirror_pkgs) {
    # we installed these packages, now we update packages that are not in
    # the mirror, or we just built a newer version for them.
    # TODO: update for breaking changes in R as well
    to_add <- setdiff(inst$package, mirror_pkgs$package)
    mirrored <- intersect(inst$package, mirror_pkgs$package)
    inst_ver <- inst$version[match(mirrored, inst$package)]
    mirror_ver <- mirror_pkgs$version[match(mirrored, mirror_pkgs$package)]
    to_update <- mirrored[package_version(inst_ver) > mirror_ver]
    todo <- c(to_add, to_update)
    inst <- inst[match(todo, inst$package), , drop = FALSE]

    # get build numbers
    inst$buildnum <- rep(1L, nrow(inst))
    inst$buildnum[todo %in% mirrored] <-
      parse_build_number(mirror_pkgs$target[match(
        todo[todo %in% mirrored],
        mirror_pkgs$package
      )])
    inst
  }

  build_pkgs <- function(inst, library) {
    files <- rep(NA_character_, nrow(inst))
    for (i in seq_along(inst$package)) {
      if (
        length(inst$build_error[[i]]) &&
          !identical(inst$build_error[[i]], FALSE)
      ) {
        cli::cli_alert_warning(
          "Failed to build package {.pkg {inst$package[[i]]}}."
        )
      } else {
        proc <- cli::cli_process_start(
          "Building binary for package {.pkg {inst$package[[i]]}}."
        )
        files[i] <- pkg_build(
          inst$package[[i]],
          library = library,
          build_number = inst$buildnum[[i]]
        )
        cli::cli_process_done(proc)
      }
    }
    inst$built_path <- files
    inst
  }

  upload_releases <- function(inst, release_org) {
    # upload packages to releases
    for (i in seq_along(inst$package)) {
      slug <- paste0(release_org, "/", inst$package[[i]])
      ver <- inst$version[[i]]
      rels <- try(ghr$list(slug))
      if (inherits(rels, "try-error")) {
        Sys.sleep(5)
        rels <- ghr$list(slug)
      }
      if (!ver %in% rels$tag_name) {
        cli::cli_alert_info("Creating GH release {slug} {ver}.")
        ghr$create(slug, ver)
      }
      cli::cli_alert_info("Adding release asset for {slug} {ver}.")
      tryCatch(
        ghr$add_asset(slug, inst$built_path[i], ver),
        error = function(err) {
          cli::cli_alert_info("Try adding release asset again.")
          Sys.sleep(60)
          ghr$add_asset(slug, inst$built_path[i], ver)
        }
      )
      Sys.sleep(5)
    }
  }

  print_report <- function(inst, mirror_pkgs) {
    pkg <- format(inst$package)
    oldver <- ifelse(
      inst$package %in% mirror_pkgs$package,
      mirror_pkgs$version[match(inst$package, mirror_pkgs$package)],
      ""
    )

    inst$build_time <- inst$build_time %||% NA_real_
    inst$build_time <- as.double(inst$build_time)
    inst$build_time <- as.difftime(inst$build_time, units = "secs")
    cols <- data_frame(
      package = inst$package,
      "old" = oldver,
      "-" = "->",
      "new" = inst$version,
      "build" = paste0("b", inst$buildnum),
      "build time" = vcapply(inst$build_time, format_time$pretty_dt)
    )

    print_table(cols)
  }

  print_table <- function(cols) {
    cols <- as.list(cols)
    for (i in seq_along(cols)) {
      if (names(cols)[i] == "-") names(cols)[i] <- ""
      cols[[i]] <- format(c(names(cols)[i], "", cols[[i]]))
      if (names(cols)[i] != "") {
        cols[[i]][2] <- strrep("-", nchar(cols[[i]][1]))
      }
    }
    writeLines(do.call(paste, cols))
  }

  # -----------------------------------------------------------------------

  structure(
    list(
      .internal = environment(),

      update = ghrepo_update
    )
  )
})

# -------------------------------------------------------------------------

#' Update a CRAN-like repository of binary packages at GitHub
#'
#' These functions are currently experimental.
#'
#' @details
#'
#' ## Update a CRAN-like repository of binary packages at GitHub
#'
#' `ghrepo$update()` updates a binary package mirror.
#'
#' ### Usage
#' ```
#' ghrepo$update(
#'   repo,
#'   subdir,
#'   release_org = "cran",
#'   source_repo = "https://cran.r-project.org",
#'   packages = NULL
#' )
#' ```
#'
#' ### Arguments
#'
#' * `repo`: GitHub slug, e.g. `r-hub/repos`.
#' * `subdir`: subdirectory in the GitHub repository, where the R package
#'   metadata should be updated. It must exist in the repository.
#'   If it does not have `PACKAGES*` files, then they will be created.
#' * `release_org`: GitHub organization or user name where the packages
#'   will be published as releases.
#' * `source_repo`: A CRAN-like repository, where source packages are
#'   taken from.
#' * `packages`: A character vector of package names to add to the binary
#'   repository, in addition to updating the ones that are already there.
#'
# -------------------------------------------------------------------------
#'
#' @name ghrepo
#' @keywords internal
#' @export

ghrepo
