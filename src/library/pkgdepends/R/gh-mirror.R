ghmirror <- local({
  ghmirror_update <- function(pkg, source_repo = "https://cran.r-project.org") {
    cran_versions <- get_package_versions(pkg, source_repo)
    atgh_versions <- get_github_versions(pkg)

    missing <- setdiff(cran_versions$version, atgh_versions$version)

    add_missing_versions(
      pkg,
      missing,
      new_pkg = length(atgh_versions) == 0,
      repo = source_repo
    )

    invisible(missing)
  }

  # -----------------------------------------------------------------------
  # Internal functions

  cran_versions <- NULL

  get_all_cran_versions <- function(
    source_repo = "https://cran.r-project.org",
    forget = FALSE
  ) {
    if (!is.null(cran_versions) && !forget) {
      return(cran_versions)
    }

    r_archive <- pkgcache::cran_archive_cache$new(
      cran_mirror = source_repo,
      update_after = as.difftime(30, units = "mins")
    )
    r_archive$check_update()
    r_archive_pkgs <- r_archive$list()

    r_source <- pkgcache::cranlike_metadata_cache$new(
      platforms = "source",
      bioc = FALSE,
      cran_mirror = source_repo,
      repos = NULL,
      update_after = as.difftime(30, units = "mins")
    )
    r_source$check_update()
    r_source_pkgs <- r_source$list()

    # we'll just drop versions with invalid version numbers, these are
    # old packages only
    badver <- is.na(package_version(r_archive_pkgs$version, strict = FALSE))
    r_archive_pkgs <- r_archive_pkgs[!badver, , drop = FALSE]

    res <- data_frame(
      package = c(r_archive_pkgs$package, r_source_pkgs$package),
      version = c(r_archive_pkgs$version, r_source_pkgs$version)
    )

    res <- res[order(tolower(res$package), package_version(res$version)), ]
    cran_versions <<- res

    res
  }

  get_package_versions <- function(
    pkg,
    source_repo = "https://cran.r-project.org",
    forget = FALSE
  ) {
    all <- get_all_cran_versions(source_repo, forget = forget)
    res <- all[all$package == pkg, , drop = FALSE]
    res
  }

  get_github_versions <- function(pkg) {
    github_versions <- tryCatch(
      gh::gh(
        "/repos/:owner/:repo/tags",
        owner = "cran",
        repo = pkg,
        .limit = Inf,
        .token = get_gh_token()
      ),
      error = function(e) list(),
      warning = function(e) list()
    )
    github_versions <- vapply(github_versions, "[[", FUN.VALUE = "", "name")
    github_versions <- grep("R-", github_versions, value = TRUE, invert = TRUE)

    data_frame(
      version = rev(github_versions)
    )
  }

  get_gh_token <- function() {
    token <- Sys.getenv("CRANATGH_GITHUB_TOKEN", NA_character_)
    if (is.na(token)) token <- Sys.getenv("GITHUB_PAT", NA_character_)
    if (is.na(token)) token <- Sys.getenv("GITHUB_TOKEN", NA_character_)
    token
  }

  add_missing_versions <- function(pkg, versions, new_pkg, repo) {
    if (length(versions) == 0) return()

    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    change_to_cranatgh_home()

    if (new_pkg) create_git_repo(pkg)

    if (!file.exists(pkg)) clone_git_repo(pkg)

    set_git_user(pkg)

    for (ver in versions) {
      metadata <- add_missing_version(pkg, ver, repo)
    }

    desc <- make_description(metadata)

    if (new_pkg) create_gh_repo(pkg, desc)

    push_to_github(pkg)

    if (!new_pkg) update_description(pkg, desc)

    invisible()
  }

  make_description <- function(pkg) {
    if (is.character(pkg)) {
      pkg <- tryCatch(
        pkgsearch::cran_package(pkg),
        error = function(e) NULL
      )
    } else if (inherits(pkg, "description")) {
      pkg <- list(
        Package = pkg$get("Package"),
        Title = pkg$get("Title"),
        URL = pkg$get("URL"),
        BugReports = pkg$get("BugReports")
      )
    } else {
      stop("'pkg' must be a character scalar or a 'description' object")
    }

    dsc <- paste(
      sep = "  ",
      dont_break(
        ":exclamation: This is a read-only mirror of the CRAN R package repository."
      ),
      dont_break(
        pkg$Package,
        " \u2014 ",
        pkg$Title,
        nullna_or(pkg$URL, paste0(". Homepage: ", pkg$URL))
      ),
      nullna_or(
        pkg$BugReports,
        dont_break("Report bugs for this package: ", pkg$BugReports)
      )
    )

    # Limit is 350 characters, but be conservative
    if (nchar(dsc) > 320) dsc <- paste0(substr(dsc, 1, 320), " ...")

    dsc
  }

  add_missing_version <- function(package, version, repo) {
    proc <- cli::cli_process_start("Adding {.pkg {package}} {version}")

    ## Rename the .git directory. We'll need it later
    file.rename(file.path(package, ".git"), "dot-git")

    ## Remove everything from the old version
    unlink(package, recursive = TRUE, force = TRUE)
    dir.create(package)

    ## Put the new version in place
    tar_file <- get_package_tarball(package, version, repo)
    untar(tar_file)
    unlink(tar_file)

    ## Put back the .git directory
    ## The unlink is for the occasional case when there is already
    ## a .git directory in the package. This is junk anyway, and it
    ## should not be there
    unlink(file.path(package, ".git"))
    file.rename("dot-git", file.path(package, ".git"))

    setwd(package)
    on.exit(setwd(".."), add = TRUE)

    ## To prevent an error like "detected dubious ownership in repository"
    if (.Platform$OS.type == "unix") {
      system("chown -R `id -un`:`id -gn` .")
    }

    ## Add all the new files
    git("status")
    git("add", "-A", ".")
    git("status")

    ## Package information from DESCRIPTION
    metadata <- desc::description$new()
    maint <- metadata$get_maintainer()
    auth <- metadata$get("Author")

    ## Commit the new version
    date <- format_iso_8601(metadata$get("Date/Publication"))
    git(
      env = c("GIT_COMMITTER_DATE" = date),
      "commit",
      "--allow-empty",
      "-m",
      paste0("version ", version),
      "--date",
      date,
      "--author",
      fix_maintainer(maint, auth)
    )

    git("tag", version)

    cli::cli_process_done(proc)

    metadata
  }

  change_to_cranatgh_home <- function() {
    home <- default_tree_location()
    if (is.na(home)) dir.create(home <- tempfile())
    setwd(home)
  }

  default_tree_location <- function() {
    Sys.getenv("CRANATGH_TREES", NA_character_)
  }

  create_git_repo <- function(path) {
    dir.create(path)
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(path)
    cli::cli_alert_info("Creating git repo in {.path {path}}")
    git("init", ".")
  }

  set_git_user <- function(path, user = NULL, email = NULL) {
    user <- user %||% default_cranatgh_user()
    email <- email %||% default_cranatgh_email()

    withr::with_dir(path, {
      git("config", "--local", "user.name", user)
      git("config", "--local", "user.email", email)
    })
  }

  default_cranatgh_user <- function() {
    Sys.getenv("CRANATGH_USER", "cran-robot")
  }

  default_cranatgh_email <- function() {
    Sys.getenv("CRANATGH_EMAIL", "csardi.gabor+cran@gmail.com")
  }

  default_cranatgh_org <- function() {
    Sys.getenv("CRANATGH_ORG", "cran")
  }

  clone_git_repo <- function(pkg) {
    url <- get_clone_url(pkg)
    proc <- cli::cli_process_start(
      "Cloning GitHub repo from {.url {safe_url(url)}}"
    )
    git("clone", url)
    cli::cli_process_done(proc)
  }

  safe_url <- function(url) {
    sub("://[-:a-z0-9]+@", "://<token>@", url)
  }

  get_clone_url <- function(package) {
    owner <- default_cranatgh_org()
    token <- get_gh_token()
    token <- if (is.na(token)) "" else paste0(token, "@")

    sprintf("https://%sgithub.com/%s/%s.git", token, owner, package)
  }

  git <- function(..., env = character(), timeout = 60 * 60) {
    processx::run(
      "git",
      args = unlist(list(...)),
      env = c(Sys.getenv(), env)
    )
  }

  get_package_tarball <- function(package, version, repo) {
    urls <- package_urls(package, version, repo)
    ok <- FALSE
    for (url in urls) {
      dest_file <- basename(url)
      tryCatch(
        {
          curl::curl_download(url, dest_file)
          ok <- TRUE
          break
        },
        error = function(err) err
      )
    }

    if (!ok) stop("Cannot download package ", package)
    dest_file
  }

  package_urls <- function(package, version, repo) {
    # work around some mistakes, file names do not match the version number
    if (package == "HTML" && version == "0.4") {
      "https://cran.rstudio.com/src/contrib/Archive/HTML/HTML_0.4-1.tar.gz"
    } else if (package == "timeslab" && version == "1.0") {
      "https://cran.r-project.org/src/contrib/Archive/timeslab/timeslab_1.0-1.tar.gz"
    } else {
      c(
        sprintf("%s/src/contrib/%s_%s.tar.gz", repo, package, version),
        sprintf(
          "%s/src/contrib/Archive/%s/%s_%s.tar.gz",
          repo,
          package,
          package,
          version
        )
      )
    }
  }

  fix_maintainer <- function(maint, auth) {
    if (is.na(maint)) maint <- auth
    maint <- sub("\\s*<", " <", maint)

    ## ': end of single quote
    ## ": start of double quote
    ## ': single quote (within double quotes)
    ## ": end of double quote
    ## ': start of single quote for the rest of the string
    maint <- gsub("'", paste0("'", '"', "'", '"', "'"), maint)

    if (is.na(maint)) maint <- "??? <???@???>"
    if (!grepl("<.*>", maint)) maint <- paste0(maint, " <???@???>")
    if (toupper(maint) == "ORPHANED") maint <- "ORPHANED <cran@R-project.org>"
    maint
  }

  dont_break <- function(...) {
    gsub("\\s+", "\u00a0", paste0(...))
  }

  nullna_or <- function(x, expr) {
    if (is.null(x) || (length(x) == 1 && is.na(x))) "" else expr
  }

  push_to_github <- function(package, forced_push = FALSE) {
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(package)

    add_gh_remote(package)

    proc <- cli::cli_process_start("Pushing {.pkg {package}} to GitHub")
    current <- sub("* ", "", fixed = TRUE, system("git branch", intern = TRUE))
    git("push", "--tags", if (forced_push) "-f", "-u", "origin", current)
    cli::cli_process_done(proc)
  }

  add_gh_remote <- function(package) {
    current <- git("remote")$stdout
    if (!grepl("\\borigin\\b", current)) {
      git(
        "remote",
        "add",
        "origin",
        get_clone_url(package)
      )
    }
  }

  update_description <- function(
    package,
    description = make_description(package)
  ) {
    description <- clean_description(description)

    proc <- cli::cli_process_start(
      "Updating repo description for {.pkg {package}}"
    )
    gh::gh(
      "PATCH /repos/:owner/:repo",
      owner = default_cranatgh_org(),
      repo = package,
      name = package,
      description = description,
      homepage = "",
      .token = get_gh_token()
    )
    cli::cli_process_done(proc)
  }

  clean_description <- function(description) {
    description <- unname(description)
    description <- gsub("\n", " ", description)
    description
  }

  structure(
    list(
      .internal = environment(),

      update = ghmirror_update
    )
  )
})
