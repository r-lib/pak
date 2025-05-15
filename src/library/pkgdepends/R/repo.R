repo <- local({
  # List packages in local repo -------------------------------------------

  repo_list <- function(..., path = ".") {
    pkgs <- suppressWarnings(
      pkgcache::parse_packages(file.path(path, "PACKAGES"))
    )
    if (nrow(pkgs) == 0) {
      pkgs$Package <- character()
      pkgs$Version <- character()
      pkgs$Depends <- character()
      pkgs$Imports <- character()
      pkgs$Suggests <- character()
      pkgs$Enhances <- character()
      pkgs$LinkingTo <- character()
      pkgs$License <- character()
      pkgs$File <- character()
      pkgs$DownloadURL <- character()
      pkgs$OS <- character()
      pkgs$Arch <- character()
      pkgs$Built <- character()
      pkgs$Filesize <- character()
      pkgs$SHA256 <- character()
      pkgs$RVersion <- character()
      pkgs$Platform <- character()
      pkgs$GraphicsAPIVersion <- character()
      pkgs$InternalsId <- character()
      pkgs$SystemRequirements <- character()
    }

    pkgs
  }

  # Delete package from local repo ----------------------------------------

  repo_delete <- function(package, ..., path = ".") {
    pkgs <- repo_list(path = path)
    idx <- find_in_data_frame(pkgs, package = package, ...)
    if (length(idx)) {
      pkgs <- pkgs[-idx, , drop = FALSE]
    }
    PACKAGES <- file.path(path, "PACKAGES")
    write_dcf(pkgs, PACKAGES)
  }

  # Add package to local repo ---------------------------------------------

  repo_add <- function(file, ..., path = ".", release_org = "cran") {
    pkgs <- repo_list(path = path)
    pkg_data <- get_package_data(file, release_org = release_org)
    pkgs <- rbind_expand(pkgs, pkg_data)
    PACKAGES <- file.path(path, "PACKAGES")
    write_dcf(pkgs, PACKAGES)
  }

  # Update package in local repo ------------------------------------------

  repo_update <- function(file, ..., path = ".", release_org = "cran") {
    pkgs <- repo_list(path = path)
    pkg_data <- get_package_data(file, release_org = release_org)

    idx <- which(
      pkgs$Package == pkg_data$Package &
        pkgs$RVersion == pkg_data$RVersion &
        (is.na(pkg_data$OS) | is.na(pkgs$OS) | pkgs$OS == pkg_data$OS) &
        (is.na(pkg_data$Arch) | is.na(pkgs$Arch) | pkgs$Arch == pkg_data$Arch)
    )
    if (length(idx)) {
      pkgs <- pkgs[-idx, , drop = FALSE]
    }

    pkgs <- rbind_expand(pkgs, pkg_data)
    PACKAGES <- file.path(path, "PACKAGES")
    write_dcf(pkgs, PACKAGES)
  }

  # List packages in repo on GH -------------------------------------------

  repo_list_gh <- function(repo, subdir) {
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    get_packages(repo, subdir, tmp)
    repo_list(path = tmp)
  }

  # Update packages in GH repo --------------------------------------------

  repo_update_gh <- function(repo, subdir, files, release_org = "cran") {
    files <- normalizePath(files)

    oldwd <- getwd()
    workdir <- tempfile()
    on.exit(
      {
        setwd(oldwd)
        unlink(workdir, recursive = TRUE)
      },
      add = TRUE
    )
    dir.create(workdir)
    setwd(workdir)

    git("clone", paste0("https://github.com/", repo))
    prepo <- parse_slug(repo)
    setwd(prepo$repo)
    setwd(subdir)

    git("config", "user.email", "csardi.gabor@gmail.com")
    git("config", "user.name", "Gabor Csardi")
    git("config", "credential.helper", "cache")
    gitcreds::gitcreds_approve(list(
      url = "https://github.com",
      username = "PersonalAccessToken",
      password = Sys.getenv("GITHUB_PAT")
    ))

    repeat {
      git("pull")
      for (file in files) {
        repo_update(file, release_org = release_org)
      }
      tryCatch(
        {
          git_push()
          break
        },
        error = function(err) {
          if (
            !grepl("(non-fast-forward|fetch first|cannot lock ref)", err$stderr)
          ) {
            stop(err)
          }
          git("reset", "HEAD^")
          git("checkout", "--", ".")
        }
      )
    }

    invisible()
  }

  # -----------------------------------------------------------------------
  # Internal functions

  get_desc <- function(path) {
    pkg <- sub("_.*$", "", basename(path))
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    untar(path, files = paste0(pkg, "/DESCRIPTION"), exdir = tmp)
    desc::desc(file.path(tmp, pkg))
  }

  get_package_data <- function(path, release_org = "cran") {
    desc <- get_desc(path)
    pkgname <- desc$get_field("Package")
    pkgver <- desc$get_field("Version")
    built <- desc$get_built()
    rminor <- get_minor_r_version(built$R)
    os <- canonize_os(built$Platform)
    arch <- canonize_arch(built$Platform)
    deps <- unname(desc$get("Depends"))
    pkg <- data_frame(
      Package = pkgname,
      Version = pkgver,
      Depends = paste0(
        if (!is.na(deps)) paste0(deps, ", "),
        "R (>= ",
        rminor,
        "), R (< ",
        rminor,
        ".99)"
      ),
      Imports = unname(desc$get("Imports")),
      Suggests = unname(desc$get("Suggests")),
      Enhances = unname(desc$get("Enhances")),
      LinkingTo = unname(desc$get("LinkingTo")),
      License = unname(desc$get("License")),
      File = basename(path),
      DownloadURL = paste0(
        "https://github.com/",
        release_org,
        "/",
        pkgname,
        "/releases/download/",
        pkgver,
        "/",
        basename(path)
      ),
      OS = os,
      Arch = arch,
      Built = desc$get_field("Built"),
      Filesize = file.size(path),
      SHA256 = cli::hash_file_sha256(path),
      RVersion = rminor,
      Platform = unname(desc$get("RemoteBuildPlatform")),
      GraphicsAPIVersion = unname(desc$get("GraphicsAPIVersion")),
      InternalsId = unname(desc$get("InternalsId")),
      SystemRequirements = unname(desc$get("SystemRequirements"))
    )

    pkg
  }

  write_dcf <- function(meta, PACKAGES, quiet = TRUE) {
    if (!quiet) cat("Writing ", PACKAGES, "\n")
    meta <- as.matrix(meta)
    write.dcf(meta, PACKAGES, width = 200)
    con <- gzfile(paste0(PACKAGES, ".gz"), "wt")
    write.dcf(meta, con, width = 200)
    close(con)
    saveRDS(meta, paste0(PACKAGES, ".rds"), compress = "xz", version = 2)
    invisible()
  }

  # case insensitive!

  find_in_data_frame <- function(df, ..., .list = NULL) {
    cols <- drop_nulls(c(list(...), .list))
    idx <- seq_len(nrow(df))
    names(df) <- tolower(names(df))
    for (i in seq_along(cols)) {
      if (length(idx) == 0) break
      n <- tolower(names(cols)[i])
      idx <- idx[df[[n]][idx] %in% cols[[i]]]
    }

    idx
  }

  canonize_arch <- function(platform) {
    if (platform == "") return(NA_character_)
    arch <- strsplit(platform, "-", fixed = TRUE)[[1]][1]
    c("aarch64" = "arm64", "x86_64" = "amd64", "s390x" = "s390x")[[arch]]
  }

  canonize_os <- function(platform) {
    if (platform == "") return(NA_character_)
    os <- strsplit(platform, "-", fixed = TRUE)[[1]][3]
    if (substr(os, 1, 6) == "darwin") os <- "darwin"
    if (substr(os, 1, 5) == "mingw") os <- "windows"
    if (substr(os, 1, 7) == "solaris") os <- "solaris"
    os
  }

  git <- function(
    ...,
    echo_cmd = TRUE,
    echo = TRUE,
    dry_run = FALSE,
    stderr_to_stdout = FALSE
  ) {
    if (dry_run) {
      cat("git", c(...), "\n")
    } else {
      processx::run(
        "git",
        c(...),
        echo_cmd = echo_cmd,
        echo = echo,
        stderr_to_stdout = stderr_to_stdout
      )
    }
  }

  git_push <- function() {
    git("add", "-A", ".")
    git("commit", "-m", "Update repository")
    git("push", "--porcelain", "origin", stderr_to_stdout = TRUE)
  }

  repo_columns <- function() {
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    file.create(file.path(tmp, "PACKAGES"))
    paste0("* `", colnames(repo_list(path = tmp)), "`", collapse = "\n")
  }

  get_packages <- function(repo, subdir, target) {
    url <- paste0("https://github.com/", repo)
    files <- git_list_files(url)
    wh <- which(files$files$path == paste0(subdir, "/PACKAGES.gz"))[1]
    if (is.na(wh)) {
      wh <- which(files$files$path == paste0(subdir, "/PACKAGES"))[1]
    }
    if (is.na(wh)) {
      wh <- which(files$files$path == paste0(subdir, "/PACKAGES.rds"))[1]
    }
    if (is.na(wh)) {
      throw(pkg_error(
        "Cannot file `PACKAGES*` in {.val {subdir}} at repo {.val {repo}}."
      ))
    }
    hash <- files$files$hash[wh]
    git_download_file(url, hash, file.path(target, "PACKAGES"))
  }

  parse_slug <- function(slug) {
    parts <- strsplit(slug, "/", fixed = TRUE)[[1]]
    list(user = parts[1], owner = parts[1], repo = parts[2])
  }

  # -----------------------------------------------------------------------
  # Exported functions

  structure(
    list(
      .internal = environment(),
      add = repo_add,
      delete = repo_delete,
      list = repo_list,
      list_gh = repo_list_gh,
      update = repo_update,
      update_gh = repo_update_gh
    ),
    class = c("standalone_repo", "standalone")
  )
})

# -------------------------------------------------------------------------

#' Query and manipulate CRAN-like repositories
#'
#' These functions are currently experimental.
#'
#' @details
#'
#' ## List packages in a repository
#'
#' `repo$list()` lists packages in a repository. It reads the `PACKAGES`
#' file containing the repository metadata.
#'
#' ### Usage
#' ```
#' repo_list(..., path = ".")
#' ```
#'
#' ### Arguments
#'
#' * `...`: ignored currently.
#' * `path`: path to repository. Must contain a `PACKAGES` file.
#'
#' ### Value
#'
#' Data frame of package data, a data frame with at least the following
#' columns, possibly more if there are other entries in the metadata:
#' `r repo$.internal$repo_columns()`
#'
# -------------------------------------------------------------------------
#'
#' ## Delete packages from repository metadata
#'
#' `repo$delete()` deletes matching packages from the repository
#' metadata.
#'
#' ### Description
#'
#' All matching packages will be removed.
#' It does not delete the files themselves.
#'
#' ### Usage
#' ```
#' repo$delete(package, ..., path = ".")
#' ```
#'
#' ### Arguments
#'
#' * `package`: package name.
#' * `...`: other fields to match, they must be named. Matching is
#'   case insensitive.
#' * `path`: path to repository. Must contain a `PACKAGES` file.
#'
# -------------------------------------------------------------------------
#'
#' ## Add a package to a repository
#'
#' ### Description
#'
#' It does not check if any version of the package is already
#' in the repository. If you want to _update_ a package, use
#' `repo$update()`.
#'
#' ### Usage
#' ```
#' repo$add(file, ..., path = ".")
#' ```
#'
#' ### Arguments
#'
#' * `file Package file.
#' * `... Ignored currently.
#' * `path Path to repository. Must contain a `PACKAGES` file.
#'
# -------------------------------------------------------------------------
#'
#' ## Update a package in a repository
#'
#' ### Description
#'
#' Previous version of the same package are removed. In particular,
#' it removes packages with matching:
#' - package name (`Package` field),
#' - R version (`Rversion` field),
#' - same OS (`OS` field), or no `OS` field,
#' - same architecture (`Arch` field), or not `Arch` field.
#'
#' ### Usage
#' ```
#' repo$update(file, ..., path = ".")
#' ```
#'
#' ### Arguments
#'
#' * `file Package file.
#' * `... Ignored currently.
#' * `path Path to repository. Must contain a `PACKAGES` file.
#'
# -------------------------------------------------------------------------
#'
#' ## Update a file in a package metadata, stored on GitHub
#'
#' ### Description
#'
#' 1. Clones the GitHub repository.
#' 2. Calls `repo_update()` with `file`, in the `subdir` directory.
#' 3. Adds and commits changes.
#' 4. Pushes the git repository to GitHub.
#' If the push fails, then it resets the git repository, pulls it from
#' GitHub and tries the update process again, until the push succeeds.
#'
#' This function needs command line git installed.
#'
#' It sets up a `cache` credential helper, so the `git push` works
#' without interaction with the user.
#'
#' ### Usage
#' ```
#' repo$update_gh(repo, subdir, files)
#' ```
#'
#' ### Arguments
#'
#' * `repo`: GitHub slug, e.g. `r-hub/repos`.
#' * `subdir`: subdirectory in the GitHub repository, where the R package
#'   metadata should be updated. It must exist in the repository.
#'   If it does not have `PACKAGES*` files, then they will be created.
#' * `files`: package files to add. The files will _not_ be added to the
#'   repository, only to the metadata. They should be in the repository
#'   already.
#'
# -------------------------------------------------------------------------
#'
#' @name repo
#' @keywords internal
#' @export

repo
