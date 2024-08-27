
# There is no easy way to update the database in yum/dnf/zypper.
# But FIXME. We'll need to solve this issue when (and if) we'll check the
# installed system packages, and only install/update the ones that are
# missing or out of date. For now this is OK, the first `install` command
# will update the DB (cache it is called I believe), and the rest will
# use it.

# TODO: get this data from the DB

sysreqs2_cmds <- utils::read.table(
   stringsAsFactors = FALSE, header = TRUE, textConnection("
   name                       os      distribution version  update_command      install_command                     query_command
   'Ubuntu Linux'             linux   ubuntu       *        'apt-get -y update' 'apt-get -y install'                dpkg-query
   'Debian Linux'             linux   debian       *        'apt-get -y update' 'apt-get -y install'                dpkg-query
   'CentOS Linux'             linux   centos       *         NA                  'yum install -y'                    rpm
   'Rocky Linux'              linux   rockylinux   *         NA                  'dnf install -y'                    rpm
   'Red Hat Enterprise Linux' linux   redhat       6         NA                  'yum install -y'                    rpm
   'Red Hat Enterprise Linux' linux   redhat       7         NA                  'yum install -y'                    rpm
   'Red Hat Enterprise Linux' linux   redhat       *         NA                  'dnf install -y'                    rpm
   'Fedora Linux'             linux   fedora       *         NA                  'dnf install -y'                    rpm
   'openSUSE Linux'           linux   opensuse     *         NA                  'zypper --non-interactive install'  rpm
   'SUSE Linux Enterprise'    linux   sle          *         NA                  'zypper --non-interactive install'  rpm
   'Alpine Linux'             linux   alpine       *         NA                  'apk add --no-cache'                apk
"))

find_sysreqs_platform <- function(sysreqs_platform = NULL) {
  sysreqs_platform <- sysreqs_platform %||% current_config()$get("sysreqs_platform")
  plt <- parse_sysreqs_platform(sysreqs_platform)
  idx <- which(
    sysreqs2_cmds$os == plt$os &
    sysreqs2_cmds$distribution == plt$distribution &
    sysreqs2_cmds$version %in% c("*", plt$version)
  )[1]
}

sysreqs2_command <- function(sysreqs_platform = NULL,
                             cmd = c("install_command", "update_command",
                                     "query_command")) {
  cmd <- match.arg(cmd)
  sel <- find_sysreqs_platform(sysreqs_platform)
  if (is.na(sel)) {
    throw(pkg_error(paste0(
      "Unknown OS. Don't know how to query or install system packages for ",
      sysreqs_platform,
      "."
    )))
  }

  sysreqs2_cmds[[cmd]][sel]
}

sysreqs2_resolve <- function(sysreqs, sysreqs_platform = NULL,
                             config = NULL, ...) {
  synchronize(sysreqs2_async_resolve(sysreqs, sysreqs_platform, config, ...))
}

sysreqs2_async_resolve <- function(sysreqs, sysreqs_platform, config, ...) {
  sysreqs; sysreqs_platform; config; list(...)

  config <- config %||% current_config()
  sysreqs_platform <- sysreqs_platform %||% config$get("sysreqs_platform")

  sysreqs2_async_update_metadata(config = config)$
    then(function() {
      sysreqs2_match(sysreqs, sysreqs_platform = sysreqs_platform, config = config, ...)
    })$
    then(function(recs) {
      sysreqs2_scripts(recs, sysreqs_platform)
    })
}

sysreqs2_scripts <- function(recs, sysreqs_platform, missing = FALSE) {
  plt <- parse_sysreqs_platform(sysreqs_platform)
  flatrecs <- unlist(recs, recursive = FALSE)
  upd <- sysreqs2_command(sysreqs_platform, "update")
  pre <- unlist(lapply(flatrecs, "[[", "pre_install"))
  post <- unlist(lapply(flatrecs, "[[", "post_install"))
  if (is.na(upd)) upd <- character()
  cmd <- sysreqs2_command(sysreqs_platform, "install")
  allpkgs <- unique(unlist(lapply(flatrecs, "[[", "packages")))
  misspkgs <- unique(unlist(lapply(flatrecs, function(x) {
    if ("packages_missing" %in% names(x)) {
      x$packages_missing
    } else {
      x$packages
    }
  })))
  pkgs <- if (missing) misspkgs else allpkgs
  ipkgs <- if (length(pkgs)) paste(pkgs, collapse = " ") else character()
  ipkgs <- if (length(ipkgs)) paste(cmd, ipkgs)
  # no need to update if nothing to do
  if (length(pre) + length(ipkgs) + length(post) == 0) upd <- character()
  res <- list(
    os = plt$os,
    distribution = plt$distribution,
    version = plt$version,
    url = NA_character_,
    pre_install = c(upd, pre),
    install_scripts = ipkgs,
    post_install = post,
    packages = allpkgs
  )
  if (missing) res$misspkgs <- misspkgs
  res
}

sysreqs2_git_repo <- function() {
  list(
    repo = Sys.getenv(
      "R_PKG_SYSREQS_GIT_REPO_URL",
      "https://github.com/r-hub/r-system-requirements.git"
    ),
    ref = Sys.getenv(
      "R_PKG_SYSREQS_GIT_REPO_REF",
      "HEAD"
    )
  )
}

sysreqs2_update_metadata <- function(path = NULL, config = NULL) {
  synchronize(sysreqs2_async_update_metadata(path = path, config = config))
}

# Increase this if the syntax breaks or if we need more / different files
# from the r-system-requirements repo.
sysreqs_db_version <- "1"

sysreqs2_async_update_metadata <- function(path = NULL, config = NULL) {
  config <- config %||% current_config()
  if (!config$get("sysreqs_db_update")) return(async_constant())

  path <- path %||% file.path(get_user_cache_dir()$root, "sysreqs")
  head_file <- file.path(path, "HEAD")
  ver_file <- file.path(path, "VERSION")

  if (file.exists(head_file) && file.exists(ver_file)) {
    mt <- file.mtime(head_file)
    ver <- readLines(ver_file)
    upd <- config$get("metadata_update_after")
    if (ver == sysreqs_db_version && !is.na(mt) && Sys.time() - mt < upd) {
      return(async_constant())
    }
  }

  upd <- function() {
    head <- if (file.exists(head_file)) readLines(head_file)[1] else ""
    repo <- sysreqs2_git_repo()
    async_git_list_refs_v1(repo$repo)$
      then(function(refs) {
        rem_head <- refs$refs$hash[refs$refs$ref == "HEAD"]
        if (rem_head == head) {
          # still update the time stamps
          Sys.setFileTime(head_file, Sys.time())
          return()
        }
        tmp <- paste0(path, "-new")
        async_git_download_repo(repo$repo, rem_head, tmp)$
          then(function() {
            unlink(path, recursive = TRUE, force = TRUE)
            file.rename(tmp, path)
            writeLines(rem_head, head_file)
            writeLines(sysreqs_db_version, ver_file)
          })
      })
  }

  timeout <- as.double(
    config$get("sysreqs_db_update_timeout"),
    units = "secs"
  )
  async_timeout(upd, timeout)$
    catch(error = function(e) {
      cli::cli_alert_warning(                                  # nocov start
        "Failed to update system requirement mappings,
         will use cached mappings.",
        wrap = TRUE
      )                                                        # nocov end
      invisible()
    })
}

sysreqs2_list_rules <- function(path = NULL) {
  if (is.null(path)) {
    cached_path <- file.path(get_user_cache_dir()$root, "sysreqs")
    head_file <- file.path(cached_path, "HEAD")
    ver_file <- file.path(cached_path, "VERSION")
    if (file.exists(ver_file) && readLines(ver_file) == sysreqs_db_version) {
      path <- cached_path
    } else {
      path <- system.file("sysreqs", package = "pkgdepends")
    }
  }
  rules <- dir(file.path(path, "rules"), pattern = "[.]json$", full.names = TRUE)
}

sysreqs2_match <- function(sysreqs, path = NULL, sysreqs_platform = NULL,
                           config = NULL) {

  rules <- sysreqs2_list_rules(path)

  result <- structure(
    vector(mode = "list", length(sysreqs)),
    names = names(sysreqs)
  )
  todo <- !is.na(sysreqs) & sysreqs != ""

  config <- config %||% current_config()
  plt <- parse_sysreqs_platform(sysreqs_platform %||% config$get("sysreqs_platform"))

  rsysreqs <- sysreqs[todo]
  for (r in rules) {
    rule <- jsonlite::fromJSON(r, simplifyVector = FALSE)
    pats <- unlist(rule$patterns)
    mch <- vapply(
      pats,
      grepl,
      logical(length(rsysreqs)),
      x = rsysreqs,
      ignore.case = TRUE
    )
    mch <- apply(rbind(mch), 1, any)
    if (!any(mch)) next
    for (dep in rule$dependencies) {
      appl <- FALSE
      for (const in dep$constraints) {
        if (identical(const$os, plt$os) &&
            identical(const$distribution, plt$distribution) &&
            (is.null(const$versions) || plt$version %in% const$versions)) {
          appl <- TRUE
          break
        }
      }
      if (!appl) next
      sysreq_name <- tools::file_path_sans_ext(basename(r))
      rec <- list(
        sysreq = sysreq_name,
        packages = unname(unlist(dep$packages)),
        pre_install = unname(unlist(dep$pre_install)),
        post_install = unname(unlist(dep$post_install))
      )
      for (idx in which(mch)) {
        result[todo][[idx]] <- c(result[todo][[idx]], list(rec))
      }
      break
    }
  }

  result
}

sysreqs_update_state <- function(sys, spkgs = NULL) {
  spkgs <- spkgs %||% sysreqs_list_system_packages()
  spkgs <- spkgs[grepl("^.i$", spkgs$status), ]
  allspkgs <- unique(unlist(c(spkgs$package, spkgs$provides)))
  for (i in seq_along(sys)) {
    elt <- sys[[i]]
    for (j in seq_along(elt)) {
      elt[[j]]$packages_missing <- setdiff(elt[[j]]$packages, allspkgs)
    }
    if (!is.null(elt)) sys[[i]] <- elt
  }
  sys
}
