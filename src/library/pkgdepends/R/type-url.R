# -----------------------------------------------------------------------
# API

parse_remote_url <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, type_url_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "url"
  parsed_specs$hash <- vcapply(specs, url_hash)
  parsed_specs$package[parsed_specs$package == ""] <- NA_character_

  # Special case downloads from GH
  parsed_gh_specs <- re_match(specs, type_github_download_url_rx())
  parsed_specs$package <- ifelse(
    is.na(parsed_specs$package) & !is.na(parsed_gh_specs$repo),
    parsed_gh_specs$repo,
    parsed_specs$package
  )

  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i, ])
  )
}

resolve_remote_url <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  remote
  direct
  config
  cache
  dependencies
  list(...)
  nocache <- is_true_param(remote$params, "nocache")
  type_url_resolve(
    remote,
    cache,
    config,
    direct = direct,
    dependencies = dependencies[[2 - direct]],
    nocache = nocache
  )$then(function(x) x$resolution)
}

download_remote_url <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  resolution
  target
  target_tree
  config
  cache
  which
  on_progress

  remote <- resolution$remote[[1]]
  packaged <- resolution$metadata[[1]][["RemotePackaged"]]
  tmpd <- type_url_tempdir(remote, config)

  rimraf(c(target, target_tree))

  status <- NULL

  # If we are installing an install plan, then it might not be there
  nocache <- is_true_param(resolution$params[[1]], "nocache")
  if (!file.exists(tmpd$ok)) {
    dx <- type_url_resolve(remote, cache, config, nocache)$then(function(x) {
      newres <- x$resolution
      status <<- x$data$status
      res_etag <- resolution$metadata[[1]][["RemoteEtag"]]
      new_etag <- newres$metadata[["RemoteEtag"]]
      if (res_etag != new_etag) {
        warning("Package file at `", remote$url, "` has changed") # nocov
      }
    })
  } else {
    status <- "Had"
    dx <- async_constant(0)
  }

  dx$then(function() {
    if (packaged == "TRUE") {
      mkdirp(dirname(target))
      if (!file.copy(tmpd$archive, target, overwrite = TRUE)) {
        throw(pkg_error(
          "Failed to copy package from {.path {tmpd$archive}} to
             {.path{ target}}.",
          i = "It was downloaded from {.url {remote$url}}."
        ))
      }
    } else {
      mkdirp(target_tree)
      if (!file.copy(tmpd$extract, target_tree, recursive = TRUE)) {
        throw(pkg_error(
          # nocov start
          "Failed to copy package from {.path {tmpd$archive}} to
             {.path{ target}}.",
          i = "It was downloaded from {.url {remote$url}}."
        )) # nocov end
      }
    }
  })$then(function() status)
}

satisfy_remote_url <- function(resolution, candidate, config, ...) {
  ## 1. package name must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 2. installed ref is good, if it has the same etag
  if (candidate$type == "installed") {
    want_reinst <- is_true_param(resolution$params[[1]], "reinstall")
    if (want_reinst) {
      return(structure(FALSE, reason = "Re-install requested"))
    }
    t1 <- tryCatch(candidate$extra[[1]]$remoteetag, error = function(e) "")
    t2 <- resolution$metadata[[1]][["RemoteEtag"]]
    ok <- is_string(t1) && is_string(t2) && t1 == t2
    if (!ok) {
      return(structure(FALSE, reason = "Installed URL etag mismatch"))
    } else {
      return(TRUE)
    }
  }

  ## 3. same url is good
  if (candidate$type == "url") {
    if (resolution$ref == candidate$ref) return(TRUE)
    return(structure(FALSE, reason = "URL mismatch"))
  }

  structure(FALSE, reason = "Repo type mismatch")
}

installedok_remote_url <- function(installed, solution, config, ...) {
  identical(installed$package, solution$package) &&
    identical(installed$version, solution$version) &&
    identical(installed[["remotetype"]], "url") &&
    identical(installed[["remoteetag"]], solution$metadata[[1]][["RemoteEtag"]])
}

# -----------------------------------------------------------------------
# Internal functions

type_url_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>",
    package_name_rx(),
    ")=)?",
    "(?:url::)",
    "(?<url>.*)",
    "$"
  )
}

url_hash <- function(x) {
  cli::hash_obj_md5(x)
}

# E.g. url::https://github.com/r-lib/tidyselect/archive/main.tar.gz

type_github_download_url_rx <- function() {
  paste0(
    "^",
    "(?:url::)",
    "https://github.com/",
    github_username_rx(),
    "/",
    github_repo_rx(),
    "/archive/",
    "(?<branch>[^/.]+)",
    "[.]tar[.]gz$"
  )
}

type_url_tempdir <- function(remote, config) {
  base <- basename(remote$url)
  filename <- paste0(substr(remote$hash, 1, 7), "-", basename(remote$url))
  archive <- file.path(config$get("cache_dir"), filename)
  extract <- file.path(config$get("cache_dir"), paste0(filename, "-t"))
  ok <- file.path(config$get("cache_dir"), paste0(filename, "-ok"))
  list(
    archive = archive,
    extract = extract,
    cachepath = file.path("archives", filename),
    ok = ok
  )
}

type_url_download_and_extract <- function(
  remote,
  cache,
  config,
  tmpd,
  nocache
) {
  id <- NULL
  tmpd <- tmpd
  async_constant(
    if (nocache) {
      mkdirp(dirname(tmpd$archive))
      download_one_of(remote$url, tmpd$archive)$then(function(dl) {
        attr(dl, "action") <- "Got"
        dl
      })
    } else {
      cache$package$async_update_or_add(
        tmpd$archive,
        remote$url,
        path = tmpd$cachepath,
        http_headers = default_download_headers(remote$url)
      )
    }
  )$then(function(dl) {
    tmpd$status <<- attr(dl, "action")
    tmpd$etag <<- if (is.na(dl$etag)) substr(dl$sha256, 1, 16) else dl$etag
    tmpd$id <<- cli::hash_obj_md5(tmpd$etag)
    rimraf(c(tmpd$extract, tmpd$ok))
    mkdirp(tmpd$extract)
    run_uncompress_process(tmpd$archive, tmpd$extract)
  })$then(function(status) {
    tmpd$pkgdir <<- get_pkg_dir_from_archive_dir(tmpd$extract)
    cat("ok\n", file = tmpd$ok)
    tmpd
  })
}

type_url_resolve <- function(
  remote,
  cache,
  config,
  direct = FALSE,
  dependencies = character(),
  nocache = FALSE
) {
  tmpd <- type_url_tempdir(remote, config)
  xdirs <- NULL
  type_url_download_and_extract(remote, cache, config, tmpd, nocache)$then(
    function(dirs) {
      xdirs <<- dirs
      resolve_from_description(
        path = dirs$pkgdir,
        sources = remote$url,
        remote = remote,
        direct = direct,
        config = config,
        cache = cache,
        dependencies = dependencies
      )
    }
  )$then(function(x) {
    x$target <- file.path(
      "src/contrib",
      paste0(x$package, "_", x$version, "-", substr(xdirs$id, 1, 10), ".tar.gz")
    )
    x$metadata[["RemoteEtag"]] <- xdirs$etag
    x$extra[[1]][["resolve_download_status"]] <- tmpd$status
    x$metadata[["RemotePackaged"]] <-
      x$extra[[1]]$description$has_fields("Packaged")
    x$params[[1]] <- remote$params
    list(resolution = x, data = xdirs)
  })
}


get_pkg_dir_from_archive_dir <- function(x) {
  top <- dir(x)
  if (length(top) != 1) {
    throw(pkg_error(
      "Package archive at {.path {x}} should contain exactly one directory.",
      i = "It has {cli::no(length(top))} file{?s}/director{?y/ies}:
       {.path {top}}"
    ))
  }
  file.path(x, top)
}
