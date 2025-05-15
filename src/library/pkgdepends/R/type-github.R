### -----------------------------------------------------------------------
### API

parse_remote_github <- function(specs, config, ...) {
  pds <- re_match(specs, github_rx())
  if (any(unk <- is.na(pds$.match))) {
    pds[unk] <- re_match(specs[unk], github_url_rx())
    pds[unk, "subdir"] <- ""
  }

  pds$ref <- pds$.text
  cn <- setdiff(colnames(pds), c(".match", ".text"))
  pds <- pds[, cn]
  pds$type <- "github"
  pds$package <- ifelse(nzchar(pds$package), pds$package, pds$repo)
  lapply(
    seq_len(nrow(pds)),
    function(i) as.list(pds[i, ])
  )
}

resolve_remote_github <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  force(direct)
  force(dependencies)
  ## Get the DESCRIPTION data, and the SHA we need
  type_github_get_data(remote)$then(function(resp) {
    data <- list(
      desc = resp$description,
      sha = resp$sha,
      remote = remote,
      direct = direct,
      dependencies = dependencies[[2 - direct]]
    )
    type_github_make_resolution(data)
  })
}

download_remote_github <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  ## A GitHub package needs to be built, from the downloaded repo
  ## If we are downloading a solution, then we skip building the vignettes,
  ## because these will be built later by pkginstall.
  ##
  ## We cache both the downloaded repo snapshot and the built package in
  ## the package cache. So this is how we go:
  ##
  ## 1. If there is a locally built bianry in the cache (including
  ##    vignettes if they are requested), then we use that.
  ## 2. If there is a built package in the cache (including vignettes
  ##    if they are needed), then we use that.
  ## 3. If there is a repo snapshot in the cache, we build an R package
  ##    from it. (Add also add it to the cache.)
  ## 4. Otherwise we download the repo, add it to the cache, build the
  ##    R package, and add that to the cache as well.

  package <- resolution$package
  sha <- resolution$extra[[1]][["remotesha"]] %||% NA_character_
  need_vignettes <- which == "resolution"
  nocache <- is_true_param(resolution$params[[1]], "nocache")
  source <- is_true_param(resolution$params[[1]], "source")

  # in case there is a leftover package/tree here
  unlink(c(target, target_tree), recursive = TRUE)

  ## 1. Check for a binary package

  if (!nocache && !source) {
    ptfm <- current_r_platform()
    hit <- cache$package$copy_to(
      target,
      package = package,
      sha256 = sha,
      built = TRUE,
      platform = ptfm,
      rversion = current_r_version(),
      .list = c(if (need_vignettes) c(vignettes = TRUE))
    )

    if (nrow(hit)) {
      return(paste("Had", ptfm)) # TODO: untested currently
    }
  }

  ## 2. Check if we have a built package in the cache. We do not check the
  ## ref or the type, so the package could have been built from a local
  ## ref or from another repo. As long as the sha is the same, we are
  ## fine. If we don't require vignetted, then a package with or without
  ## vignettes is fine.

  if (!nocache) {
    hit <- cache$package$copy_to(
      target,
      package = package,
      sha256 = sha,
      built = TRUE,
      .list = c(if (need_vignettes) c(vignettes = TRUE))
    )
    if (nrow(hit)) {
      "!DEBUG found GH `resolution$ref`@`sha` in the cache"
      return("Had")
    }
  }

  ## 3. Check if we have a repo snapshot in the cache.

  rel_target <- resolution$target
  if (!nocache) {
    subdir <- resolution$remote[[1]]$subdir
    hit <- cache$package$copy_to(
      target_tree,
      package = package,
      sha256 = sha,
      built = FALSE
    )
    if (nrow(hit)) {
      "!DEBUG found GH zip for `resolution$ref`@`sha` in the cache"
      return("Had")
    }
  }

  ## 4. Need to download the repo

  "!DEBUG Need to download GH package `resolution$ref`@`sha`"
  urls <- resolution$sources[[1]]
  rel_zip <- paste0(rel_target, "-t")
  type_github_download_repo(
    urls,
    target_tree,
    rel_zip,
    sha,
    package,
    cache,
    on_progress,
    nocache
  )$then(function() {
    "!DEBUG Building package `resolution$package`"
    return("Got")
  })
}

type_github_download_repo <- function(
  urls,
  repo_zip,
  rel_zip,
  sha,
  package,
  cache,
  on_progress,
  nocache
) {
  urls
  repo_zip
  sha
  package
  cache
  on_progress
  nocache
  ## TODO: progress
  headers <- type_github_get_headers()
  download_file(
    urls,
    repo_zip,
    on_progress = on_progress,
    headers = headers
  )$then(function() {
    if (!nocache) {
      cache$package$add(
        repo_zip,
        rel_zip,
        package = package,
        sha = sha,
        built = FALSE
      )
    }
    "Got"
  })
}

## ----------------------------------------------------------------------

satisfy_remote_github <- function(resolution, candidate, config, ...) {
  ## 1. package name must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 1. installed ref is good, if it has the same sha
  if (candidate$type == "installed") {
    want_reinst <- is_true_param(resolution$params[[1]], "reinstall")
    if (want_reinst) {
      return(structure(FALSE, reason = "Re-install requested"))
    }
    sha1 <- candidate$extra[[1]][["remotesha"]] %||% NA_character_
    sha2 <- resolution$extra[[1]][["remotesha"]] %||% NA_character_
    ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
    if (!ok) {
      return(structure(FALSE, reason = "Installed package sha mismatch"))
    } else {
      return(TRUE)
    }
  }

  ## 2. local packages satisfy a GH remote
  ## See https://github.com/r-lib/pkgdepends/issues/229
  if (candidate$type == "local") {
    return(TRUE)
  }

  ## 3. other refs are also good, as long as they have the same sha
  sha1 <- (if (is.list(candidate$extra[[1]]))
    candidate$extra[[1]][["remotesha"]]) %||%
    NA_character_
  sha2 <- resolution$extra[[1]][["remotesha"]] %||% NA_character_
  ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
  if (!ok) {
    return(structure(FALSE, reason = "Candidate package sha mismatch"))
  } else {
    return(TRUE)
  }
}

installedok_remote_github <- function(installed, solution, config, ...) {
  identical(installed$package, solution$package) &&
    identical(installed$version, solution$version) &&
    identical(installed[["remotesha"]], solution$metadata[[1]][["RemoteSha"]])
}

## ----------------------------------------------------------------------
## Internal functions

type_github_builtin_token <- function() {
  pats <- c(
    paste0("3687d8b", "b0556b7c3", "72ba1681d", "e5e689b", "3ec61279"),
    paste0("8ffecf5", "13a136f3d", "23bfe46c4", "2d67b3c", "966baf7b")
  )
  once_per_session(cli::cli_alert_warning(c(
    "Using bundled GitHub PAT. ",
    "Please add your own PAT using {.code gitcreds::gitcreds_set()}."
  )))
  sample(pats, 1)
}

type_github_get_headers <- function() {
  headers <- c(
    "Accept" = "application/vnd.github.v3+json",
    "Content-Type" = "application/json; charset=utf-8"
  )

  token <- NA_character_
  if (Sys.getenv("CI", "") != "") {
    token <- Sys.getenv("CI_GITHUB_TOKEN", NA_character_)
  }
  if (is.na(token))
    token <- tryCatch(
      gitcreds_get()$password,
      error = function(e) NA_character_
    )
  if (is.na(token)) token <- type_github_builtin_token()
  headers <- c(headers, c("Authorization" = paste("token", token)))

  headers <- c(headers, c("User-Agent" = "r-lib/pak"))
  headers
}

type_github_get_data <- function(rem) {
  dx <- if (!is.null(rem$pull) && rem$pull != "") {
    type_github_get_data_pull(rem)
  } else if (!is.null(rem$release) && rem$release != "") {
    type_github_get_data_release(rem)
  } else {
    type_github_get_data_ref(rem)
  }

  dx$then(function(data) {
    chain_error(
      dsc <- desc::desc(text = data$desc),
      new_github_baddesc_error(rem, call)
    )
    list(sha = data$sha, description = dsc)
  })
}

type_github_get_data_ref <- function(rem) {
  user <- rem$username
  repo <- rem$repo
  ref <- rem$commitish %|z|% "HEAD"
  subdir <- rem$subdir %&z&% paste0(utils::URLencode(rem$subdir), "/")

  query <- sprintf(
    "{
    repository(owner: \"%s\", name: \"%s\") {
      description: object(expression: \"%s:%sDESCRIPTION\") {
        ... on Blob {
          isBinary
          text
        }
      }
      sha: object(expression: \"%s\") {
        oid
      }
    }
  }",
    user,
    repo,
    ref,
    subdir,
    ref
  )

  github_query(query)$then(function(resp) {
    check_github_response_ref(resp$response, resp$obj, rem, call. = call)
  })$then(function(obj) {
    list(
      sha = obj[[c("data", "repository", "sha", "oid")]],
      desc = obj[[c("data", "repository", "description", "text")]]
    )
  })
}

check_github_response_ref <- function(resp, obj, rem, call.) {
  if (!is.null(obj$errors)) {
    throw(new_github_query_error(rem, resp, obj, call.))
  }
  if (isTRUE(obj[[c("data", "repository", "description", "isBinary")]])) {
    throw(new_github_baddesc_error(rem, call.))
  }
  if (is.null(obj[[c("data", "repository", "sha")]])) {
    throw(new_github_noref_error(rem, call.))
  }
  if (is.null(obj[[c("data", "repository", "description", "text")]])) {
    throw(new_github_no_package_error(rem, call.))
  }
  obj
}

type_github_get_data_pull <- function(rem) {
  call <- sys.call(-1)
  user <- rem$username
  repo <- rem$repo
  pull <- rem$pull
  subdir <- rem$subdir %&z&% paste0(utils::URLencode(rem$subdir), "/")

  query <- sprintf(
    "{
    repository(owner: \"%s\", name: \"%s\") {
      pullRequest(number: %s) {
        headRefOid
        headRef {
          target {
            ... on Commit {
              file(path: \"%sDESCRIPTION\") {
                object {
                  ... on Blob {
                    isBinary
                    text
                  }
                }
              }
            }
          }
        }
      }
    }
  }",
    user,
    repo,
    pull,
    subdir
  )

  github_query(query)$then(function(resp) {
    check_github_response_pull(resp$response, resp$obj, rem, call. = call)
  })$then(function(obj) {
    ref <- obj[[c("data", "repository", "pullRequest", "headRefOid")]]
    txt <- obj[[c(
      "data",
      "repository",
      "pullRequest",
      "headRef",
      "target",
      "file",
      "object",
      "text"
    )]]
    list(sha = ref, desc = txt)
  })
}

check_github_response_pull <- function(resp, obj, rem, call.) {
  if (!is.null(obj$errors)) {
    throw(new_github_query_error(rem, resp, obj, call.))
  }
  # No full coverage here, because unless something goes super wrong,
  # these cases almost never happen.
  if (!is.null(obj$errors)) {
    throw(new_github_query_error(rem, resp, obj, call.)) # nocov
  }

  if (
    isTRUE(obj[[c(
      "data",
      "repository",
      "pullRequest",
      "headRef",
      "target",
      "file",
      "object",
      "isBinary"
    )]])
  ) {
    throw(new_github_baddesc_error(rem, call.)) # nocov
  }
  if (
    is.null(obj[[c(
      "data",
      "repository",
      "pullRequest",
      "headRef",
      "target",
      "file",
      "object"
    )]])
  ) {
    throw(new_github_no_package_error(rem, call.))
  }
  obj
}

type_github_get_data_release <- function(rem) {
  call <- sys.call(-1)
  user <- rem$username
  repo <- rem$repo
  ref <- NULL
  subdir <- rem$subdir %&z&% paste0(utils::URLencode(rem$subdir), "/")

  query <- sprintf(
    "{
    repository(owner: \"%s\", name:\"%s\") {
      latestRelease {
        tagName
        tagCommit {
          oid,
          file(path: \"%sDESCRIPTION\") {
            object {
              ... on Blob {
                isBinary
                text
              }
            }
          }
        }
      }
    }
  }",
    user,
    repo,
    subdir
  )

  github_query(query)$then(function(resp) {
    check_github_response_release(resp$response, resp$obj, rem, call. = call)
  })$then(function(obj) {
    ref <- obj[[c("data", "repository", "latestRelease", "tagCommit", "oid")]]
    txt <- obj[[c(
      "data",
      "repository",
      "latestRelease",
      "tagCommit",
      "file",
      "object",
      "text"
    )]]
    list(sha = ref, desc = txt)
  })
}

check_github_response_release <- function(resp, obj, rem, call.) {
  if (!is.null(obj$errors)) {
    throw(new_github_query_error(rem, resp, obj, call.))
  }
  if (is.null(obj[[c("data", "repository", "latestRelease")]])) {
    throw(new_github_no_release_error(rem, call.))
  }
  if (
    isTRUE(obj[[c(
      "data",
      "repository",
      "latestRelease",
      "tagCommit",
      "file",
      "object",
      "isBinary"
    )]])
  ) {
    throw(new_github_baddesc_error(rem, call.))
  }
  if (
    is.null(obj[[c(
      "data",
      "repository",
      "latestRelease",
      "tagCommit",
      "file",
      "object"
    )]])
  ) {
    throw(new_github_no_package_error(rem, call.))
  }
  obj
}

type_github_make_resolution <- function(data) {
  deps <- resolve_ref_deps(
    data$desc$get_deps(),
    data$desc$get("Remotes"),
    data$desc$get(extra_config_fields(data$desc$fields()))
  )

  url <- Sys.getenv("R_PKG_GITHUB_API_URL", "https://api.github.com")
  proto <- sub(":.*$", "", url)
  host <- sub("^[^:]*://", "", url)
  sha <- data$sha
  sha7 <- substr(sha, 1, 7)
  username <- data$remote$username
  repo <- data$remote$repo
  subdir <- data$remote$subdir %|z|% NULL
  commitish <- data$remote$commitish %|z|% NULL
  pull <- data$remote$pull %|z|% NULL
  release <- data$remote$release %|z|% NULL
  package <- data$desc$get_field("Package")
  version <- data$desc$get_field("Version")
  dependencies <- data$dependencies
  unknown <- deps$ref[deps$type %in% dependencies]
  unknown <- setdiff(unknown, c(base_packages(), "R"))

  meta <- c(
    RemoteType = "github",
    RemoteHost = host,
    RemoteRepo = repo,
    RemoteUsername = username,
    RemotePkgRef = data$remote$ref,
    RemoteRef = if (is.null(pull)) commitish %||% "HEAD" else NULL,
    RemotePull = pull,
    RemoteSha = sha,
    RemoteSubdir = subdir,
    GithubRepo = repo,
    GithubUsername = username,
    GithubRef = if (is.null(pull)) commitish %||% "HEAD" else NULL,
    GitHubPull = pull,
    GithubSHA1 = sha,
    GithubSubdir = subdir
  )

  list(
    ref = data$remote$ref,
    type = data$remote$type,
    direct = data$direct,
    status = "OK",
    package = package,
    version = version,
    license = data$desc$get_field("License", NA_character_),
    sources = sprintf(
      "%s://%s/repos/%s/%s/zipball/%s",
      proto,
      host,
      username,
      repo,
      sha
    ),
    target = sprintf("src/contrib/%s_%s_%s.tar.gz", package, version, sha7),
    remote = list(data$remote),
    deps = list(deps),
    unknown_deps = unknown,
    extra = list(list(remotesha = sha)),
    metadata = meta,
    params = data$remote$params,
    sysreqs = data$desc$get_field("SystemRequirements", "")
  )
}

github_query <- function(
  query,
  url = paste0(
    Sys.getenv("R_PKG_GITHUB_API_URL", "https://api.github.com"),
    "/graphql"
  ),
  headers = character(),
  ...
) {
  query
  url
  headers
  list(...)

  headers <- c(headers, type_github_get_headers())
  data <- tojson$write_str(
    list(query = query),
    opts = list(auto_unbox = TRUE)
  )
  resp <- NULL
  obj <- NULL

  http_post(url, data = data, headers = headers, ...)$catch(
    error = function(err) throw(new_github_offline_error())
  )$then(function(res) {
    resp <<- res
    json <- rawToChar(res$content %||% raw())
    obj <<- if (nzchar(json)) jsonlite::fromJSON(json, simplifyVector = FALSE)
    res
  })$then(http_stop_for_status)$catch(async_http_error = function(err) {
    throw(new_github_http_error(resp, obj), parent = err)
  })$then(function(res) {
    list(response = resp, obj = obj)
  })
}

# -----------------------------------------------------------------------
# Errors

new_github_error <- function(..., call. = NULL) {
  cond <- new_error(..., call. = call.)
  class(cond) <- c("github_error", class(cond))
  cond
}

# No internet?

new_github_offline_error <- function(call. = NULL) {
  new_github_error("Cannot query GitHub, are you offline?", call. = call.)
}

# HTTP error

new_github_http_error <- function(response, obj, call. = NULL) {
  if (
    response$status_code == 401 &&
      nzchar(obj$message) &&
      grepl("Bad credentials", obj$message)
  ) {
    return(new_github_badpat_error(call. = call.))
  }
  new_github_error("GitHub HTTP error", call. = call.)
}

# Error in a query

new_github_query_error <- function(rem, response, obj, call. = NULL) {
  if ("RATE_LIMITED" %in% vcapply(obj$errors, "[[", "type")) {
    return(new_github_ratelimited_error(response, obj, call. = NULL))

    # we don't actually get this response currently
  } else if (
    grepl("Could not resolve to a User", vcapply(obj$errors, "[[", "message"))
  ) {
    return(new_github_nouser_error(rem, obj, call. = call.)) # nocov
  } else if (
    grepl(
      "Could not resolve to a Repository",
      vcapply(obj$errors, "[[", "message")
    )
  ) {
    return(new_github_norepo_error(rem, obj, call. = call.))
  } else if (
    grepl(
      "Could not resolve to a PullRequest",
      vcapply(obj$errors, "[[", "message")
    )
  ) {
    return(new_github_nopr_error(rem, obj, call. = call.))
  }

  # Otherwise some generic code
  ghmsgs <- sub("\\.?$", ".", vcapply(obj$errors, "[[", "message")) # nocov
  msg <- paste0("GitHub error: ", paste0(ghmsgs, collapse = ", ")) # nocov
  new_github_error(msg, call. = call.) # nocov
}

# No such user/org

new_github_nouser_error <- function(rem, obj, call. = NULL) {
  # nocov
  new_github_error(
    # nocov
    "Can't find GitHub user {rem$username}.", # nocov
    call. = call. # nocov
  ) # nocov
}

# No such repo

new_github_norepo_error <- function(rem, obj, call. = NULL) {
  new_github_error(
    sprintf("Can't find GitHub repo %s/%s.", rem$username, rem$repo),
    call. = call.
  )
}
# Not an R package?

new_github_no_package_error <- function(rem, call. = NULL) {
  subdir <- rem$subdir %&z&% paste0(" in directory '", rem$subdir, "'")
  msg <- sprintf(
    "Can't find R package in GitHub repo %s/%s%s",
    rem$username,
    rem$repo,
    subdir
  )
  new_github_error(msg, call. = call.)
}

# Invalid GitHub PAT

new_github_badpat_error <- function(call. = NULL) {
  new_github_error(paste0(
    "Bad GitHub credentials, ",
    "make sure that your GitHub token is valid."
  ))
}

# DESCRIPTION does not parse

new_github_baddesc_error <- function(rem, call. = NULL) {
  subdir <- rem$subdir %&z&% paste0(", in directory `", rem$subdir, "`")
  msg <- sprintf(
    "Can't parse DESCRIPTION file in GitHub repo %s/%s%s",
    rem$username,
    rem$repo,
    subdir
  )
  new_github_error(msg, call. = call.)
}

# No such PR

new_github_nopr_error <- function(rem, obj, call. = NULL) {
  msg <- sprintf(
    "Can't find PR #%s in GitHub repo %s/%s",
    rem$pull,
    rem$username,
    rem$repo
  )
  new_github_error(msg, call. = call.)
}

# No releases
new_github_no_release_error <- function(rem, obj, call. = NULL) {
  msg <- sprintf(
    "Can't find any release in GitHub repo %s/%s.",
    rem$username,
    rem$repo
  )
  new_github_error(msg, call. = call.)
}

# No such branch/tag/ref

new_github_noref_error <- function(rem, call. = NULL) {
  ref <- rem$commitish %|z|% "HEAD"
  msg <- sprintf(
    "Can't find reference @%s in GitHub repo %s/%s.",
    ref,
    rem$username,
    rem$repo
  )
  new_github_error(msg, call. = call.)
}

# Rate limited

new_github_ratelimited_error <- function(response, obj, call. = NULL) {
  headers <- curl::parse_headers_list(response$headers)
  ghmsgs <- sub("\\.?$", ".", vcapply(obj$errors, "[[", "message"))
  msg <- paste0("GitHub error: ", paste0(ghmsgs, collapse = ", "))
  if ("x-ratelimit-reset" %in% names(headers)) {
    reset <- format(
      .POSIXct(headers$`x-ratelimit-reset`, tz = "UTC"),
      usetz = TRUE
    )
    msg <- paste0(msg, " Rate limit will reset at ", reset, ".")
  }
  new_github_error(msg, call. = call.)
}
