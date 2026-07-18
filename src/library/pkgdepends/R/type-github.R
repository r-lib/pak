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
    remote$subdir <- resp$subdir %||% remote$subdir
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
  sha1 <- (if (is.list(candidate$extra[[1]])) {
    candidate$extra[[1]][["remotesha"]]
  }) %||%
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

# Well-known subdirectories to probe for a DESCRIPTION when no `subdir` is
# given. Order matters: the first match wins, and "" (the repo root) always
# takes precedence, so repos with a root DESCRIPTION are unaffected.
github_subdir_candidates <- function() {
  c("", "pkg-r", "r", "R")
}

# The subdirs to query for a remote: the explicit `subdir` if supplied,
# otherwise the well-known candidates.
github_query_subdirs <- function(rem) {
  sub <- rem$subdir %||% ""
  if (nzchar(sub)) sub else github_subdir_candidates()
}

# GraphQL alias names for each candidate dir, in order: desc1, desc2, ...
github_subdir_aliases <- function(n) {
  if (n == 0) character() else paste0("desc", seq_len(n))
}

# Slash-suffixed, URL-encoded path prefixes (root -> "").
github_subdir_paths <- function(dirs) {
  vapply(
    dirs,
    function(d) if (nzchar(d)) paste0(utils::URLencode(d), "/") else "",
    character(1),
    USE.NAMES = FALSE
  )
}

# Aliased `object(expression: "<ref>:<path>DESCRIPTION")` blocks (ref variant).
github_ref_desc_fragment <- function(ref, dirs) {
  aliases <- github_subdir_aliases(length(dirs))
  paths <- github_subdir_paths(dirs)
  frag <- sprintf(
    "%s: object(expression: \"%s:%sDESCRIPTION\") { ... on Blob { isBinary text } }",
    aliases,
    ref,
    paths
  )
  paste(frag, collapse = "\n      ")
}

# Aliased `file(path: "<path>DESCRIPTION")` blocks (pull/release variants).
github_file_desc_fragment <- function(dirs) {
  aliases <- github_subdir_aliases(length(dirs))
  paths <- github_subdir_paths(dirs)
  frag <- sprintf(
    "%s: file(path: \"%sDESCRIPTION\") { object { ... on Blob { isBinary text } } }",
    aliases,
    paths
  )
  paste(frag, collapse = "\n      ")
}

# Walk the candidate dirs in priority order and return the first DESCRIPTION
# hit as list(text=, subdir=). `get_node(obj, alias)` extracts the Blob node
# (a list with `isBinary`/`text`) for an alias, or NULL if absent. Throws
# baddesc if the first blob found (in priority order) is binary. Returns NULL
# if nothing found.
github_pick_desc <- function(obj, dirs, get_node, rem, call.) {
  aliases <- github_subdir_aliases(length(dirs))
  for (i in seq_along(dirs)) {
    node <- get_node(obj, aliases[i])
    if (is.null(node)) {
      next
    }
    if (isTRUE(node$isBinary)) {
      throw(new_github_baddesc_error(rem, call.))
    }
    if (!is.null(node$text)) {
      return(list(text = node$text, subdir = dirs[i]))
    }
  }
  NULL
}

# The pull/release DESCRIPTION probes use GraphQL `file(path:)` fields, one per
# candidate subdir. GitHub reports a `NOT_FOUND` error (not just a null node)
# for every candidate that does not exist, which is expected while probing more
# than one subdir. Drop those aliased file errors so that only genuine failures
# (a missing repo, PR, release, ...) are left in `obj$errors`.
github_drop_subdir_errors <- function(obj) {
  if (is.null(obj$errors)) {
    return(obj)
  }
  probe_miss <- vlapply(obj$errors, function(e) {
    n <- length(e$path)
    identical(e$type, "NOT_FOUND") &&
      n > 0 &&
      grepl("^desc[0-9]+$", e$path[[n]])
  })
  obj$errors <- obj$errors[!probe_miss]
  if (length(obj$errors) == 0) {
    obj$errors <- NULL
  }
  obj
}

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
  if (is.na(token)) {
    token <- tryCatch(
      gitcreds_get()$password,
      error = function(e) NA_character_
    )
  }
  if (is.na(token)) {
    token <- type_github_builtin_token()
  }
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
    list(sha = data$sha, description = dsc, subdir = data$subdir)
  })
}

type_github_get_data_ref <- function(rem) {
  user <- rem$username
  repo <- rem$repo
  ref <- rem$commitish %|z|% "HEAD"
  dirs <- github_query_subdirs(rem)

  query <- sprintf(
    "{
    repository(owner: \"%s\", name: \"%s\") {
      %s
      sha: object(expression: \"%s\") {
        oid
      }
    }
  }",
    user,
    repo,
    github_ref_desc_fragment(ref, dirs),
    ref
  )

  github_query(query)$then(function(resp) {
    obj <- check_github_response_ref(resp$response, resp$obj, rem, call. = call)
    hit <- github_pick_desc(
      obj,
      dirs,
      get_node = function(o, a) o[[c("data", "repository", a)]],
      rem = rem,
      call. = call
    )
    if (is.null(hit)) {
      throw(new_github_no_package_error(rem, call))
    }
    list(
      sha = obj[[c("data", "repository", "sha", "oid")]],
      desc = hit$text,
      subdir = hit$subdir
    )
  })
}

check_github_response_ref <- function(resp, obj, rem, call.) {
  if (!is.null(obj$errors)) {
    throw(new_github_query_error(rem, resp, obj, call.))
  }
  if (is.null(obj[[c("data", "repository", "sha")]])) {
    throw(new_github_noref_error(rem, call.))
  }
  obj
}

type_github_get_data_pull <- function(rem) {
  call <- sys.call(-1)
  user <- rem$username
  repo <- rem$repo
  pull <- rem$pull
  dirs <- github_query_subdirs(rem)

  query <- sprintf(
    "{
    repository(owner: \"%s\", name: \"%s\") {
      pullRequest(number: %s) {
        headRefOid
        headRef {
          target {
            ... on Commit {
              %s
            }
          }
        }
      }
    }
  }",
    user,
    repo,
    pull,
    github_file_desc_fragment(dirs)
  )

  github_query(query)$then(function(resp) {
    obj <- check_github_response_pull(resp$response, resp$obj, rem, call. = call)
    hit <- github_pick_desc(
      obj,
      dirs,
      get_node = function(o, a) {
        o[[c(
          "data",
          "repository",
          "pullRequest",
          "headRef",
          "target",
          a,
          "object"
        )]]
      },
      rem = rem,
      call. = call
    )
    if (is.null(hit)) {
      throw(new_github_no_package_error(rem, call))
    }
    ref <- obj[[c("data", "repository", "pullRequest", "headRefOid")]]
    list(sha = ref, desc = hit$text, subdir = hit$subdir)
  })
}

check_github_response_pull <- function(resp, obj, rem, call.) {
  obj <- github_drop_subdir_errors(obj)
  if (!is.null(obj$errors)) {
    throw(new_github_query_error(rem, resp, obj, call.))
  }
  obj
}

type_github_get_data_release <- function(rem) {
  call <- sys.call(-1)
  user <- rem$username
  repo <- rem$repo
  dirs <- github_query_subdirs(rem)

  query <- sprintf(
    "{
    repository(owner: \"%s\", name:\"%s\") {
      latestRelease {
        tagName
        tagCommit {
          oid,
          %s
        }
      }
    }
  }",
    user,
    repo,
    github_file_desc_fragment(dirs)
  )

  github_query(query)$then(function(resp) {
    obj <- check_github_response_release(resp$response, resp$obj, rem, call. = call)
    hit <- github_pick_desc(
      obj,
      dirs,
      get_node = function(o, a) {
        o[[c(
          "data",
          "repository",
          "latestRelease",
          "tagCommit",
          a,
          "object"
        )]]
      },
      rem = rem,
      call. = call
    )
    if (is.null(hit)) {
      throw(new_github_no_package_error(rem, call))
    }
    ref <- obj[[c("data", "repository", "latestRelease", "tagCommit", "oid")]]
    list(sha = ref, desc = hit$text, subdir = hit$subdir)
  })
}

check_github_response_release <- function(resp, obj, rem, call.) {
  obj <- github_drop_subdir_errors(obj)
  if (!is.null(obj$errors)) {
    throw(new_github_query_error(rem, resp, obj, call.))
  }
  if (is.null(obj[[c("data", "repository", "latestRelease")]])) {
    throw(new_github_no_release_error(rem, call.))
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
  retry = http_retry_post(),
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

  http_post(url, data = data, headers = headers, retry = retry, ...)$catch(
    error = function(err) throw(new_github_offline_error(), parent = err)
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
    any(grepl(
      "Could not resolve to a User",
      vcapply(obj$errors, "[[", "message")
    ))
  ) {
    return(new_github_nouser_error(rem, obj, call. = call.)) # nocov
  } else if (
    any(grepl(
      "Could not resolve to a Repository",
      vcapply(obj$errors, "[[", "message")
    ))
  ) {
    return(new_github_norepo_error(rem, obj, call. = call.))
  } else if (
    any(grepl(
      "Could not resolve to a PullRequest",
      vcapply(obj$errors, "[[", "message")
    ))
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
