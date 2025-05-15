# nocov start

str_starts_with <- function(x, pre) {
  substring(x, 1, nchar(pre)) == pre
}

gr_response_headers_graphql <- function(upd = NULL) {
  list(
    server = "GitHub.com",
    `content-type` = "application/json; charset=utf-8",
    `x-oauth-scopes` = "delete:packages, delete_repo, read:org, repo, workflow, write:packages",
    `x-accepted-oauth-scopes` = "repo",
    `x-github-media-type` = "github.v3; format=json",
    `x-ratelimit-limit` = "5000",
    `x-ratelimit-remaining` = "4998",
    `x-ratelimit-reset` = as.integer(
      Sys.time() + as.difftime(1, units = "hours")
    ),
    `x-ratelimit-used` = "2",
    `x-ratelimit-resource` = "graphql",
    `access-control-expose-headers` = "ETag, Link, Location, Retry-After, X-GitHub-OTP, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Used, X-RateLimit-Resource, X-RateLimit-Reset, X-OAuth-Scopes, X-Accepted-OAuth-Scopes, X-Poll-Interval, X-GitHub-Media-Type, X-GitHub-SSO, X-GitHub-Request-Id, Deprecation, Sunset",
    `access-control-allow-origin` = "*",
    `strict-transport-security` = "max-age=31536000; includeSubdomains; preload",
    `x-frame-options` = "deny",
    `x-content-type-options` = "nosniff",
    `x-xss-protection` = "0",
    `referrer-policy` = "origin-when-cross-origin, strict-origin-when-cross-origin",
    `content-security-policy` = "default-src 'none'",
    vary = "Accept-Encoding, Accept, X-Requested-With",
    `x-github-request-id` = basename(tempfile())
  )
}

make_dummy_zip <- function(commit) {
  mkdirp(tmp <- tempfile())
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(tmp)
  root <- paste0(commit$repo, "-", commit$branch)
  mkdirp(root)
  setwd(root)
  for (i in seq_along(commit$files)) {
    nm <- names(commit$files)[[i]]
    ct <- commit$files[[i]]
    mkdirp(dirname(nm))
    if (is.raw(ct)) {
      writeBin(ct, nm)
    } else {
      writeLines(ct, nm)
    }
  }
  setwd(tmp)
  zip::zip(paste0(root, ".zip"), root)
  file.path(tmp, paste0(root, ".zip"))
}

re_gh_auth <- function() {
  paste0(
    "^token (gh[pousr]_[A-Za-z0-9_]{36,251}|",
    "[[:xdigit:]]{40})$"
  )
}

process_repos <- function(repos) {
  for (i in seq_along(repos$users)) {
    u <- names(repos$users)[i]
    repos$users[[i]]$user <- u
    for (j in seq_along(repos$users[[i]]$repos)) {
      r <- names(repos$users[[i]]$repos)[j]
      repos$users[[i]]$repos[[j]]$user <- u
      for (k in seq_along(repos$users[[i]]$repos[[j]]$commits)) {
        repos$users[[i]]$repos[[j]]$commits[[k]]$user <- u
        repos$users[[i]]$repos[[j]]$commits[[k]]$repo <- r
      }
    }
  }
  repos
}

gh_fmt_desc <- function(dsc) {
  if (is.null(dsc)) {
    return(NA)
  } else if (is.raw(dsc)) {
    list(
      isBinary = TRUE,
      text = NA
    )
  } else {
    list(
      isBinary = FALSE,
      text = dsc
    )
  }
}

gh_app <- function(repos = NULL, log = interactive(), options = list()) {
  app <- webfakes::new_app()

  # Log requests by default
  if (log) app$use("logger" = webfakes::mw_log())

  # Parse JSON body, even if no content-type header is sent
  app$use(
    "json body parser" = webfakes::mw_json(
      type = c(
        "",
        "application/json",
        "application/json; charset=utf-8"
      )
    )
  )

  #  app$use("text body parser" = webfakes::mw_text(type = c("text/plain", "application/json")))
  #  app$use("multipart body parser" = webfakes::mw_multipart())
  #  app$use("URL encoded body parser" = webfakes::mw_urlencoded())

  # Add etags by default
  app$use("add etag" = webfakes::mw_etag())

  # Add date by default
  app$use("add date" = function(req, res) {
    res$set_header("Date", as.character(Sys.time()))
    "next"
  })

  app$locals$repos <- process_repos(repos)
  app$locals$data <- list()

  app$use(function(req, res) {
    auth <- req$get_header("Authorization")
    if (is.null(auth)) return("next")
    if (!grepl(re_gh_auth(), auth)) {
      res$set_status(401)
      res$send_json(
        auto_unbox = TRUE,
        list(
          message = "Bad credentials",
          documentation_url = "https://docs.github.com/graphql"
        )
      )
    } else {
      req$.token <- sub("^token ", "", auth)
      "next"
    }
  })

  app$post("/404/graphql", function(req, res) {
    res$send_status(404)
  })

  app$post("/graphql", function(req, res) {
    re_ref <- paste0(
      "owner:[ ]*\"(?<user>[^\"]+)\"",
      "(?s:.)*",
      "name:[ ]*\"(?<repo>[^\"]+)\"",
      "(?s:.)*",
      "description:[ ]*object[(]expression:[ ]*\"[^:]+:(?<path>[^\"]+)\"",
      "(?s:.)*",
      "sha:[ ]*object[(]expression:[ ]*\"(?<ref>[^\"]+)\""
    )

    psd <- re_match(req$json$query, re_ref)
    if (is.na(psd$.match)) return("next")

    if (!psd$user %in% names(app$locals$repos$users)) {
      send_user_not_found(res, psd)
      return()
    }
    if (!psd$repo %in% names(app$locals$repos$users[[psd$user]]$repos)) {
      send_repo_not_found(res, psd)
      return()
    }

    commits <- app$locals$repos$users[[psd$user]]$repos[[psd$repo]]$commits
    for (cmt in commits) {
      if (
        (!is.null(cmt$tag) && cmt$tag == psd$ref) ||
          (!is.null(cmt$branch) && cmt$branch == psd$ref) ||
          str_starts_with(cmt$sha, psd$ref)
      ) {
        add_gh_headers(res)
        dsc <- cmt$files[[psd$path]]
        if (
          !is.null(cmt$token) &&
            (is.null(req$.token) || req$.token != cmt$token)
        ) {
          send_repo_not_found(res, psd)
          return()
        }
        res$send_json(
          auto_unbox = TRUE,
          list(
            data = list(
              repository = list(
                description = gh_fmt_desc(dsc),
                sha = list(oid = cmt$sha)
              )
            )
          )
        )
        return()
      }
    }

    res$set_status(200)
    res$send_json(
      auto_unbox = TRUE,
      list(
        data = list(
          repository = list(
            description = NA,
            sha = NA
          )
        )
      )
    )
  })

  app$post("/graphql", function(req, res) {
    re_pull <- paste0(
      "owner:[ ]*\"(?<user>[^\"]+)\"",
      "(?s:.)*",
      "name:[ ]*\"(?<repo>[^\"]+)\"",
      "(?s:.)*",
      "pullRequest[(]number:[ ]*(?<pull>[0-9]+)[)]",
      "(?s:.)*",
      "file[(]path:[ ]*\"(?<path>.*)\""
    )

    psd <- re_match(req$json$query, re_pull)
    if (is.na(psd$.match)) return("next")

    if (!psd$user %in% names(app$locals$repos$users)) {
      send_user_not_found(res, psd)
      return()
    }
    if (!psd$repo %in% names(app$locals$repos$users[[psd$user]]$repos)) {
      send_repo_not_found(res, psd)
      return()
    }

    commits <- app$locals$repos$users[[psd$user]]$repos[[psd$repo]]$commits
    for (cmt in commits) {
      if (!is.null(cmt$pull) && cmt$pull == psd$pull) {
        add_gh_headers(res)
        dsc <- cmt$files[[psd$path]]
        res$send_json(
          auto_unbox = TRUE,
          list(
            data = list(
              repository = list(
                pullRequest = list(
                  headRefOid = cmt$sha,
                  headRef = list(
                    target = list(file = list(object = gh_fmt_desc(dsc)))
                  )
                )
              )
            )
          )
        )
        return()
      }
    }

    send_pull_not_found(res, psd)
  })

  # @*release
  app$post("/graphql", function(req, res) {
    re_release <- paste0(
      "owner:[ ]*\"(?<user>[^\"]+)\"",
      "(?s:.)*",
      "name:[ ]*\"(?<repo>[^\"]+)\"",
      "(?s:.)*",
      "file[(]path:[ ]*\"(?<path>.*)\""
    )

    psd <- re_match(req$json$query, re_release)
    if (is.na(psd$.match)) return("next")

    commits <- app$locals$repos$users[[psd$user]]$repos[[psd$repo]]$commits
    for (cmt in commits) {
      if (isTRUE(cmt$latestRelease)) {
        add_gh_headers(res)
        dsc <- cmt$files[[psd$path]]
        res$send_json(
          auto_unbox = TRUE,
          list(
            data = list(
              repository = list(
                latestRelease = list(
                  tagName = cmt$tagName,
                  tagCommit = list(
                    oid = cmt$sha,
                    file = list(object = gh_fmt_desc(dsc))
                  )
                )
              )
            )
          )
        )
        return()
      }
    }

    send_no_releases(res, psd)
  })

  app$get("/repos/:user/:repo/zipball/:sha", function(req, res) {
    if (!req$params$user %in% names(app$locals$repos$users)) {
      send_user_not_found(res, req$params)
      return()
    }
    if (
      !req$params$repo %in%
        names(app$locals$repos$users[[req$params$user]]$repos)
    ) {
      send_repo_not_found(res, req$params)
      return()
    }

    commits <- app$locals$repos$users[[req$params$user]]$repos[[
      req$params$repo
    ]]$commits
    shas <- vapply(commits, "[[", "", "sha")
    if (!req$params$sha %in% shas) {
      send_sha_not_found(res, req$params)
      return()
    }

    cmt <- commits[[which(shas == req$params$sha)]]
    z <- make_dummy_zip(cmt)
    res$send_file(z, root = "/")
  })

  app
}

add_gh_headers <- function(res) {
  headers <- gr_response_headers_graphql()
  for (i in seq_along(headers)) {
    res$set_header(names(headers)[i], headers[i])
  }
}

send_user_not_found <- function(res, psd) {
  res$set_status(200)
  res$send_json(
    auto_unbox = TRUE,
    list(
      data = list(repository = NA),
      errors = list(
        list(
          type = "NOT_FOUND",
          path = list("repository"),
          locations = list(
            list(
              line = 2,
              column = 3
            )
          ),
          message = sprintf(
            "Could not resolve to a Repository with the name '%s'.",
            paste0(psd$user, "/", psd$repo)
          )
        )
      )
    )
  )
}

send_repo_not_found <- function(res, psd) {
  send_user_not_found(res, psd)
}

send_ref_not_found <- function(res, psd) {
  res$send_status(404)
}

send_pull_not_found <- function(res, psd) {
  res$set_status(200)
  res$send_json(
    auto_unbox = TRUE,
    list(
      data = list(repository = list(pullRequest = NA)),
      errors = list(
        list(
          type = "NOT_FOUND",
          path = list("repository", "pullRequest"),
          locations = list(
            list(
              line = 3L,
              column = 5L
            )
          ),
          message = sprintf(
            "Could not resolve to a PullRequest with the number of %s.",
            psd$pull
          )
        )
      )
    )
  )
}

send_sha_not_found <- function(res, psd) {
  # TODO
  res$send_status(404)
}

send_no_releases <- function(res, psd) {
  res$set_status(200)
  res$send_json(
    auto_unbox = TRUE,
    list(
      data = list(repository = list(latestRelease = NA))
    )
  )
}

# nocov end
