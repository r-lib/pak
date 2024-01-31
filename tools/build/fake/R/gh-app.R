# -------------------------------------------------------------------------
# GH app

mkdirp <- function(x) {
  dir.create(x, showWarnings = FALSE, recursive = TRUE)
}

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
    `x-ratelimit-reset` = as.integer(Sys.time() + as.difftime(1, units = "hours")),
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
  app$use("json body parser" = webfakes::mw_json(
    type = c(
      "",
      "application/json",
      "application/json; charset=utf-8"
    )
  ))

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
    if (is.null(auth)) {
      return("next")
    }
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
      "owner:[ ]*\"(?<user>[^\"]+)\"", "(?s:.)*",
      "name:[ ]*\"(?<repo>[^\"]+)\"", "(?s:.)*",
      "description:[ ]*object[(]expression:[ ]*\"[^:]+:(?<path>[^\"]+)\"", "(?s:.)*",
      "sha:[ ]*object[(]expression:[ ]*\"(?<ref>[^\"]+)\""
    )

    psd <- re_match(req$json$query, re_ref)
    if (is.na(psd$.match)) {
      return("next")
    }

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
      if ((!is.null(cmt$tag) && cmt$tag == psd$ref) ||
        (!is.null(cmt$branch) && cmt$branch == psd$ref) ||
        str_starts_with(cmt$sha, psd$ref)) {
        add_gh_headers(res)
        dsc <- cmt$files[[psd$path]]
        if (!is.null(cmt$token) &&
          (is.null(req$.token) || req$.token != cmt$token)) {
          send_repo_not_found(res, psd)
          return()
        }
        res$send_json(
          auto_unbox = TRUE,
          list(data = list(repository = list(
            description = gh_fmt_desc(dsc),
            sha = list(oid = cmt$sha)
          )))
        )
        return()
      }
    }

    res$set_status(200)
    res$send_json(
      auto_unbox = TRUE,
      list(data = list(repository = list(
        description = NA,
        sha = NA
      )))
    )
  })

  app$post("/graphql", function(req, res) {
    re_pull <- paste0(
      "owner:[ ]*\"(?<user>[^\"]+)\"", "(?s:.)*",
      "name:[ ]*\"(?<repo>[^\"]+)\"", "(?s:.)*",
      "pullRequest[(]number:[ ]*(?<pull>[0-9]+)[)]", "(?s:.)*",
      "file[(]path:[ ]*\"(?<path>.*)\""
    )

    psd <- re_match(req$json$query, re_pull)
    if (is.na(psd$.match)) {
      return("next")
    }

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
          list(data = list(repository = list(pullRequest = list(
            headRefOid = cmt$sha,
            headRef = list(target = list(file = list(object = gh_fmt_desc(dsc))))
          ))))
        )
        return()
      }
    }

    send_pull_not_found(res, psd)
  })

  # @*release
  app$post("/graphql", function(req, res) {
    re_release <- paste0(
      "owner:[ ]*\"(?<user>[^\"]+)\"", "(?s:.)*",
      "name:[ ]*\"(?<repo>[^\"]+)\"", "(?s:.)*",
      "file[(]path:[ ]*\"(?<path>.*)\""
    )

    psd <- re_match(req$json$query, re_release)
    if (is.na(psd$.match)) {
      return("next")
    }

    commits <- app$locals$repos$users[[psd$user]]$repos[[psd$repo]]$commits
    for (cmt in commits) {
      if (isTRUE(cmt$latestRelease)) {
        add_gh_headers(res)
        dsc <- cmt$files[[psd$path]]
        res$send_json(
          auto_unbox = TRUE,
          list(data = list(repository = list(latestRelease = list(
            tagName = cmt$tagName,
            tagCommit = list(
              oid = cmt$sha,
              file = list(object = gh_fmt_desc(dsc))
            )
          ))))
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
    if (!req$params$repo %in% names(app$locals$repos$users[[req$params$user]]$repos)) {
      send_repo_not_found(res, req$params)
      return()
    }

    commits <- app$locals$repos$users[[req$params$user]]$repos[[req$params$repo]]$commits
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

gh_app_desc <- function(pkg) {
  sprintf("Package: %s\nVersion: 1.0.0\n", pkg)
}

random_sha <- function() {
  paste(
    sample(c(0:9, letters[1:6]), 64, replace = TRUE),
    collapse = ""
  )
}

gh_app_repos <- list(
  users = list(
    "r-lib" = list(
      repos = list(
        pak = list(
          commits = list(
            list(
              sha = "111ef906acb58fe406370f7bc0a72cac55dbbb231ea687494c25742ca521255a",
              branch = "main",
              tag = "HEAD",
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            ),
            list(
              sha = "a503fe843f11c279864f29d58137f8de319d115b239ce48ccc15406306019480",
              branch = "main",
              tag = "v0.1.2",
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            ),
            list(
              sha = "e65de1e9630dbfcaf1044718b742bf806486b107239ce48ccc15406306019480",
              branch = "main",
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            ),
            list(
              sha = "b001d6ddeab1589ad367b62baabbeeb2af3b0ebac2e61d239df660c1d63e3232",
              branch = "somebranch",
              pull = 90,
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            ),
            list(
              sha = "b001d6ddeab1589ad367b62baabbeeb2af3b0ebac2e61d239df660c1d63e3232",
              latestRelease = TRUE,
              tagName = "v1.2.3",
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            )
          )
        ),
        bad = list(
          commits = list(
            list(
              sha = "546d9eab84b002c35302dda3822560950c7528cfc9ef1b916cecd9dbef3cf6b6",
              tag = "HEAD",
              branch = "main",
              files = list(
                DESCRIPTION = "this is not\na good file\n",
                "bin/DESCRIPTION" = charToRaw("\xf0\xb0\xa0")
              )
            ),
            list(
              sha = "546d9eab84b002c35302dda3822560950c7528cfc9ef1b916cecd9dbef3cf6b6",
              pull = 100,
              branch = "other",
              files = list(DESCRIPTION = "this is not\na good file\n")
            )
          )
        ),
        crayon = list(
          commits = list(
            list(
              sha = "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975",
              tag = "HEAD",
              branch = "main",
              files = list(DESCRIPTION = gh_app_desc("crayon"), NAMESPACE = "")
            ),
            list(
              sha = "b5221ab024605019800ddea474f7a0981a4d53f719f5af2b1af627b34e0760b2",
              branch = "b5221ab024605019800ddea474f7a0981a4d53f719f5af2b1af627b34e0760b2",
              files = list(DESCRIPTION = gh_app_desc("crayon"), NAMESPACE = "")
            ),
            list(
              sha = "9d93692f8f7c1d6b2308d0c4aa83cdc2d99ec1fd0097cede1d9aa1301247cb01",
              branch = "pr61",
              pull = 79,
              files = list(DESCRIPTION = gh_app_desc("crayon"), NAMESPACE = "")
            )
          )
        ),
        pkgconfig = list(
          commits = list(
            list(
              sha = "c9be9cde5e91ad771d1b5150781e6e8d32a7be0e9ab227bdf45cb41ad513004c",
              branch = "pr7",
              pull = 7,
              files = list(DESCRIPTION = gh_app_desc("pkgconfig"), NAMESPACE = "")
            )
          )
        )
      )
    ),
    "wesm" = list(
      repos = list(
        "feather" = list(
          commits = list(
            list(
              sha = "ec40c1eae1ac83b86fc41bb2f5cd916152d19015649c3d209f2c08115dd993b1",
              tag = "HEAD",
              branch = "main",
              files = list("R/DESCRIPTION" = gh_app_desc("feather"), NAMESPACE = "")
            )
          )
        )
      )
    ),
    "gaborcsardi" = list(
      repos = list(
        "secret-test" = list(
          commits = list(
            list(
              sha = "599cc5d745d2079eddf1ff582b83d381e885cd30f33bafebbe83e73d010cfa93",
              tag = "HEAD",
              branch = "main",
              token = "b9984750bea6a170081ca98255c3b43fe5fb0978",
              files = list("DESCRIPTION" = gh_app_desc("secret"), NAMESPACE = "")
            )
          )
        ),
        "secret" = list(
          commits = list(
            list(
              sha = "7f9fb08e26015e05529cd4d7fc2a7edbd88c783d456ff83a96dcc58ace1d3ea5",
              tag = "HEAD",
              branch = "x",
              files = list("DESCRIPTION" = gh_app_desc("secret"), NAMESPACE = "")
            )
          )
        )
      )
    ),
    "tidyverse" = list(
      repos = list(
        "tidyverse.org" = list(
          commits = list(
            list(
              sha = "d998eab68c66d862c31a6091f9e71200b13bb44ea754e3371d098dcaa20e51a4",
              tag = "HEAD",
              branch = "main",
              files = list("foo" = "bar")
            )
          )
        )
      )
    ),
    "cran" = list(
      repos = list(
        "rJava" = list(
          commits = list(
            list(
              sha = "dfb3b64b13343e07b2db038777d9dc2aba5d824c5eca8c891c87bd4fd38d7256",
              tag = "HEAD",
              branch = "master",
              files = list(
                DESCRIPTION = "Package: rJava\nVersion: 1.0-6\nSystemRequirements: Java JDK 1.2 or higher (for JRI/REngine JDK 1.4 or higher), GNU make\n",
                NAMESPACE = ""
              )
            )
          )
        )
      )
    )
  )
)
