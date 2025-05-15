ghr <- local({
  # -------------------------------------------------------------------------

  ghr_list <- function(repo) {
    synchronize(async_ghr_list(repo))
  }

  async_ghr_list <- function(repo) {
    repo <- parse_slug(repo)
    query <- glue::glue(
      "{
    rateLimit {
      cost
      remaining
    }
    repository(owner: \"<repo$user>\", name: \"<repo$repo>\") {
      releases(last: 100) {
        nodes {
          id
          name
          createdAt
          tagName
        }
      }
    }
  }",
      .open = "<",
      .close = ">"
    )

    github_query(query)$then(function(resp) {
      rls <- resp$obj$data$repository$releases$nodes
      data_frame(
        id = vcapply(rls, "[[", "id"),
        name = vcapply(rls, "[[", "name"),
        tag_name = vcapply(rls, "[[", "tagName"),
        created_at = parse_iso_8601(vcapply(rls, "[[", "createdAt"))
      )
    })
  }

  # -------------------------------------------------------------------------

  ghr_get <- function(repo, tag) {
    synchronize(async_ghr_get(repo, tag))
  }

  async_ghr_get <- function(repo, tag) {
    prepo <- parse_slug(repo)
    ep <- glue::glue("/repos/{prepo$owner}/{prepo$repo}/releases/tags/{tag}")
    async_github_v3_query(ep)$then(function(resp) {
      jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    })
  }

  # -------------------------------------------------------------------------

  ghr_list_assets <- function(repo, tag) {
    synchronize(async_ghr_list_assets(repo, tag))
  }

  async_ghr_list_assets <- function(repo, tag) {
    repo <- parse_slug(repo)
    query <- glue::glue(
      "{
    rateLimit {
      cost
      remaining
    }
    repository(owner: \"<repo$user>\", name: \"<repo$repo>\") {
      release(tagName: \"<tag>\") {
        releaseAssets(last: 100) {
          nodes {
            id
            name
            downloadUrl
            size
            createdAt
            updatedAt
            contentType
          }
        }
      }
    }
  }",
      .open = "<",
      .close = ">"
    )

    github_query(query)$then(function(resp) {
      asts <- resp$obj$data$repository$release$releaseAssets$nodes
      data_frame(
        id = vcapply(asts, "[[", "id"),
        name = vcapply(asts, "[[", "name"),
        download_url = vcapply(asts, "[[", "downloadUrl"),
        size = viapply(asts, "[[", "size"),
        created_at = parse_iso_8601(vcapply(asts, "[[", "createdAt")),
        updated_at = parse_iso_8601(vcapply(asts, "[[", "updatedAt")),
        content_type = vcapply(asts, "[[", "contentType")
      )
    })
  }

  # -------------------------------------------------------------------------

  ghr_add_asset <- function(repo, file, tag, name = basename(file)) {
    invisible(synchronize(async_ghr_add_asset(repo, file, tag, name)))
  }

  async_ghr_add_asset <- function(repo, file, tag, name = basename(file)) {
    repo
    file
    tag
    name

    async_ghr_delete_asset(repo, tag, name)$then(
      function(res) res$release$upload_url
    )$catch(async_http_404 = function(err) {
      async_ghr_create(repo, tag)$then(function(rel) rel$upload_url)
    })$then(function(upload_url) {
      upload_url <- sub("[{].*[}]", "", upload_url)
      prepo <- parse_slug(repo)
      async_github_v3_query(
        url = upload_url,
        endpoint = "",
        query = c(name = name),
        method = "POST",
        headers = c("Content-Type" = "application/octet-stream"),
        file = file
      )
    })$then(function(resp) {
      jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    })
  }

  # -------------------------------------------------------------------------

  ghr_delete_asset <- function(repo, tag, name) {
    synchronize(async_ghr_delete_asset(repo, tag, name))
  }

  async_ghr_delete_asset <- function(repo, tag, name) {
    prepo <- parse_slug(repo)
    async_ghr_get(repo, tag)$then(function(res) {
      release_id <- res$id
      asset_names <- vcapply(res$assets, "[[", "name")
      if (name %in% asset_names) {
        asset_id <- res$assets[[match(name, asset_names)]]$id
        ep <- glue::glue(
          "/repos/{prepo$user}/{prepo$repo}/releases/assets/{asset_id}"
        )
        async_github_v3_query(ep, method = "DELETE")$then(function(resp) {
          list(
            release = res,
            deleted = TRUE
          )
        })
      } else {
        list(release = res, deleted = FALSE)
      }
    })
  }

  # -------------------------------------------------------------------------

  ghr_create <- function(repo, tag) {
    invisible(synchronize(async_ghr_create(repo, tag)))
  }

  async_ghr_create <- function(
    repo,
    tag,
    description = "",
    draft = FALSE,
    prerelease = FALSE,
    generate_release_notes = FALSE
  ) {
    prepo <- parse_slug(repo)
    ep <- glue::glue("/repos/{prepo$owner}/{prepo$repo}/releases")
    data <- tojson$write_str(
      list(
        tag_name = tag,
        name = paste0(prepo$repo, " ", tag),
        body = description,
        draft = draft,
        prerelease = prerelease,
        generate_release_notes = generate_release_notes
      ),
      list(auto_unbox = TRUE)
    )

    async_github_v3_query(
      ep,
      data = data,
      method = "POST"
    )$then(function(resp) {
      jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    })
  }

  # -------------------------------------------------------------------------

  parse_slug <- function(slug) {
    parts <- strsplit(slug, "/", fixed = TRUE)[[1]]
    list(user = parts[1], owner = parts[1], repo = parts[2])
  }

  async_github_v3_query <- function(
    endpoint,
    query = NULL,
    method = c("GET", "POST", "DELETE"),
    headers = NULL,
    data = NULL,
    file = NULL,
    url = NULL
  ) {
    method <- match.arg(method)

    headers <- update_named_vector(type_github_get_headers(), headers)

    base <- url %||%
      Sys.getenv("R_PKG_GITHUB_API_URL", "https://api.github.com")
    query_str <- paste(
      glue::glue(
        "{curl::curl_escape(names(query))}={curl::curl_escape(query)}"
      ),
      collapse = "&"
    )
    url <- paste0(base, endpoint, "?", query_str)

    px <- if (method == "GET") {
      http_get(url, headers = headers)
    } else if (method == "POST") {
      if (is.null(data) + is.null(file) != 1) {
        throw(pkg_error(
          "Must specify exactly of {.arg data} and {.arg file} for POST requests."
        ))
      }
      if (is.null(data)) data <- readBin(file, "raw", file.size(file))
      http_post(url, data, headers = headers)
    } else if (method == "DELETE") {
      http_delete(url, headers = headers)
    }

    px$then(http_stop_for_status)
  }

  # github_query() is in type-github.R

  # -----------------------------------------------------------------------
  # Exported functions

  structure(
    list(
      .internal = environment(),

      async_add_asset = async_ghr_add_asset,
      async_delete_asset = async_ghr_delete_asset,
      async_create = async_ghr_create,
      async_get = async_ghr_get,
      async_list = async_ghr_list,
      async_list_assets = async_ghr_list_assets,

      add_asset = ghr_add_asset,
      delete_asset = ghr_delete_asset,
      create = ghr_create,
      get = ghr_get,
      list = ghr_list,
      list_assets = ghr_list_assets
    )
  )
})

# -------------------------------------------------------------------------

#' GitHub Releases
#'
#' Functions to query and manipulate GitHub releases. These functions are
#' currently experimental.
#'
#' @details
#'
#' ## List releases
#'
#' ### Description
#'
#' `ghr$list()` lists the last 100 releases for a GitHub repository.
#' `ghr$async_list()` is the async version of `ghr$list()`.
#'
#' ### Usage
#' ```
#' ghr$list(repo)
#' ghr$async_list(repo)
#' ```
#'
#' ### Arguments
#'
#' * `repo`: repository slug, e.g. `"cran/cli"`.
#'
#' ### Value
#'
#' Data frame with columns:
#'   * `id`: release id,
#'   * `name`: release name, usually the version number, possibly with
#'     a `v` prefix: `3.6.1` or `v3.6.1`, but can be different.
#'   * `tag_name`: usually the same as `name`.
#'   * `created_at`: `POSIXct` vector.
#'
# -------------------------------------------------------------------------
#'
#' ## Get information about a release
#'
#' ### Description
#'
#' `ghr$get()` downloads information about a release, including
#' release assets.
#'
#' `ghr$async_get` is the async version of `ghr$get`.
#'
#' ### Usage
#' ```
#' ghr$get(repo, tag)
#' ```
#'
#' ### Arguments
#'
#' * `repo`: repository slug, e.g. `"cran/cli"`.
#' * `tag`: tag to get.
#'
#' ### Value
#'
#' Named list, see
#' <https://docs.github.com/en/rest/releases/releases#get-a-release>
#' for the entries.
#'
# -------------------------------------------------------------------------
#'
#' ## List assets of a release
#'
#' ### Description
#'
#' `ghr$list_assets()` lists the last 100 assets of a release.
#'
#' `ghr$async_list_assets()` is the async version of `ghr$list_assets()`
#'
#' ### Usage
#' ```
#' ghr$list_assets(repo, tag)
#' ```
#'
#' ### Arguments
#'
#' * `repo`: repository slug, e.g. `"cran/cli"`.
#' * `tag`: tag to query.
#'
#' ### Value
#'
#' Data frame with columns:
#' * `id`: asset id,
#' * `name`: file name of the asset,
#' * `download_url`: download URL,
#' * `size`: size in bytes,
#' * `created_at`: `POSIXct` vector,
#' * `updated_at`: `POSXct` vector,
#' * `content_type`: content type of asset.
#'
# -------------------------------------------------------------------------
#'
#' ## Add a release asset
#'
#' ### Description
#'
#' `ghr$add_asset()` adds an asset to a GitHub release.
#'
#' `ghr$async_add_asset()` is the async version of `ghr$add_asset()`.
#'
#' ### Usage
#' ```
#' ghr%add_asset(repo, file, tag, name = basename(file))
#' ```
#'
#' ### Arguments
#'
#' * `repo`: repository slug, e.g. `cran/cli`.
#' * `file`: path to file to upload as an asset.
#' * `tag`: tag name to add the asset to. It must exist on GitHub.
#' * `name`: file name of the asset in the release.
#'
#' ### Details
#'
#' If an asset with the same name already exists, then that will be deleted
#' first.
#'
#' ### Value
#'
#' Response from GitHub as a named list. See
#' <https://docs.github.com/en/rest/releases/assets#upload-a-release-asset>
#' for the structure.
#'
# -------------------------------------------------------------------------
#' ## Delete a release asset
#'
#' ### Description
#'
#' `ghr$delete_asset()` deleted a release asset.
#'
#' `ghr$async_delete_asset()` is an async version of `ghr$delete_asset()`.
#'
#' ### Usage
#' ```
#' ghr$delete_asset(repo, tag, name)
#' ghr$async_delete_asset(repo, tag, name)
#' ```
#'
#' ### Arguments
#'
#' * `repo`: repository slug, e.g. `cran/cli`.
#' * `tag`: tag name to create a release for. It must exist on GitHub.
#' * `name`: name of the asset.
#'
#' ### Value
#'
#' A list with entries:
#' * `release`: a list with the data about the release, before the
#'   deletion. It has the same format as the return value of `ghr$get()`.
#' * `deleted`: `TRUE` if the asset was deleted. `FALSE` if the asset
#'   did not exist.
#'
# -------------------------------------------------------------------------
#'
#' ## Create a GitHub release
#'
#' ### Description
#'
#' `ghr$create()` creates a GitHub release from a tag.
#'
#' `ghr$async_create()` is an async version of `ghr$create()`.
#'
#' ### Usage
#' ```
#' ghr$create(
#'   repo,
#'   tag,
#'   description = "",
#'   draft = FALSE,
#'   prerelease = FALSE,
#'   generage_release_notes = FALSE
#' )
#' ```
#'
#' ### Arguments
#'
#' * `repo`: repository slug, e.g. `cran/cli`.
#' * `tag`: tag name to create a release for. It must exist on GitHub.
#' * `description`: release description.
#' * `draft`: whether to create a draft release.
#' * `prerelease`: whether to create a prerelease.
#' * `generate_release_notes`: whether to auto-generate release notes.
#'
#' ### Value
#'
#' Response from GitHub as a named list. See
#' <https://docs.github.com/en/rest/releases/releases#create-a-release>
#' for the structure.
#'
# -------------------------------------------------------------------------
#'
#' @name ghr
#' @keywords internal
#' @export

ghr
