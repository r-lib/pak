parse_remote_gitlab <- function(specs, config, ...) {
  pds <- re_match(specs, gitlab_rx())
  pds$ref <- pds$.text
  pds$protocol[pds$protocol == ""] <- "https"
  pds$host[pds$host == ""] <- "gitlab.com"
  pds$path <- paste0("/", pds$projectpath, "/", pds$project)
  pds$dotgit <- ""
  pds$commitish[pds$commitish == ""] <- "HEAD"
  pds$url <- paste0(pds$protocol, "://", pds$host, pds$path, ".git")
  cn <- setdiff(colnames(pds), c(".match", ".text"))
  pds <- pds[, cn]
  pds$type <- "gitlab"
  pds$package <- ifelse(nzchar(pds$package), pds$package, pds$project)
  lapply(
    seq_len(nrow(pds)),
    function(i) as.list(pds[i, ])
  )
}

resolve_remote_gitlab <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  resolve_remote_git(remote, direct, config, cache, dependencies, ...)$then(
    function(res) {
      res$metadata["RemoteHost"] <- remote$host
      res$metadata["RemoteRepo"] <- remote$project
      res$metadata["RemoteUsername"] <- remote$projectpath
      res$metadata["RemoteType"] <- "gitlab"
      if (!is.null(remote$subdir) && remote$subdir != "") {
        res$metadata["RemoteSubdir"] <- remote$subdir
      }
      res
    }
  )
}

download_remote_gitlab <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  download_remote_git(
    resolution,
    target,
    target_tree,
    config,
    cache,
    which,
    on_progress
  )
}

satisfy_remote_gitlab <- function(resolution, candidate, config, ...) {
  satisfy_remote_git(resolution, candidate, config, ...)
}

installedok_remote_gitlab <- function(installed, solution, config, ...) {
  installedok_remote_git(installed, solution, config, ...)
}

# source: https://docs.gitlab.com/ee/user/reserved_names.html#limitations-on-usernames-project-and-group-names
gitlab_slug_rx <- function() {
  "[a-zA-Z0-9][-._a-zA-Z0-9]*[a-zA-Z0-9]"
}

gitlab_project_rx <- function() {
  paste0("(?<project>", gitlab_slug_rx(), ")")
}

gitlab_project_path_rx <- function() {
  paste0("(?<projectpath>", gitlab_slug_rx(), "(?:/", gitlab_slug_rx(), ")*)")
}

gitlab_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>",
    package_name_rx(),
    ")=)?",
    "gitlab::",
    ## Optional protocol::host
    "(?:(?<protocol>[^/]*)://(?<host>[^/]+)/)?",
    gitlab_project_path_rx(),
    "/",
    gitlab_project_rx(),
    ## Optional subdirectory, prefixed with /-, ie project/-/sub/dir
    "(?:/-",
    github_subdir_rx(),
    ")?",
    "(?:",
    github_commitish_rx(),
    ")?",
    "$"
  )
}
