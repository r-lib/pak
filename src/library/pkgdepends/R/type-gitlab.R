
parse_remote_gitlab <- function(specs, config, ...) {

  pds <- re_match(specs, gitlab_rx())
  pds$ref <- pds$.text
  pds$protocol[pds$protocol == ""] <- "https"
  pds$host[pds$host == ""] <- "gitlab.com"
  pds$path <- paste0("/", pds$username, "/")
  pds$dotgit <- ""
  pds$commitish[pds$commitish == ""] <- "HEAD"
  pds$url <- paste0(pds$protocol, "://", pds$host, pds$path, pds$repo, ".git")
  cn <- setdiff(colnames(pds), c(".match", ".text"))
  pds <- pds[, cn]
  pds$type <- "gitlab"
  pds$package <- ifelse(nzchar(pds$package), pds$package, pds$repo)
  lapply(
    seq_len(nrow(pds)),
    function(i) as.list(pds[i,])
  )
}

resolve_remote_gitlab <- function(remote, direct, config, cache,
                                  dependencies, ...) {
  resolve_remote_git(remote, direct, config, cache, dependencies, ...)$
    then(function(res) {
      res$metadata["RemoteHost"] <- remote$host
      res$metadata["RemoteRepo"] <- remote$repo
      res$metadata["RemoteUsername"] <- remote$username
      res$metadata["RemoteType"] <- "gitlab"
      if (!is.null(remote$subdir) && remote$subdir != "") {
        res$metadata["RemoteSubdir"] <- remote$subdir
      }
      res
    })
}

download_remote_gitlab <- function(resolution, target, target_tree,
                                   config, cache, which, on_progress) {
  download_remote_git(
    resolution,
    target,
    target_tree,
    config, cache,
    which,
    on_progress
  )
}

satisfy_remote_gitlab <- function(resolution, candidate,
                                  config, ...) {
  satisfy_remote_git(resolution, candidate, config, ...)
}

installedok_remote_gitlab <- function(installed, solution, config, ...) {
  installedok_remote_git(installed, solution, config, ...)
}

gitlab_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    "gitlab::",
    "(?:(?<protocol>[^/]*)://(?<host>[^/]+))?",
    github_username_rx(), "/",
    github_repo_rx(),
    github_subdir_rx(), "?",
    "(?:", github_commitish_rx(), ")?",
    "$"
  )
}
