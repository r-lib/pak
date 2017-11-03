#' Activiate a repository
#'
#' @param ... One or more repositories to set. If a repository is unnamed it is
#'   assumed to be the CRAN repository.
#' @export
repo_activate <- function(...) {
  repo <- unlist(list(...))
  nms <- names2(repo)

  if (sum(nms == "") > 1) {
    stop("Can have at most one unnamed repository", call. = FALSE)
  }
  nms[nms == ""] <- "CRAN"
  names(repo) <- nms

  old <- getOption("repos")
  both <- c(repo, old)

  # Remove duplicates, keeping the new values
  both <- both[!duplicated(names(both))]

  options(repos = both)
  invisible(old)
}

#' Deactivate a repository
#'
#' @param repo name of repository to deactivate. If no repositories are left
#'   after deactivation `"https://cloud.r-project.org"` is set.
#' @export
repo_deactivate <- function(repo) {
  old <- getOption("repos")
  repos <- old[setdiff(names(old), repo)]

  # if no more repos, just set the cloud repo
  if (length(repos) == 0) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }

  options(repos = repos)
  invisible(old)
}

#' Repository status
#' @export
repo_status <- function() {
  repos <- getOption("repos")
  data.frame(type = names(repos), url = repos, row.names = NULL)
}
