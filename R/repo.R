#' Activiate a repository
#'
#' @param repo name of repository to activate.
#' @param ... One or more repositories to set. If a repository is unnamed it is
#'   assumed to be the CRAN repository.
#' @export
repo_activate <- function(repo) {
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
#'
#' @param repos Repositories of which to check status.
#' @return A data.frame with the following columns
#' - type - The repository type
#' - url - The repository URL
#' - time - Total time in seconds taken for a HEAD request to the repositiory
#' - last_modified - Last time the repositiory was updated
#' @export
repo_status <- function(repos = getOption("repos")) {

  # TODO: how to hangdle
  resp_vals <- async::async(function(url, ...) {
    async::http_head(url, ...)$
      then(async::http_stop_for_status)$
      then(function(resp) list(time = resp$times[["total"]], last_modified = resp$modified))$
      catch(function(err) list(time = NA_real_, last_modified = as.POSIXct(NA)))
  })

  res <- async::synchronise(async_map(unname(repos), resp_vals, timeout = 2))
  time <- vdapply(res, "[[", "time")
  last_modified <- do.call(c, lapply(res, "[[", "last_modified"))

  tibble::tibble(type = names(repos), url = repos, time = time,
                 last_modified = last_modified)
}
