#' A set of handy regular expressions related to R packages
#'
#' If you use these in R, make sure you specify `perl = TRUE`, see
#' [base::grep()].
#'
#' Currently included:
#' - `pkg_name`: A valid package name.
#' - `type_cran`: A `cran::` package reference.
#' - `type_bioc`: A `bioc::` package reference.
#' - `type_standard`: A `standard::` package reference.
#' - `type_github`: A `github::` package reference.
#' - `type_git`: A `git::` package reference.
#' - `type_local`: A `local::` package reference.
#' - `type_deps`: A `deps::` package reference.
#' - `type_installed`: An `installed::` package reference.
#' - `github_username`: A GitHub username.
#' - `github_repo`: A GitHub repository name.
#' - `github_url`: A GitHub URL.
#'
#' @return A named list of strings.
#'
#' @export
#' @examples
#' pkg_rx()

pkg_rx <- function() {
  list(
    pkg_name = package_name_rx(),
    type_cran = standard_rx("cran"),
    type_bioc = standard_rx("bioc"),
    type_standard = standard_rx("standard"),
    type_github = github_rx(),
    type_git = git_rx(),
    type_local = local_rx(),
    type_deps = type_deps_rx(),
    type_installed = type_installed_rx(),
    github_username = github_username_rx(),
    github_repo = github_repo_rx(),
    github_url = github_url_rx()
  )
}
