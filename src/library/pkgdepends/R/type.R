default_remote_types <- function() {
  default <- list(
    cran = list(
      parse = parse_remote_cran,
      resolve = resolve_remote_cran,
      download = download_remote_cran,
      satisfy = satisfy_remote_cran,
      installedok = installedok_remote_cran
    ),
    bioc = list(
      parse = parse_remote_bioc,
      resolve = resolve_remote_bioc,
      download = download_remote_bioc,
      satisfy = satisfy_remote_bioc,
      installedok = installedok_remote_bioc
    ),
    standard = list(
      parse = parse_remote_standard,
      resolve = resolve_remote_standard,
      download = download_remote_standard,
      satisfy = satisfy_remote_standard,
      installedok = installedok_remote_standard
    ),
    git = list(
      parse = parse_remote_git,
      resolve = resolve_remote_git,
      download = download_remote_git,
      satisfy = satisfy_remote_git,
      installedok = installedok_remote_git
    ),
    github = list(
      parse = parse_remote_github,
      resolve = resolve_remote_github,
      download = download_remote_github,
      satisfy = satisfy_remote_github,
      installedok = installedok_remote_github
    ),
    gitlab = list(
      parse = parse_remote_gitlab,
      resolve = resolve_remote_gitlab,
      download = download_remote_gitlab,
      satisfy = satisfy_remote_gitlab,
      installedok = installedok_remote_gitlab
    ),
    local = list(
      parse = parse_remote_local,
      resolve = resolve_remote_local,
      download = download_remote_local,
      satisfy = satisfy_remote_local,
      installedok = installedok_remote_local
    ),
    deps = list(
      parse = parse_remote_deps,
      resolve = resolve_remote_deps,
      download = download_remote_deps,
      satisfy = satisfy_remote_deps,
      installedok = installedok_remote_deps
    ),
    installed = list(
      parse = parse_remote_installed,
      resolve = resolve_remote_installed,
      download = download_remote_installed,
      satisfy = satisfy_remote_installed,
      installedok = installedok_remote_installed
    ),
    url = list(
      parse = parse_remote_url,
      resolve = resolve_remote_url,
      download = download_remote_url,
      satisfy = satisfy_remote_url,
      installedok = installedok_remote_url
    ),
    any = list(
      parse = parse_remote_any,
      resolve = resolve_remote_any,
      download = download_remote_any,
      satisfy = satisfy_remote_any,
      installedok = installedok_remote_any
    ),
    param = list(
      parse = parse_remote_param,
      resolve = resolve_remote_param,
      download = download_remote_param,
      satisfy = satisfy_remote_param,
      installedok = installedok_remote_param
    )
  )

  modifyList(default, as.list(getOption("pkg.remote_types")))
}
