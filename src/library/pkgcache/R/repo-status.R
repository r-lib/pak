
#' Show the status of CRAN-like repositories
#'
#' It checks the status of the configured or supplied repositories,
#' for the specified platforms and R versions.
#'
#' The returned data frame has a `summary()` method, which shows
#' the same information is a concise table. See examples below.
#'
#' @param platforms Platforms to use, default is [default_platforms()].
#' @param r_version R version(s) to use, the default is the current
#'   R version, via [getRversion()].
#' @param bioc Whether to add the Bioconductor repositories. If you
#'   already configured them via `options(repos)`, then you can
#'   set this to `FALSE`. See [bioc_version()] for the details about
#'   how pkgcache handles Bioconductor repositories.
#' @param cran_mirror The CRAN mirror to use, see
#'   [default_cran_mirror()].
#' @return A data frame that has a row for every repository, on every
#' queried platform and R version. It has these columns:
#' * `name`: the name of the repository. This comes from the names
#'   of the configured repositories in `options("repos")`, or
#'   added by pkgcache. It is typically `CRAN` for CRAN, and the
#'   current Bioconductor repositories are `BioCsoft`, `BioCann`,
#'   `BioCexp`, `BioCworkflows`, `BioCbooks`.
#' * `url`: base URL of the repository.
#' * `bioc_version`: Bioconductor version, or `NA` for
#'   non-Bioconductor repositories.
#' * `platform`: platform, see [default_platforms()] for possible values.
#' * `path`: the path to the packages within the base URL, for a
#'   given platform and R version.
#' * `r_version`: R version, one of the specified R versions.
#' * `ok`: Logical flag, whether the repository contains a metadata
#'   file for the given platform and R version.
#' * `ping`: HTTP response time of the repository in seconds. If
#'   the `ok` column is `FALSE`, then this columns in `NA`.
#' * `error`: the error object if the HTTP query failed for this
#'   repository, platform and R version.
#'
#' @family repository functions
#' @export
#' @examplesIf pkgcache:::run_examples()
#' repo_status()
#' rst <- repo_status(
#'   platforms = c("windows", "macos"),
#'   r_version = c("4.0", "4.1")
#' )
#' summary(rst)

repo_status <- function(platforms = default_platforms(),
                        r_version = getRversion(),
                        bioc = TRUE,
                        cran_mirror = default_cran_mirror()) {
  synchronise(async_repo_status(
    platforms,
    r_version,
    bioc,
    cran_mirror
  ))
}

async_repo_status <- function(platforms = default_platforms(),
                              r_version = getRversion(),
                              bioc = TRUE,
                              cran_mirror = default_cran_mirror()) {
  r_version <- get_minor_r_version(r_version)
  repos <- cmc__get_repos(getOption("repos"), bioc, cran_mirror, r_version)
  dirs <- get_all_package_dirs(platforms, r_version)
  dirs <- dirs[, c("platform", "rversion", "contriburl")]
  names(dirs) <- sub("rversion", "r_version", names(dirs))
  names(dirs) <- sub("contriburl", "path", names(dirs))

  # We need to make sure that * is a wildcard character, both in
  # repos and in dirs. We replicate wildcard dirs for all R versions,
  # then we match * r_version lines in repos to all dirs.

  wc_dirs <- merge(
    subset(dirs[dirs$r_version == "*", ], select = -r_version),
    data_frame(r_version = r_version)
  )

  dirs <- rbind(wc_dirs, dirs[dirs$r_version != "*", ])

  wc_sts <- merge(
    subset(repos[repos$r_version == "*", ], select = -r_version),
    dirs,
    by = character()
  )

  nwc_sts <- merge(
    subset(repos[repos$r_version != "*", ]),
    dirs,
    by = "r_version"
  )

  sts <- as_data_frame(rbind(wc_sts, nwc_sts))

  fns <- paste0("PACKAGES", c(".rds", ".gz", ""))
  urls <- mapx(sts$url, "/", sts$path, "/", list(fns), paste0)

  ping <- function(u) http_head(u)$then(http_stop_for_status)
  ping_any <- function(us) {
    when_any(.list = lapply(us, ping))$
      catch(error = function(err) err)$
      finally(function() done <<- done + 1L)
  }

  done <- 0L
  todo <- nrow(sts)

  reqs <- when_all(.list = lapply(urls, ping_any))

  reqs$then(
    function(res) {
      sts$ok <- !vlapply(res, inherits, "error")
      sts$ping <- NA_real_
      sts$ping[sts$ok] <- vdapply(
        res[sts$ok],
        function(r) r$times[["total"]]
      )
      sts$error <- list(NULL)
      sts$error[!sts$ok] <- res[!sts$ok]
      class(sts) <- c("pkgcache_repo_status", class(sts))
      sts
    }
  )
}

#' @export

summary.pkgcache_repo_status <- function(object, ...) {

  srv <- sub("^https?://([^/]*).*$", "\\1", object$url)
  if (length(unique(object$r_version)) == 1) {
    key <- paste0(format(object$name), " @ ", srv)
  } else {
    key <- paste0(
      format(object$name), " @ ", format(srv),
      " (R ", object$r_version, ")"
    )
  }
  ssm <- data_frame(repository = unique(key))

  pls <- unique(object$platform)
  for (pl in pls) ssm[[pl]] <- TRUE
  ssm$ping <- NA_real_

  for (i in seq_len(nrow(ssm))) {
    ssm$ping[i] <- mean(object$ping[key == ssm$repository[i]])
    for (pl in pls) {
      ok <- object$ok[key == ssm$repository[i] & object$platform == pl]
      ssm[[pl]][i] <- if (length(ok) == 0) NA else ok
    }
  }

  class(ssm) <- c("pkgcache_repo_status_summary", class(ssm))
  ssm
}

#' @export

print.pkgcache_repo_status_summary <- function(x, ...) {

  repo <- format(c("Repository summary:", x$repository))
  ping <- format(
    c("", paste0("   (", format(format_time$pretty_sec(x$ping)), ")")),
    justify = "right"
  )
  ping <- sub("\\(NA.*\\)", "  ", ping)

  if (cli::is_utf8_output()) {
    symbol_ok <- cli::symbol$tick
    symbol_notok <- cli::symbol$cross
  } else {
    symbol_ok <- "OK"
    symbol_notok <- "--"
  }

  pls <- setdiff(names(x), c("repository", "ping"))
  pl_strs <- lapply(pls, function(pl) {
    s <- format(
      c(pl, ifelse(!is.na(x[[pl]]) & x[[pl]], symbol_ok, symbol_notok)),
      justify = "centre"
    )
    s[-1] <- ifelse(!is.na(x[[pl]]) & x[[pl]], cli::col_green(s[-1]), cli::col_red(s[-1]))
    paste0(" ", s, "")
  })
  pl_str <- do.call("paste0", pl_strs)

  cat(paste0(repo, "  ", pl_str, cli::col_cyan(ping)), sep = "\n")
  invisible(x)
}

#' @export

`[.pkgcache_repo_status_summary` <- function (x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pkgcache_repo_status_summary")
  NextMethod("[")
}
