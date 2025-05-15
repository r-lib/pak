#' Package downloads
#'
#' The [`pkg_download_proposal`] and [`pkg_installation_proposal`] classes
#' both have download methods, to downloads package files into a
#' configured directory (see ['Configuration'][pkgdepends-config]).
#'
#' They return a `pkg_download_result` object, which is a data frame,
#' that adds extra columns to [`pkg_resolution_result`] (for
#' [`pkg_download_proposal`]) or [`pkg_solution_result`]
#' (for [`pkg_installation_proposal`]):
#'
#' ```{r child = {options(rx_downloads = TRUE); "tools/doc/resolution-result.Rmd" }}
#' ```
#' `r { options(rx_downloads = TRUE); doc_share_rmd("tools/doc/resolution-result.Rmd", "inst/docs/download-result.rds")}`
#'
#' @name pkg_downloads
#' @aliases pkg_download_result
NULL

pkgplan_download_resolution <- function(self, private) {
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) {
    throw(pkg_error(
      "Package list has changed, you need to call the {.code $resolve()}
       method again?"
    ))
  }
  on.exit(private$done_progress_bar(), add = TRUE)
  on.exit(cli::ansi_show_cursor(), add = TRUE)
  cli::ansi_hide_cursor()
  synchronise(self$async_download_resolution())
}

pkgplan_async_download_resolution <- function(self, private) {
  self
  private
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) {
    throw(pkg_error(
      "Package list has changed, you need to call the {.code $resolve()}
       method again?"
    ))
  }

  pkgplan_async_download_internal(
    self,
    private,
    private$resolution$result,
    "resolution"
  )$then(function(value) {
    private$downloads <- value
    self$get_resolution_download()
  })
}

pkgplan_download_solution <- function(self, private) {
  if (is.null(private$solution)) self$solve()
  if (private$dirty) {
    throw(pkg_error(
      "Package list has changed, you need to call the {.code $resolve()}
       method again?"
    ))
  }
  on.exit(private$done_progress_bar(), add = TRUE)
  on.exit(cli::ansi_show_cursor(), add = TRUE)
  cli::ansi_hide_cursor()
  synchronise(self$async_download_solution())
}

pkgplan_async_download_solution <- function(self, private) {
  if (is.null(private$solution)) self$solve()
  if (private$dirty) {
    throw(pkg_error(
      "Package list has changed, you need to call the {.code $resolve()}
       method again?"
    ))
  }

  pkgplan_async_download_internal(
    self,
    private,
    private$solution$result$data,
    "solution"
  )$then(function(value) {
    private$solution_downloads <- value
    self$get_solution_download()
  })
}

pkgplan_stop_for_solution_download_error <- function(self, private) {
  dl <- self$get_solution_download()
  if (any(bad <- tolower(dl$download_status) == "failed")) {
    msgs <- vcapply(
      which(bad),
      function(i) {
        urls <- format_items(dl$sources[[i]])
        sprintf("Failed to download %s from %s.", dl$package[i], urls)
      }
    )
    msg <- paste(msgs, collapse = "\n")
    err <- structure(
      list(message = msg, call = NULL, errors = dl$download_errors[bad]),
      class = c("error", "condition")
    )
    stop(err)
  }
}

pkgplan_stop_for_resolution_download_error <- function(self, private) {
  dl <- self$get_resolution_download()
  if (any(bad <- tolower(dl$download_status) == "failed")) {
    msgs <- vcapply(
      which(bad),
      function(i) {
        urls <- format_items(dl$sources[[i]])
        sprintf("Failed to download %s from %s.", dl$package[i], urls)
      }
    )
    msg <- paste(msgs, collapse = "\n")
    err <- structure(
      list(message = msg, call = NULL),
      class = c("error", "condition")
    )
    stop(err)
  }
}

pkgplan_async_download_internal <- function(self, private, what, which) {
  if (any(what$status != "OK")) {
    stop("Resolution has errors, cannot start downloading")
  }
  start <- Sys.time()
  private$progress_bar <- private$create_progress_bar(what)

  dl <- lapply(seq_len(nrow(what)), function(idx) {
    force(idx)
    private$download_res(
      what[idx, ],
      which = which,
      on_progress = function(data) private$update_progress_bar(idx, "got", data)
    )$then(function(x) {
      private$update_progress_bar(idx, "done", x)
      x
    })$catch(error = function(x) private$update_progress_bar(idx, "error", x))
  })

  when_all(.list = dl)$then(function(dls) {
    what$fulltarget <- vcapply(dls, "[[", "fulltarget")
    what$fulltarget_tree <- vcapply(dls, "[[", "fulltarget_tree")
    what$download_status <- vcapply(dls, "[[", "download_status")
    what$download_error <- lapply(dls, function(x) x$download_error[[1]])
    what$file_size <- vdapply(dls, "[[", "file_size")
    what$used_cached_binary <- vlapply(dls, "[[", "used_cached_binary")
    class(what) <- c("pkgplan_downloads", class(what))
    attr(what, "metadata")$download_start <- start
    attr(what, "metadata")$download_end <- Sys.time()
    what
  })$finally(function() private$done_progress_bar())
}

pkgplan_download_res <- function(self, private, res, which, on_progress) {
  force(private)
  download_remote(
    res,
    config = private$config,
    cache = private$cache,
    which = which,
    on_progress = on_progress
  )
}

download_remote <- function(
  res,
  config,
  cache,
  which,
  on_progress = NULL,
  remote_types = NULL
) {
  remote_types <- c(default_remote_types(), remote_types)
  dl <- remote_types[[res$type]]$download %||% type_default_download
  target <- file.path(config$get("cache_dir"), res$target)
  target_tree <- file.path(config$get("cache_dir"), paste0(res$target, "-t"))
  mkdirp(dirname(target))
  async(dl)(
    res,
    target,
    target_tree,
    config,
    cache = cache,
    which = which,
    on_progress = on_progress
  )$then(function(s) {
    if (
      length(res$sources[[1]]) &&
        !file.exists(target) &&
        !file.exists(target_tree)
    ) {
      stop("Failed to download ", res$type, " package ", res$package)
    }

    dlres <- res
    if (!grepl("^Had", s) && !identical(s, "Got") && !identical(s, "Current"))
      s <- "Got"
    if (grepl("^Had-binary-", s)) {
      dlres$used_cached_binary <- TRUE
      s <- "Had"
    } else {
      dlres$used_cached_binary <- FALSE
    }
    dlres$fulltarget <- target
    dlres$fulltarget_tree <- target_tree
    dlres$download_status <- s
    dlres$download_error <- list(NULL)
    dlres$file_size <- file.size(target)
    dlres
  })$catch(error = function(err) {
    dlres <- res
    dlres$used_cached_binary <- NA
    dlres$fulltarget <- target
    dlres$fulltarget_tree <- target_tree
    dlres$download_status <- "Failed"
    dlres$download_error <- list(err)
    dlres$file_size <- NA_integer_
    dlres
  })
}

download_ping_if_not_source <- function(
  resolution,
  target,
  config,
  cache,
  on_progress
) {
  resolution
  target
  config
  cache
  on_progress
  mkdirp(dirname(target))

  if (is_true_param(resolution$params[[1]], "nocache")) {
    ## If the cache is ignored, then we just download it and put it
    ## at the right place
    download_one_of(
      resolution$sources[[1]],
      target,
      on_progress = on_progress
    )$then(function() "Got")
  } else if (resolution$platform == "source") {
    ## If it is a source package, then the package name, version number
    ## and package type must match. If there is such a package in the cache
    ## we just take it
    cache$package$async_copy_or_add(
      target,
      resolution$sources[[1]],
      path = resolution$target,
      package = resolution$package,
      version = resolution$version,
      platform = resolution$platform,
      on_progress = on_progress,
      http_headers = default_download_headers(resolution$sources[[1]])
    )$then(function(.) attr(., "action"))
  } else {
    ## If not a source package, then we try to update it, in case there is
    ## a newly built binary
    cache$package$async_update_or_add(
      target,
      resolution$sources[[1]],
      path = resolution$target,
      package = resolution$package,
      version = resolution$version,
      platform = resolution$platform,
      on_progress = on_progress,
      http_headers = default_download_headers(resolution$sources[[1]])
    )$then(function(.) attr(., "action"))
  }
}

download_ping_if_no_sha <- function(
  resolution,
  target,
  config,
  cache,
  on_progress
) {
  resolution
  target
  config
  cache
  on_progress
  mkdirp(dirname(target))

  # If the cache is ignored, then we download it, always
  if (is_true_param(resolution$params[[1]], "nocache")) {
    return(
      download_one_of(
        resolution$sources[[1]],
        target,
        on_progress = on_progress
      )$then(function() "Got")
    )
  }

  # If we wanted to _install_ a source package, then look for a
  # locally built binary in the cache
  rver <- config$get("r-versions")
  if (
    resolution$platform == "source" &&
      config$get("goal") == "install" &&
      !is_true_param(resolution$params[[1]], "source") &&
      length(rver) == 1
  ) {
    ## Try to find a binary in the cache
    cplt <- current_r_platform()
    bin <- cache$package$copy_to(
      target,
      package = resolution$package,
      version = resolution$version,
      platform = cplt,
      built = TRUE,
      rversion = rver
    )
    if (nrow(bin)) {
      return(async_constant(paste0("Had-binary-", cplt)))
    }
  }

  if (!"sha256" %in% names(resolution) || is.na(resolution$sha256)) {
    ## Otherwise we need to ping or download a package
    ## If we don't know the hash of the CRAN package, then just download
    ## it. This happens if there is some discrepancy between the package
    ## data and the metadata.
    cache$package$async_update_or_add(
      target,
      resolution$sources[[1]],
      path = resolution$target,
      package = resolution$package,
      version = resolution$version,
      platform = resolution$platform,
      on_progress = on_progress,
      http_headers = default_download_headers(resolution$sources[[1]])
    )$then(function(.) attr(., "action"))
  } else {
    ## There is a sha hash in the metadata, so we can search for that
    ## in the package cache.
    cache$package$async_copy_or_add(
      target,
      resolution$sources[[1]],
      path = resolution$target,
      package = resolution$package,
      version = resolution$version,
      platform = resolution$platform,
      sha256 = resolution$sha256,
      on_progress = on_progress,
      http_headers = default_download_headers(resolution$sources[[1]])
    )$then(function(.) attr(., "action"))
  }
}

pkgplan_get_resolution_download <- function(self, private) {
  if (is.null(private$downloads)) stop("No downloads")
  private$downloads
}

pkgplan_get_solution_download <- function(self, private) {
  if (is.null(private$solution_downloads)) stop("No downloads")
  private$solution_downloads
}

#' @export

`[.pkgplan_downloads` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pkgplan_downloads")
  NextMethod("[")
}

type_default_download <- function(
  resolution,
  target,
  config,
  cache,
  on_progress
) {
  ## TODO
  stop("Not implemented yet")
}

default_download_headers <- function(url) {
  if (any(grepl("^https://ghcr.io", url))) {
    c("Authorization" = "Bearer QQ==")
  }
}
