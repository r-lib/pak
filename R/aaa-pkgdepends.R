
is_character <- function(x) {
  is.character(x) && ! any(is.na(x))
}

is_character <- assertthat::`on_failure<-`(
  is_character, function(call, env) {
    paste0(deparse(call$x), " must be a character vector without NAs")
  })

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_string <- assertthat::`on_failure<-`(
  is_string, function(call, env) {
    paste0(deparse(call$x), " is not a string (length 1 character)")
  })

is_string_or_null <- function(x) {
  is.null(x) || is_string(x)
}

is_string_or_null <- assertthat::`on_failure<-`(
  is_string_or_null, function(call, env) {
    paste0(deparse(call$x), " must be a string (length 1 character) or NULL")
  })

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_flag <- assertthat::`on_failure<-`(
  is_flag, function(call, env) {
    paste0(deparse(call$x), " is not a flag (length 1 logical)")
  })

## To be refined

is_path <- function(x) {
  is_string(x)
}

is_path <- assertthat::`on_failure<-`(
  is_path, function(call, env) {
    paste0(deparse(call$x), " must be a path")
  })

is_path_or_null <- function(x) {
  is_string_or_null(x)
}

is_path_or_null <- assertthat::`on_failure<-`(
  is_path_or_null, function(call, env) {
    paste0(deparse(call$x), " must be a path or NULL")
  })

## is_valid_config is in remotes.R, as the configuration parameters
## are there

all_named <- function(x) {
  length(names(x)) == length(x) && all(names(x) != "")
}

all_named <- assertthat::`on_failure<-`(
  all_named, function(call, env) {
    paste0(deparse(call$x), " must be a list of named entries")
  })

is_existing_file <- function(x) {
  assertthat::assert_that(is_path(x))
  file.exists(x) && ! file.info(x)$isdir
}

is_existing_file <- assertthat::`on_failure<-`(
  is_existing_file, function(call, env) {
    paste0("File ", deparse(call$x), " does not exist")
  })

is_platform_list <- function(x) {
  is.character(x) && length(x) > 0 && ! any(is.na(x))
}

is_platform_list <- assertthat::`on_failure<-`(
  is_platform_list, function(call, env) {
    paste0(deparse(call$x), " must be a non-empty characater vector")
  })

is_dependencies <- function(x) {
  is_na_scalar(x) || isTRUE(x) || identical(x, FALSE) ||
    (is_character(x) && all(x %in% dep_types())) ||
    (is.list(x) && all(names(x) == c("direct", "indirect")) &&
     all(unlist(x) %in% dep_types()))
}

is_dependencies <- assertthat::`on_failure<-`(
  is_dependencies, function(call, env) {
    paste0(
      deparse(call$x),
      " must be TRUE, FALSE, NA or a list of dependency types"
    )
  })

is_r_version_list <- function(x) {
  if (is_character(x) && length(x) > 0) {
    tryCatch({
      package_version(x)
      TRUE
    }, error = function(e) FALSE)
  } else {
    FALSE
  }
}

is_r_version_list <- assertthat::`on_failure<-`(
  is_r_version_list, function(call, env) {
    paste0(deparse(call$x), " must be a list or R version numbers")
  })

build_package <- function(path, build_args = list()) {
  default_args <- list(
    path = path, dest_path = NULL, binary = FALSE, vignettes = TRUE,
    manual = TRUE, args = NULL, quiet = TRUE
  )
  args <- utils::modifyList(default_args, build_args)
  do.call(pkgbuild::build, args)
}

dep_types_hard <- function() c("Depends", "Imports", "LinkingTo")
dep_types_soft <- function() c("Suggests", "Enhances")
dep_types <- function() c(dep_types_hard(), dep_types_soft())

fast_parse_deps <- function(pkgs) {
  no_pkgs <- nrow(pkgs)
  cols <- intersect(colnames(pkgs), dep_types())
  ## as.character is for empty tibbles, e.g. from empty BioC repos
  deps <- as.character(unlist(pkgs[, cols], use.names = FALSE))
  nna <- which(!is.na(deps))
  if (length(nna)) {
    not_na_deps <- deps[nna]
    sp <- strsplit(not_na_deps, ",", fixed = TRUE)
    ll <- sapply(sp, length, USE.NAMES = FALSE)
    sp <- unlist(sp, use.names = FALSE)
    parsed <- rematch2::re_match(sp,
      paste0("^\\s*(?<package>[^(\\s]+)\\s*",
             "(?:\\((?<op>[^0-9\\s]+)\\s*(?<version>[^)\\s]+)\\))?\\s*$"))
    parsed$idx <- rep(rep(seq_len(no_pkgs), length(cols))[nna], ll)
    parsed$type <- rep(rep(cols, each = no_pkgs)[nna], ll)
    parsed$ref <- parsed$package
    parsed$upstream <- pkgs$Package[parsed$idx]
    parsed <- parsed[, c("upstream", "idx", "ref", "type", "package",
                         "op", "version")]
    parsed <- parsed[! parsed$package %in% base_packages(), ]
    parsed <- parsed[order(parsed$idx), ]

  } else {
    parsed <- tibble::tibble(
      upstream = character(),
      idx = integer(),
      ref = character(),
      type = character(),
      package = character(),
      version = character(),
      op = character())
  }

  parsed
}

fast_select_deps <- function(deps, which, dependencies) {
  res <- deps[deps$idx == which, ]
  res <- res[res$type %in% dependencies,
             c("ref", "type", "package", "op", "version")]
  res[! res$package %in% base_packages(), ]
}

make_null_deps <- function() {
  tibble::tibble(
    ref = character(), type = character(), package = character(),
    op = character(), version = character())
}

parse_deps <- function(deps, type) {
  assertthat::assert_that(length(deps) == length(type))
  deps <- lapply(strsplit(deps, ","), str_trim)
  rx <- paste0(
    "(?<type>)",
    "^\\s*",
    "(?<package>[^\\s]+)",
    "\\s*",
    "(?:[(](?<op>>|>=|==|<|<=)\\s*(?<version>[-0-9\\.]+)[)])?\\s*$"
  )
  base <- base_packages()
  lapply(seq_along(deps), function(i) {
    x <- omit_cols(rematch2::re_match(deps[[i]], pattern = rx), c(".text", ".match"))
    x$type <- if (length(x$type) > 0) type[[i]] else character()
    x[! x$package %in% base, ]
  })
}

deps_from_desc <- function(deps, last) {
  op_ver <- strsplit(deps$version, "\\s+")
  deps$op <- vcapply(op_ver, "[", 1)
  deps$op[deps$op == "*"] <- ""
  deps$version <- vcapply(op_ver, "[", 2)
  deps$version[is.na(deps$version)] <- ""
  deps$ref <- paste0(deps$package, if (last) "@last")
  base <- base_packages()
  res <- tibble::as_tibble(deps[!deps$package %in% base,
                        c("ref", "type", "package", "op", "version")])
  rownames(res) <- NULL
  res
}

parse_all_deps <- function(deps) {
  deps <- stats::na.omit(deps)
  res <- do.call(rbind, parse_deps(deps, names(deps)))
  if (is.null(res)) res <- parse_deps("", "")[[1]]
  res$ref <- res$package
  res[, c("ref", setdiff(names(res), "ref"))]
}

get_cran_extension <- function(platform) {
  switch(
    platform,
    "source" = ".tar.gz",
    "macos" = ".tgz",
    "windows" = ".zip",
    stop("Unknown platform: ", sQuote(platform))
  )
}

resolve_ref_deps <- function(deps, remotes) {
  deps <- deps_from_desc(deps, last = FALSE)

  if (is.na(remotes)) return (deps)

  parse <- function(x) {
    str_trim(strsplit(x, "\\s*,\\s*", perl = TRUE)[[1]])
  }

  remotes <- str_trim(stats::na.omit(remotes))
  remotes <- parse(remotes)
  remotes_packages <- vcapply(parse_remotes(remotes), "[[", "package")
  keep <- which(remotes_packages %in% deps$package)
  deps$ref[match(remotes_packages[keep], deps$package)] <- remotes[keep]
  deps
}

interpret_dependencies <- function(dp) {
  hard <- dep_types_hard()

  res <- if (isTRUE(dp)) {
    list(c(hard, "Suggests"), hard)

  } else if (identical(dp, FALSE)) {
    list(character(), character())

  } else if (is_na_scalar(dp)) {
    list(hard, hard)

  } else if (is.list(dp) && all(names(dp) == c("direct", "indirect"))) {
    dp

  } else {
    list(dp, dp)
  }

  names(res) <- c("direct", "indirect")
  res
}

remotes__create_progress_bar <- function(self, private, what) {
  if (!is_verbose()) return(NULL)
  bar <- list()

  what$status <- NA_character_
  what$current <- NA_integer_
  what$finished_at <- NA_real_
  what$reported <- FALSE

  bar$what <- what[, c("ref", "type", "status", "filesize", "current",
                       "finished_at", "direct", "package", "reported",
                       "cache_status")]
  bar$spinner <- cli::get_spinner()
  bar$spinner_state <- 1L
  bar$chars <- progress_chars()

  bar$bar <- cliapp::cli_progress_bar(
    show_after = 0,
    format = ":xbar :xpkgs :xbytes :xspin :xmsg",
    total = nrow(what),
    force = TRUE)

  private$progress_bar_timer <-
    asNamespace("pkgcache")$async_timer$new(
      1/10, function() private$show_progress_bar())
  private$progress_bar_timer$listen_on("error", function(...) { })

  bar
}

remotes__update_progress_bar <- function(self, private, idx, data) {
  if (!is_verbose()) return(NULL)

  if (identical(data, "done")) {
    private$progress_bar$what$status[idx] <- "DONE"
    private$progress_bar$what$finished_at[idx] <- Sys.time()
  } else {
    ## TODO: redirects!
    total <- private$progress_bar$what$filesize[idx]
    if (data$total > 0) {
      private$progress_bar$what$filesize[idx] <- data$total
    }
    private$progress_bar$what$current[idx] <- data$current
    if (data$total && data$total == data$current) {
      private$progress_bar$what$status[idx] <- "DONE"
      private$progress_bar$what$finished_at[idx] <- Sys.time()
    }
  }
}

remotes__show_progress_bar <- function(self, private) {
  if (!is_verbose()) return()
  bar <- private$progress_bar
  what <- bar$what[! bar$what$type %in% c("installed", "deps"), ]
  what <- what[what$cache_status == "miss", ]
  pkg_done <- sum(!is.na(what$status))
  pkg_total <- nrow(what)
  percent <- pkg_done / pkg_total
  bytes_done <- sum(what$current, na.rm = TRUE)
  bytes_total <- sum(what$filesize)

  tokens <- list(
    xbar = make_bar_pkgdepends(bar$chars, percent, width = 15),
    xpkgs = make_progress_packages(pkg_done, pkg_total),
    xbytes = make_progress_bytes(bytes_done, bytes_total),
    xspin = make_spinner(private),
    xmsg = make_trailing_download_msg(bar)
  )

  bar$bar$tick(0, tokens = tokens)
}

remotes__done_progress_bar <- function(self, private) {
  if (!is_verbose()) return()
  private$progress_bar_timer$cancel()
  private$progress_bar$bar$terminate()
}

make_progress_packages <- function(done, total) {
  ## This is a workaround for an RStudio bug:
  ## https://github.com/r-lib/pkginstall/issues/42
  if (Sys.getenv("RSTUDIO", "") == "" ||
      Sys.getenv("RSTUDIO_TERM", "") != "") {
    bgblue <- crayon::bgBlue
    black <- crayon::black
  } else {
    bgblue <- crayon::reset
    black <- function(x) x
  }

  paste0(bgblue(black(paste0(" ", done, "/", total, " "))), " pkgs")
}

make_progress_bytes <- function(done, total) {
  if (is.na(total) || total == 0) {
    paste0(" | Got ", prettyunits::pretty_bytes(done))

  } else {
    paste0(
      " | ", prettyunits::pretty_bytes(done), " / ",
      prettyunits::pretty_bytes(total)
    )
  }
}

make_spinner <- function(private)  {
  progress_bar <- private$progress_bar
  spin <- progress_bar$spinner$frames[[progress_bar$spinner_state]]
  progress_bar$spinner_state <-
    progress_bar$spinner_state %% length(progress_bar$spinner$frames) + 1L
  private$progress_bar <- progress_bar
  paste0("[", spin, "]")
}

make_trailing_download_msg <- function(bar) {
  tab <- bar$what
  dd <- !is.na(tab$finished_at) & !tab$reported &
    !tab$type %in% c("installed", "deps") & tab$cache_status == "miss"
  if (any(dd)) {
    w <- which(dd & tab$finished_at == min(tab$finished_at[dd]))[1]
    bar$what$reported[w] <- TRUE
    paste0("Got ", tab$package[w])
  } else {
    if (all(tab$current == 0, na.rm = TRUE)) return("Starting...")
    w <- sample(which(is.na(tab$finished_at)), 1)
    if (w == 0) "" else paste0("Getting ", tab$package[w])
  }
}

remotes_download_resolution <- function(self, private) {
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")
  asNamespace("pkgcache")$synchronise(self$async_download_resolution())
}

remotes_async_download_resolution <- function(self, private) {
  self ; private
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  remotes_async_download_internal(self, private,
                                  private$resolution$result,
                                  "resolution")$
    then(function(value) {
      private$downloads <- value
      self$get_resolution_download()
    })
}

remotes_download_solution <- function(self, private) {
  if (is.null(private$solution)) self$solve()
  if (private$dirty) stop("Need to resolve, remote list has changed")
  asNamespace("pkgcache")$synchronise(self$async_download_solution())
}

remotes_async_download_solution <- function(self, private) {
  if (is.null(private$solution)) self$solve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  remotes_async_download_internal(self, private,
                                  private$solution$result$data,
                                  "solution")$
    then(function(value) {
      private$solution_downloads <- value
      self$get_solution_download()
    })
}

remotes_stop_for_solution_download_error <- function(self, private) {
  dl <- self$get_solution_download()
  if (any(bad <- tolower(dl$download_status) == "failed")) {
    msgs <- vcapply(
      which(bad),
      function(i) {
        urls <- format_items(dl$sources[[i]])
        glue::glue("Failed to download {dl$package[i]} \\
              from {urls}.")
      }
    )
    msg <- paste(msgs, collapse = "\n")
    err <- structure(
      list(message = msg, call = NULL, errors = dl$download_error[bad]),
      class = c("error", "condition"))
    stop(err)
  }
}

remotes_async_download_internal <- function(self, private, what, which) {
  if (any(what$status != "OK")) {
    stop("Resolution has errors, cannot start downloading")
  }
  start <- Sys.time()
  private$progress_bar <- private$create_progress_bar(what)

  dl <- lapply(seq_len(nrow(what)), function(idx) {
    force(idx)
    private$download_res(
      what[idx, ],
      on_progress = function(data) {
        private$update_progress_bar(idx, data)
        TRUE
      },
      which = which)$
      finally(function() private$update_progress_bar(idx, "done"))
  })

  asNamespace("pkgcache")$when_all(.list = dl)$
    then(function(dls) {
      what$fulltarget <- vcapply(dls, "[[", "fulltarget")
      what$download_status <- vcapply(dls, "[[", "download_status")
      what$download_error <- lapply(dls, function(x) x$download_error[[1]])
      what$file_size <- vdapply(dls, "[[", "file_size")
      class(what) <- c("remotes_downloads", class(what))
      attr(what, "metadata")$download_start <- start
      attr(what, "metadata")$download_end <- Sys.time()
      what
    })$
    finally(function() private$done_progress_bar())
}

remotes_download_res <- function(self, private, res, which, on_progress) {
  force(private)
  download_remote(
    res,
    config = private$config,
    cache = private$cache,
    which = which,
    on_progress = on_progress
  )
}

download_remote <- function(res, config, cache, which,
                            on_progress = NULL, remote_types = NULL) {
  remote_types <- c(default_remote_types(), remote_types)
  dl <- remote_types[[res$type]]$download %||% type_default_download
  target <- file.path(config$cache_dir, res$target)
  mkdirp(dirname(target))
  asNamespace("pkgcache")$async(dl)(res, target, config, cache = cache,
    which = which, on_progress = on_progress)$
    then(function(s) {
      if (length(res$sources[[1]]) && !file.exists(target)) {
        stop("Failed to download ", res$type, " package ", res$package)
      }
      if (!identical(s, "Had") && !identical(s, "Got") &&
          !identical(s, "Current")) s <- "Got"
      dlres <- res
      dlres$fulltarget <- target
      dlres$download_status <- s
      dlres$download_error <- list(NULL)
      dlres$file_size <- file.size(target)
      dlres
    })$
    catch(error = function(err) {
      dlres <- res
      dlres$fulltarget <- target
      dlres$download_status <- "Failed"
      dlres$download_error <- list(err)
      dlres$file_size <- NA_integer_
      dlres
    })
}

download_ping_if_not_source <- function(resolution, target, config, cache,
                                        on_progress) {
  resolution; target; config; cache; on_progress
  mkdirp(dirname(target))

  if (resolution$platform == "source") {
    ## If it is a source package, then the package name, version number
    ## and package type must match. If there is such a package in the cache
    ## we just take it
    cache$package$async_copy_or_add(
      target, resolution$sources[[1]], path = resolution$target,
      package = resolution$package, version = resolution$version,
      platform = resolution$platform, on_progress = on_progress)$
    then(~ attr(., "action"))

  } else {
    ## If not a source package, then we try to update it, in case there is
    ## a newly built binary
    cache$package$async_update_or_add(
      target, resolution$sources[[1]], path = resolution$target,
      package = resolution$package, version = resolution$version,
      platform = resolution$platform, on_progress = on_progress)$
    then(~ attr(., "action"))
  }
}

download_ping_if_no_sha <- function(resolution, target, config, cache,
                                    on_progress) {
  resolution; target; config; cache; on_progress
  mkdirp(dirname(target))

  if (! "sha256" %in% names(resolution) || is.na(resolution$sha256)) {
    ## If we don't know the hash of the CRAN package, then just download
    ## it. This happens if there is some discrepancy between the package
    ## data and the metadata.
    cache$package$async_copy_or_add(
      target, resolution$sources[[1]], path = resolution$target,
      package = resolution$package, version = resolution$version,
      platform = resolution$platform, on_progress = on_progress)$
    then(~ attr(., "action"))

  } else {
    ## There is a sha hash in the metadata, so we can search for that
    ## in the package cache.
    cache$package$async_copy_or_add(
      target, resolution$sources[[1]], path = resolution$target,
      package = resolution$package, version = resolution$version,
      platform = resolution$platform, sha256 = resolution$sha256,
      on_progress = on_progress)$
    then(~ attr(., "action"))
  }
}

remotes_get_resolution_download <- function(self, private) {
  if (is.null(private$downloads)) stop("No downloads")
  private$downloads
}

remotes_get_solution_download <- function(self, private) {
  if (is.null(private$solution_downloads)) stop("No downloads")
  private$solution_downloads
}

#' @noRd

print.remotes_downloads <- function(x, ...) {
  cat(format.remotes_downloads(x, ...))
}

#' @noRd

format.remotes_downloads <- function(x, ...) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  meta <- attr(x, "metadata")

  direct <- unique(x$ref[x$direct])
  dt <- prettyunits::pretty_dt(meta$download_end - meta$download_start)
  head <- glue::glue(
    "PAK DOWNLOADS, {length(direct)} refs, downloaded in {dt} ")
  width <- getOption("width") - crayon::col_nchar(head, type = "width") - 1
  head <- paste0(head, strrep(cli::symbol$line, max(width, 0)))
  push(crayon::blue(crayon::bold(head)), sep = "\n")

  push(format_dls(x, x$direct, header = NULL))
  push(format_dls(x, (! x$direct), header = "Dependencies", by_type = TRUE))
  push(format_failed_dls(x))

  paste0(result, collapse = "")
}

get_failed_dls <- function(dls) {
  dls$ref[dls$download_status == "Failed"]
}

format_dls <- function(dls, which, header, by_type = FALSE,
                       mark_failed = TRUE) {
  if (!length(dls$ref[which])) return()

  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  if (!is.null(header)) push(crayon::blue(crayon::bold(paste0(header, ":"))), sep = "\n")

  mark <- function(wh, short = FALSE) {
    ref <- ref2 <- sort(unique(dls$ref[wh]))
    if (short) ref2 <- basename(ref)
    if (mark_failed) {
      failed_dls <- get_failed_dls(dls[wh,])
      ref2 <- ifelse(ref %in% failed_dls, crayon::bold(crayon::red(ref2)), ref2)
    }
    ref2
  }

  if (by_type) {
    for (t in sort(unique(dls$type[which]))) {
      push(crayon::blue(paste0("  ", t, ":")), sep = "\n")
      which2 <- which & dls$type == t
      push(comma_wrap(
        mark(which2, short = t %in% c("deps", "installed")), indent = 4),
        sep = "\n")
    }

  } else {
    push(comma_wrap(mark(which)), sep = "\n")
  }

  paste0(result, collapse = "")
}

format_failed_dls <- function(res) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  failed <- get_failed_dls(res)
  if (length(failed) > 0) push(crayon::bold(crayon::red("Errors:")), sep = "\n")
  for (f in failed) push(format_failed_dl(res, f))

  paste0(result, collapse = "")
}

format_failed_dl <- function(dls, failed_dl) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  push("  ", failed_dl, ": ")
  wh <- which(failed_dl == dls$ref & dls$download_status == "Failed")
  errs <- unique(vcapply(dls$download_error[wh], conditionMessage))
  push(paste(errs, collapse = "\n    "), sep = "\n")

  paste0(result, collapse = "")
}

type_default_download <- function(resolution, target, config, cache,
                                  on_progress) {
  ## TODO
  stop("Not implemented yet")
}

mkdirp <- function(dir, msg = NULL) {
  s <- vlapply(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
  if (any(s) && !is.null(msg) && is_verbose()) {
    cliapp::cli_alert_info("{msg}: {path {format_items(dir[s])}}")
  }
  invisible(s)
}

file_get_time <- function(path) {
  file.info(path)$mtime
}

file_set_time <- function(path, time = Sys.time()) {
  assertthat::assert_that(
    is_character(path),
    inherits(time, "POSIXct"))
  vlapply(path, Sys.setFileTime, time = time)
}

## file.copy is buggy when to is a vector

file_copy_with_time <- function(from, to) {
  mkdirp(dirname(to))
  if (length(to) > 1) {
    mapply(file.copy, from, to,
           MoreArgs = list(overwrite =  TRUE, copy.date = TRUE),
           USE.NAMES = FALSE)
  } else {
    file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
  }
}

is_valid_package <- function(file) {
  if (!file.exists(file)) {
    FALSE
  }  else if (grepl("\\.zip$", file)) {
    is_valid_package_zip(file)
  } else if (grepl("\\.tgz$|\\.tar\\.gz$", file)) {
    is_valid_package_targz(file)
  } else {
    ## Just ignore other files
    FALSE
  }
}

is_valid_package_zip <- function(file) {
  if (file.info(file)$size == 0) return(FALSE)
  tryCatch(
    is_package_file_list(file, suppressWarnings(zip_list(file))),
    error = function(e) FALSE
  )
}

is_valid_package_targz <- function(file) {
  if (file.info(file)$size == 0) return(FALSE)
  con <- gzfile(file, open = "rb")
  on.exit(close(con), add = TRUE)
  tryCatch(
    is_package_file_list(file, utils::untar(con, list = TRUE)),
    error = function(e) FALSE
  )
}

is_package_file_list <- function(file, list) {
  pkgname <- pkg_name_from_file(file)

  ## A single directory, named after the package
  if (any(! grepl(paste0("^", pkgname, "\\b"), list))) return(FALSE)

  ## DESCRIPTION file
  if (! paste0(pkgname, "/DESCRIPTION") %in% list) return(FALSE)

  return(TRUE)
}

pkg_name_from_file <- function(x) {
  sub("^([a-zA-Z0-9\\.]+)_.*$", "\\1", basename(x))
}

package_name_rx <- function() "[[:alpha:]][[:alnum:].]*[[:alnum:]]"

## CRAN and GitHub are special, because they have shorthands,
## so we need to know their regexes to find the type of the remotes 

standard_rx <- function(remote_name = "standard") {
  paste0(
    "^",
    ## Optional remote type
    "(?:", remote_name, "::)?",
    ## Package name, only valid names
    "(?<package>", package_name_rx(), ")",
    ## Package version, only valid version numbers
    "(?:@(?:(?:(?<atleast>>=)?",
    "(?<version>[0-9]+[-\\.][0-9]+(?:[-\\.][0-9]+)*|current|last))))?",
    "$"
  )
}

#' Match a GH username
#'
#' * may only contain alphanumeric characters or hyphens
#' * cannot have multiple consecutive hyphens
#' * cannot begin or end with a hyphen
#' * maximum 39 characters
#'
#' Based on https://github.com/shinnn/github-username-regex
#'
#' @keywords internal
#' @noRd

github_username_rx <- function() {
  "(?<username>(?:[a-zA-Z\\d](?:[a-zA-Z\\d]|-(?=[a-zA-Z\\d])){0,38}))"
}

github_repo_rx <- function() "(?<repo>[^/@#]+)"
github_subdir_rx <- function() "(?:/(?<subdir>(?:[^@#]*[^@#/])/?))"
github_commitish_rx <- function() "(?:@(?<commitish>[^*].*))"
github_pull_rx <- function() "(?:#(?<pull>[0-9]+))"
github_release_rx <- function() "(?:@(?<release>[*]release))"
github_detail_rx <- function() {
  sprintf(
    "(?:(?:%s)|(?:%s)|(?:%s))?",
    github_commitish_rx(),
    github_pull_rx(),
    github_release_rx()
  )
}

github_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    ## Optional remote type
    "(?:github::)?",
    github_username_rx(), "/",
    github_repo_rx(),
    github_subdir_rx(), "?",
    ## Commit / PR / Release
    github_detail_rx(),
    "$"
  )
}

github_url_commitish_rx <- function() {
  "(?:(?:tree|commit|releases/tag)/(?<commitish>.+$))"
}

github_url_pull_rx <- function() "(?:pull/(?<pull>.+$))"

github_url_release_rx <- function() "(?:releases/)(?<release>.+$)"

github_url_detail_rx <- function() {
  glue::glue("(?:/(?:",
       "{github_url_commitish_rx()}",
       "|{github_url_pull_rx()}",
       "|{github_url_release_rx()}",
       "))?")
}

## We need to select the shortest match here, to avoid matching a
## a .git suffix

github_url_repo_rx <- function() "(?<repo>[^/@#]+?)"

github_url_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    ## Optional remote type
    "(?:github::)?",
    ## Optional protocol
    "(?:(?:https?://)|(?:ssh://(?:[^@]+@)?)?)",
    ## Servername
    "(?:[^/:]+)[/:]",
    ## Username
    github_username_rx(), "/",
    ## Repo
    github_url_repo_rx(),
    ## subdir, always empty
    "(?<subdir>)",
    ## Optional Extension
    "(?:[.]git)?",
    ## Commit / PR / Release
    github_url_detail_rx(),
    "$"
  )
}

remote_type_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    ## Remote type
    "(?:(?<type>[-_[:alnum:]]+)::)?",
    ## Rest of ref
    "(?<rest>.*)$"
  )
}

type_default_parse <- function(specs, ...) {
  m <- rematch2::re_match(specs, remote_type_rx())
  lapply_rows(m, function(x)
    list(package = x$package, type = x$type, rest = x$rest, ref = x$.text)
  )
}

get_remote_types <- function(specs) {
  m <- rematch2::re_match(specs, remote_type_rx())
  types <- m$type

  types[types == "" & grepl(standard_rx(), specs, perl = TRUE)] <- "standard"
  types[types == "" & grepl(github_rx(), specs, perl = TRUE)] <- "github"
  types[types == "" & grepl(github_url_rx(), specs, perl = TRUE)] <- "github"

  if (any(bad <- types == "")) {
    stop("Can't parse remotes: ", paste(specs[bad], collapse = ", "))
  }

  types
}

#' Parse package location specifications
#'
#' @param specs character vector
#' @param remote_types custom remote types can be added here
#' @param ... additional arguments are passed to the individual parser
#'   functions
#' @return List of parsed specification.
#'
#' @noRd

parse_remotes <- function(specs, remote_types = NULL, ...) {
  remote_types <- c(default_remote_types(), remote_types)
  types <- get_remote_types(specs)
  unique_types <- unique(types)
  res <- vector("list", length(specs))

  if (any(bad <- setdiff(unique_types, names(remote_types)))) {
    stop("Unknown remote type(s): ", format_items(bad))
  }

  for (this in unique_types) {
    parser <- remote_types[[this]]$parse %||% type_default_parse
    this_specs <- specs[types == this]
    new_remotes <- parser(this_specs, ...)
    new_remotes <- lapply(new_remotes, function(x) { x$type <- this; x })
    new_remotes <- lapply(
      new_remotes,
      add_class,
      c(paste0("remote_ref_", this), "remote_ref")
    )
    res[types == this] <- new_remotes
  }
  res
}

#' Class for package dependency resolution and package downloads
#'
#' @section Usage:
#' ```
#' r <- remotes()$new(specs, config = list())
#'
#' r$resolve()
#' r$async_resolve()
#' r$get_resolution()
#' r$draw_tree(pkgs)
#'
#' r$download()
#' r$async_download()
#' r$get_download_status()
#'
#' @section Arguments:
#' * `specs`: Package specifications. See 'Remote Types' below.
#' * `config`: Custom configuration, a named list. See the list of options
#'   below.
#' * `pkgs`: Charcater vector of regular expressions, to specify the
#'   packages to query.
#'
#' @section Details:
#'
#' `$new()` creates a new resolution/download task. The packages must be
#' supplied.
#'
#' `$resolve()` resolves all remote packages, and their dependencies.
#' The result of the resolution is stored internally in the `remotes`
#' object, and also returned. It can be queried later with
#' `$get_resolution()`. See `Resolution Table` below.
#'
#' `$async_resolve()` returns a deferred value for the resolution of
#' all remote packages. The deferred value resolves to the same value as
#' returned by `$resolve()`.
#'
#' `$get_resolution()` returns the result of the resolution. See
#' `Resolution Table` below.
#'
#' `$draw_tree()` draws the dependency tree of one or more packages,
#' after resolution.
#'
#' @section Remote types:
#' The following remote types are currently supported:
#'
#' * CRAN packages:
#'     \preformatted{
#'     [cran::]<package>[@[>=]<version> | @current | @last]
#'     }
#' * Packages from GitHub repositories
#'     \preformatted{
#'     [<package>=][github::]<username>/<repo>[/<subdir>]
#'     [@committish | #<pull> | @[*]release]
#'     }
#'
#' @section Resolution table:
#'
#' The data frame returned by `$resolve()` et al. has the following
#' columns:
#' * `ref` The original remote reference.
#' * `direct` Whether the reference was specified (`TRUE`), or it is a
#'   dependency (`FALSE`).
#' * `status` Status of the resolution: `"OK"` or `"FAILED"`.
#' * `package` Name of the package.
#' * `version` Package version.
#' * `platform` Platform: `"source"`, `"macos"` or `"windows"` currently.
#' * `rversion` If it is a binary package, which R version it was built for.
#'   For source packages `"*"` is used.
#' * `repodir` The relative directory of the package file in the repository.
#' * `sources` URLs to download the package from. CRAN URLs are not stable,
#'   so each row typically contains multiple URLs here.
#' * `target` Relative path for the package file in the repository.
#' * `fulltarget` Fulle path for the package file in the cache.
#'
#' @section Configuration options:
#' * `cache_dir` Path to the cache. By default a temporary directory is
#'   used. (TODO)
#' * `platforms` The platforms to use. Defaults to the current platform
#'   on Windows and macOS and source.
#' * `cran-mirror` The CRAN mirror to use. Defaults to `getOption("repos")`
#'   or the cloud mirror if not set.
#' * `dependencies` Dependencies to install. Defaults to the *hard*
#'   dependencies: `Depends`, `Imports`, `LinkingTo`.
#' * `r-versions`: R version to support, for the binary packages. Defaults
#'   to the running R version.
#'
#' @name remotes
#' @examples
#' ## This does download a bunch of packages, so we don't run it currently
#' \dontrun{
#' rems <- remotes()$new(c("dplyr", "r-lib/rcmdcheck"))
#' rems$resolve()
#' rems$download_resolution()
#' rems$get_download_status()
#' }
#' @noRd
NULL

remotes <- function() {
  R6::R6Class(
    "remotes",
    public = list(
      initialize = function(specs, config = list(), library = NULL,
                            remote_types = NULL)
        remotes_init(self, private, specs, config, library, remote_types),

      async_resolve = function()
        remotes_async_resolve(self, private),
      resolve = function()
        remotes_resolve(self, private),
      get_resolution = function()
        remotes_get_resolution(self, private),

      async_download_resolution = function()
        remotes_async_download_resolution(self, private),
      download_resolution = function()
        remotes_download_resolution(self, private),
      get_resolution_download = function()
        remotes_get_resolution_download(self, private),

      solve = function(policy = c("lazy", "upgrade"))
        remotes_solve(self, private, match.arg(policy)),
      stop_for_solve_error = function()
        remotes_stop_for_solve_error(self, private),
      get_solution = function()
        remotes_get_solution(self, private),
      get_install_plan = function()
        remotes_install_plan(self, private),
      draw_tree = function(pkgs = NULL)
        remotes_draw_tree(self, private, pkgs),

      async_download_solution = function()
        remotes_async_download_solution(self, private),
      download_solution = function()
        remotes_download_solution(self, private),
      get_solution_download = function()
        remotes_get_solution_download(self, private),
      stop_for_solution_download_error = function()
        remotes_stop_for_solution_download_error(self, private),

      print = function(...)
        remotes_print(self, private, ...)
    ),

    private = list(
      library = NULL,
      dirty = FALSE,
      remotes = list(),
      cache = NULL,
      resolution = NULL,
      solution = NULL,
      downloads = NULL,
      solution_downloads = NULL,
      download_cache = NULL,
      config = NULL,
      progress_bar = NULL,
      progress_bar_timer = NULL,
      remote_types = NULL,

      download_res = function(res, which, on_progress = NULL)
        remotes_download_res(self, private, res, which, on_progress),
      subset_resolution = function(which)
        remotes__subset_resolution(self, private, which),
      create_lp_problem = function(pkgs, policy)
        remotes__create_lp_problem(self, private, pkgs, policy),
      solve_lp_problem = function(problem)
        remotes__solve_lp_problem(self, private, problem),

      create_progress_bar = function(what)
        remotes__create_progress_bar(self, private, what),
      update_progress_bar = function(idx, data)
        remotes__update_progress_bar(self, private, idx, data),
      show_progress_bar = function()
        remotes__show_progress_bar(self, private),
      done_progress_bar = function()
        remotes__done_progress_bar(self, private)
    )
  )
}

remotes_init <- function(self, private, specs, config, library,
                         remote_types) {

  assertthat::assert_that(is_character(specs),
              is_valid_config(config),
              is_path_or_null(library))

  private$remotes <- parse_remotes(specs)
  private$config <- utils::modifyList(remotes_default_config(), config)
  private$remote_types <- remote_types %||% default_remote_types()

  if (!is.null(library)) {
    mkdirp(library, msg = "Creating library directory")
    library <- normalizePath(library)
  }
  private$library <- library
  mkdirp(private$download_cache <- private$config$cache_dir)

  private$cache <- list(
    metadata = pkgcache::cranlike_metadata_cache$new(
      replica_path = private$config$metadata_cache_dir,
      platforms = private$config$platforms,
      r_version = private$config$`r-version`,
      cran_mirror = private$config$`cran-mirror`),
    package = pkgcache::package_cache$new(private$config$package_cache_dir),
    installed = if (!is.null(library)) make_installed_cache(library)
  )

  private$dirty <- TRUE
  invisible(self)
}

remotes_default_config <- function() {
  list(
    "cache_dir"          = detect_download_cache_dir(),
    "package_cache_dir"  = NULL,
    "metadata_cache_dir" = tempfile(),
    "platforms"          = default_platforms(),
    "cran-mirror"        = default_cran_mirror(),
    "dependencies"       = dep_types_hard(),
    "r-versions"         = current_r_version()
  )
}

is_valid_config <- function(x) {
  assertthat::assert_that(is.list(x), all_named(x))
  assertthat::assert_that(all(names(x) %in% names(remotes_default_config())))
  for (n in names(x)) {
    switch (
      n,
      cache_dir          = assertthat::assert_that(is_path(x[[n]])),
      package_cache_dir  = assertthat::assert_that(is_path(x[[n]])),
      metadata_cache_dir = assertthat::assert_that(is_path(x[[n]])),
      platforms          = assertthat::assert_that(is_platform_list(x[[n]])),
      "cran-mirror"      = assertthat::assert_that(is_string(x[[n]])),
      dependencies       = assertthat::assert_that(is_dependencies(x[[n]])),
      "r-versions"       = assertthat::assert_that(is_r_version_list(x[[n]]))
    )
  }
  TRUE
}

is_valid_config <- assertthat::`on_failure<-`(
  is_valid_config, function(call, env) {
    paste0(deparse(call$x), " is not a valid configuration list")
  })

remotes_get_total_files <- function(self, private) {
  nrow(private$resolution$result)
}

remotes_print <- function(self, private, ...) {
  cat("<remotes>\n")

  ## refs
  refs <- vcapply(private$remotes, "[[", "ref")
  cat(
    strwrap(
      paste0("- refs: ", paste(glue::backtick(refs), collapse = ", ")),
      indent = 0, exdent = 4
    ),
    sep = "\n"
  )

  ## library
  if (!is.null(private$library)) {
    cat("- library:", glue::backtick(private$library), "\n")
  }

  ## resolution
  if (!is.null(private$resolution$result)) {
    if (all(private$resolution$result$status == "OK")) {
      cat("- has resolution\n")
    } else {
      cat("- has resolution, with errors\n")
    }
  }

  ## solution
  if (!is.null(private$solution$result)) {
    if (private$solution$result$status == "OK") {
      cat("- has solution\n")
    } else {
      cat("- has solution, with errors\n")
    }
  }

  invisible(self)
}

res_make_empty_df <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- tibble::tibble(
        ref      = character(),
        type     = character(),
        direct   = logical(),
        status   = character(),         # "OK" or "FAILED"
        package  = character(),
        version  = character(),
        license  = character(),
        needscompilation
                 = logical(),
        priority = character(),
        md5sum   = character(),
        sha256   = character(),
        filesize = integer(),
        built    = character(),
        platform = character(),         # "source" or platform string
        rversion = character(),         # * or version number (prefix)
        repodir  = character(),
        target   = character(),
        deps     = list(),
        mirror   = character(),         # for CRAN/BioC
        sources  = list(),              # list of URLs
        remote   = list(),              # parsed remote ref
        error    = list(),              # list of errors
        metadata = list(),              # named character of entries
        extra    = list(),              # any extra data (e.g. GitHub sha)
        dep_types= list()
      )
    }
    data
  }
})

res_df_defaults <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- list(
        direct   = FALSE,
        status   = "OK",
        license  = NA_character_,
        needscompilation
                 = TRUE,
        priority = NA_character_,
        md5sum   = NA_character_,
        sha256   = NA_character_,
        filesize = NA_integer_,
        built    = NA_character_,
        platform = "source",
        rversion = "*",
        repodir  =  "src/contrib",
        target   =
          quote(file.path("src/contrib", paste0(package, "_", version, ".tar.gz"))),
        deps     = list(make_null_deps()),
        remote   = quote(parse_remotes(ref)),
        error    = list(list()),
        metadata = list(list()),
        mirror   = NA_character_,
        extra    = list(list()),
        dep_types= list(list())
      )
    }
    data
  }
})

res_df_entry_types <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- vcapply(res_make_empty_df(), class)
    }
    data
  }
})

res_df_must_have <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- setdiff(names(res_make_empty_df()), names(res_df_defaults()))
    }
    data
  }
})

res_check_entries <- function(ent) {
  ## Some columns are required
  assertthat::assert_that(is.data.frame(ent))

  if (length(miss <- setdiff(res_df_must_have(), names(ent)))) {
    stop("Entries missing from remote: ", format_items(miss))
  }

  ent_types <- vcapply(ent, class)
  exp_types <- res_df_entry_types()[names(ent)]
  if (any(bad <- ent_types != exp_types)) {
    items <- paste0(names(ent)[bad], " (", ent_types[bad], ", expected ",
                    exp_types[bad], ")")
    stop("Wrong entry types: ", format_items(items))
  }

  invisible(ent)
}

#' @noRd
#' @param df Resolution data frame (tibble, really).
#' @param entries List of entries to add.

res_add_df_entries <- function(df, entries) {
  if (!tibble::is_tibble(entries)) entries <- res_one_row_tibble(entries)
  entries <- res_add_defaults(entries)
  tibble::as_tibble(rbind(df, entries))[names(df)]
}

res_one_row_tibble <- function(l) {
  assertthat::assert_that(is.list(l) && all_named(l))
  samp <- res_make_empty_df()[names(l)]
  bad <- vlapply(samp, is.list) & !vlapply(l, is.list)
  l[bad] <- lapply(l[bad], list)
  tibble::as_tibble(l)
}

res_add_defaults <- function(df) {
  if (length(bad <- setdiff(res_df_must_have(), names(df)))) {
    stop("Entries missing from remote: ", format_items(bad))
  }

  all_types <- res_df_entry_types()
  miss <- setdiff(names(all_types), names(df))
  def <- lapply(res_df_defaults()[miss], eval, envir = df,
                enclos = environment())
  df[names(def)] <- def
  df <- df[, names(all_types)]

  ent_types <- vcapply(df, typeof)
  exp_types <- all_types[names(df)]
  if (any(bad <- ent_types != exp_types)) {
    items <- paste0(names(df)[bad], " (", ent_types[bad], ", expected ",
                    exp_types[bad], ")")
    stop("Wrong entry types: ", format_items(items))
  }

  df
}

#' @noRd

print.remotes_resolution <- function(x, ...) {
  cat(format(x, ...))
}

#' @noRd

format.remotes_resolution <- function(x, ...) {

  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  meta <- attr(x, "metadata")

  direct <- unique(x$ref[x$direct])
  dt <- prettyunits::pretty_dt(meta$resolution_end - meta$resolution_start)
  head <- glue::glue(
    "PAK RESOLUTION, {length(direct)} refs, resolved in {dt} ")
  width <- getOption("width") - crayon::col_nchar(head, type = "width") - 1
  head <- paste0(head, strrep(cli::symbol$line, max(width, 0)))
  push(crayon::blue(crayon::bold(head)), sep = "\n")

  push(format_refs(x, x$direct, header = NULL))
  push(format_refs(x, (! x$direct), header = "Dependencies", by_type = TRUE))
  push(format_failed_refs(x))

  paste0(result, collapse = "")
}

get_failed_refs <- function(res) {
  failed <- tapply(res$status, res$ref, function(x) all(x != "OK"))
  names(which(failed))
}

format_refs <- function(res, which, header, by_type = FALSE,
                        mark_failed = TRUE) {
  if (!length(res$ref[which])) return()

  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  if (!is.null(header)) push(crayon::blue(crayon::bold(paste0(header, ":"))), sep = "\n")

  mark <- function(wh, short = FALSE) {
    ref <- ref2 <- sort(unique(res$ref[wh]))
    if (short) ref2 <- basename(ref)
    if (mark_failed) {
      failed_ref <- get_failed_refs(res[wh,])
      ref2 <- ifelse(ref %in% failed_ref, crayon::bold(crayon::red(ref2)), ref2)
    }
    ref2
  }

  if (by_type) {
    for (t in sort(unique(res$type[which]))) {
      push(crayon::blue(paste0("  ", t, ":")), sep = "\n")
      which2 <- which & res$type == t
      push(comma_wrap(mark(which2, short = t == "installed"), indent = 4),
           sep = "\n")
    }

  } else {
    push(comma_wrap(mark(which)), sep = "\n")
  }

  paste0(result, collapse = "")
}

format_failed_refs <- function(res) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  failed <- get_failed_refs(res)
  if (length(failed) > 0) push(crayon::bold(crayon::red("Errors:")), sep = "\n")
  for (f in failed) push(format_failed_ref(res, f))

  paste0(result, collapse = "")
}

format_failed_ref <- function(res, failed_ref) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  push("  ", failed_ref, ": ")
  wh <- which(failed_ref == res$ref)
  errs <- unique(vcapply(res$error[wh], conditionMessage))
  push(paste(errs, collapse = "\n    "), sep = "\n")

  paste0(result, collapse = "")
}

res__create_progress_bar <- function(self, private) {
  if (!is_verbose()) return(NULL)
  bar <- list()
  bar$spinner <- cli::get_spinner()
  bar$spinner_state <- 1L
  bar$chars <- progress_chars()

  bar$bar <- cliapp::cli_progress_bar(
    format = ":xbar:xstate :xspinner :xmsg",
    total = 10e7,
    force = TRUE
    )

  bar
}

res__update_progress_bar <- function(self, private) {
  if (!is_verbose()) return()

  deps <- nrow(private$state)
  direct <- sum(private$state$direct)
  direct_done <- sum(!is.na(private$state$status) & private$state$direct)

  bar <- if (direct >= 5) {
    make_bar_pkgdepends(private$bar$chars, direct_done / direct, width = 15)
  }

  tokens <- list(
    xbar = bar %||% "",
    xstate = make_progress_main(deps, direct_done, direct),
    xspinner = make_progress_spinner(self, private),
    xmsg = pkgdepends_make_trailing_progress_msg(self, private)
  )

  private$bar$bar$tick(0, tokens = tokens)
}

make_bar_pkgdepends <- function(chars, p, width =  15) {
  width <- width - 2L

  w <- if (isTRUE(all.equal(p, 1))) width else trunc(width * p)

  pchars <- rep(chars$fill, w)
  xchars <- rep(" ", max(width - w, 0))
  bar <- paste(
    c(chars$lpar, pchars, xchars, chars$rpar, " "),
    collapse = "")

  ## This is a workaround for an RStudio bug:
  ## https://github.com/r-lib/pkginstall/issues/42
  if (Sys.getenv("RSTUDIO", "") == "" ||
      Sys.getenv("RSTUDIO_TERM", "") != "") {
    crayon::green(bar)
  } else {
    crayon::reset(bar)
  }
}

make_progress_main <- function(deps, done, total) {
  paste0(
    "Found ",
    crayon::bgBlue(crayon::black(paste0(" ", deps, " "))),
    " deps for ",
    crayon::bgBlue(crayon::black(paste0(" ", done, "/", total, " "))),
    " pkgs"
  )
}

make_progress_spinner  <- function(self, private) {
  bar <- private$bar
  spin <- bar$spinner$frames[[bar$spinner_state]]
  bar$spinner_state <-
    bar$spinner_state %% length(bar$spinner$frames) + 1L
  private$bar <- bar
  paste0("[", spin, "]")
}

pkgdepends_make_trailing_progress_msg <- function(self, private) {
  ongoing <- private$state[is.na(private$state$status), ]
  if (nrow(ongoing) == 0) return("Done")

  types <- vcapply(ongoing$remote, "[[", "type")
  remote <- if (all(types %in% c("cran", "bioc", "standard"))) {
    ongoing$remote[[ order(ongoing$started_at)[1] ]]
  } else {
    nonstd <- ongoing[! types %in% c("cran", "bioc", "special"), ]
    nonstd$remote[[ order(nonstd$started_at)[1] ]]
  }

  if (remote$type %in% c("cran", "bioc", "standard")) {
    "Resolving standard (CRAN/BioC) packages"
  } else if (remote$type %in% "installed") {
    "Checking installed packages"
  } else {
    paste0("Resolving ", remote$ref)
  }
}

res__done_progress_bar <- function(self, private) {
  if (!is_verbose()) return()
  private$bar$bar$terminate()
}

remotes_resolve <- function(self, private) {
  "!DEBUG remotes_resolve (sync)"
  asNamespace("pkgcache")$synchronise(self$async_resolve())
}

remotes_async_resolve <- function(self, private) {
  "!DEBUG remotes_resolve (async)"
  ## We remove this, to avoid a discrepancy between them
  private$downloads <- NULL
  private$solution <- NULL

  private$dirty <- TRUE
  private$cache$metadata$check_update()
  private$resolution <- resolution()$new(
    config = private$config, cache = private$cache,
    library = private$library, remote_types = private$remote_types)

  private$resolution$push(direct = TRUE, .list = private$remotes)

  private$resolution$when_complete()$
    then(function(x) {
      private$dirty <- FALSE
      x
    })
}

remotes_get_resolution <- function(self, private) {
  if (is.null(private$resolution$result)) stop("No resolution yet")
  private$resolution$result
}

remotes__subset_resolution <- function(self, private, which) {
  if (is.null(private$resolution$result)) stop("No resolution yet")
  res <- private$resolution$result[which, ]
  attr(res, "metadata")  <- attr(private$resolution$result, "metadata")
  res
}

resolution <- function() {
  R6::R6Class(
    "resolution",
    public = list(
      result = NULL,
      initialize = function(config, cache, library = NULL,
                            remote_types = NULL)
        res_init(self, private, config, cache, library, remote_types),
      push = function(..., direct = FALSE, .list = list())
        res_push(self, private, ..., direct = direct, .list = .list),
      when_complete = function() private$deferred
    ),
    
    private = list(
      remote_types = NULL,
      config = NULL,
      cache = NULL,
      library = NULL,
      deferred = NULL,
      state = NULL,
      dependencies = NULL,
      metadata = NULL,
      bar = NULL,
      
      delayed = list(),
      delayed_refs = character(),
      resolve_delayed = function(resolve)
        res__resolve_delayed(self, private, resolve),
      
      create_progress_bar = function()
        res__create_progress_bar(self, private),
      update_progress_bar = function()
        res__update_progress_bar(self, private),
      done_progress_bar = function()
        res__done_progress_bar(self, private),
      
      set_result = function(row_idx, value)
        res__set_result(self, private, row_idx, value),
      try_finish = function(resolve)
        res__try_finish(self, private, resolve)
    )
  )
}

res_init <- function(self, private, config, cache, library,
                     remote_types) {

  "!DEBUG resolution init"
  private$config <- config
  private$cache <- cache
  private$library <- library
  private$remote_types <- remote_types %||% default_remote_types()
  private$metadata <- list(resolution_start = Sys.time())
  private$dependencies <- interpret_dependencies(config$dependencies)
  private$bar <- private$create_progress_bar()

  self$result <- res_make_empty_df()

  private$state <- tibble::tibble(
    ref = character(),
    remote = list(),
    status = character(),
    direct = logical(),
    async_id = integer(),
    started_at = Sys.time()[FALSE])

  private$deferred <- asNamespace("pkgcache")$deferred$new(
    type = "resolution_queue",
    parent_resolve = function(value, resolve, id) {
      "!DEBUG resolution done"
      wh <- which(id == private$state$async_id)
      private$state$status[wh] <- "OK"

      npkgs <- value$package[value$type != "installed"]
      ## Installed already? Resolve that as well
      if (!is.null(private$library) && length(npkgs)) {
        npkgs <- npkgs[file.exists(file.path(private$library, npkgs))]
        if (length(npkgs))  {
          lib <- normalizePath(private$library, winslash = "/",
                               mustWork = FALSE)
          refs <- paste0("installed::", lib, "/", npkgs)
          refs <- setdiff(refs, private$state$ref)
          self$push(.list = parse_remotes(refs))
        }
      }

      private$set_result(wh, value)
      private$try_finish(resolve)
      private$update_progress_bar()
    },

    parent_reject = function(value, resolve, id) {
      "!DEBUG resolution failed"
      wh <- which(id == private$state$async_id)
      private$state$status[wh] <- "FAILED"
      rec <- private$state[wh,]
      fail_val <- list(
        ref = rec$ref,
        type = rec$remote[[1]]$type,
        package = rec$remote[[1]]$package %|z|% NA_character_,
        version = NA_character_,
        sources = NA_character_,
        direct = rec$direct,
        status = "FAILED",
        remote = rec$remote,
        error = list(value)
      )
      private$set_result(wh, fail_val)
      private$try_finish(resolve)
      private$update_progress_bar()
    })
}

res_push <- function(self, private, ..., direct, .list = .list) {
  new <- c(list(...), .list)

  ## Drop the ones already resolving up front
  new_refs <- vcapply(new, "[[", "ref")
  keep <- ! new_refs %in% c(private$state$ref, private$delayed_refs)
  new <- new[keep]
  new_refs <- new_refs[keep]

  ## Drop duplicates as well
  uni_refs <- !duplicated(new_refs)
  if (! all(uni_refs)) {
    new <- new[uni_refs]
    new_refs <- new_refs[uni_refs]
  }

  ## We do CRAN/BioC/standard in batches
  ## TODO: direct ones in one go as well
  batch_types <- setdiff(c("cran", "standard", "bioc", "installed"),
                         private$remote_types)
  delay <- vcapply(new, "[[", "type") %in% batch_types
  if (!direct && any(delay)) {
    refs <- vcapply(new[delay], "[[", "ref")
    private$delayed <- c(private$delayed, new[delay])
    private$delayed_refs <- c(private$delayed_refs, refs)
    new <- new[!delay]
    "!DEBUG pushing `sum(delay)` batch resolutions"
  }

  for (n in new) {
    "!DEBUG resolution push `n$ref`"
    dx <- resolve_remote(n, direct, private$config, private$cache,
                         private$dependencies,
                         remote_types = private$remote_types)

    private$state <- rbind(
      private$state,
      tibble::tibble(ref = n$ref, remote = list(n), status = NA_character_,
             direct = direct, async_id = dx$get_id(),
             started_at = Sys.time())
    )

    private$update_progress_bar()
    dx$then(private$deferred)
  }
}

res__resolve_delayed <- function(self, private, resolve) {
  n <- private$delayed
  private$delayed <- list()
  private$delayed_refs <- character()

  refs <- vcapply(n, "[[", "ref")
  done <- refs %in% private$state$ref
  n <- n[!done]
  refs <- vcapply(n, "[[", "ref")
  "!DEBUG resolving `length(private$delayed)` delayed remotes"

  if (length(n))  {
    types <- vcapply(n, "[[", "type")
    utypes <- unique(types)
    for (t in utypes) {
      n2 <- n[types == t]

      dx <- resolve_remote(n2, direct = FALSE, private$config,
                           private$cache, private$dependencies,
                           remote_types = private$remote_types)

      private$state <- rbind(
        private$state,
        tibble::tibble(ref = vcapply(n2, "[[", "ref"), remote = n2,
               status = NA_character_, direct = FALSE,
               async_id = dx$get_id(), started_at = Sys.time())
      )
      dx$then(private$deferred)
    }
    private$update_progress_bar()
  }

  private$try_finish(resolve)
}

res__set_result <- function(self, private, row_idx, value) {
  unknown <- if ("unknown_deps" %in% names(value)) value$unknown_deps
  if (is.null(unknown) && !is.null(attr(value, "unknown_deps"))) {
    unknown <- attr(value, "unknown_deps")
    attr(value, "unknown_deps") <- NULL
  }
  value <- value[setdiff(names(value), "unknown_deps")]
  done <- value$ref %in% self$result$ref
  value <- if (is.data.frame(value)) value[!done, ] else value[!done]
  if (any(!done)) self$result <- res_add_df_entries(self$result, value)
  "!DEBUG resolution setting result, total: `nrow(self$result)`"
  if (length(unknown)) self$push(.list = parse_remotes(unknown))
}

res__try_finish <- function(self, private, resolve) {
  "!DEBUG resolution trying to finish with `nrow(self$result)` results"
  if (length(private$delayed)) return(private$resolve_delayed(resolve))
  if (all(! is.na(private$state$status))) {
    "!DEBUG resolution finished"
    private$metadata$resolution_end <- Sys.time()
    attr(self$result, "metadata") <- private$metadata
    class(self$result) <- c("remotes_resolution", class(self$result))
    private$done_progress_bar()
    resolve(self$result)
  }
}

resolve_remote <- function(remote, direct, config, cache, dependencies,
                           remote_types = NULL) {

  remote; direct; config; cache; dependencies; remote_types

  remote_types <- c(default_remote_types(), remote_types)

  type <- remote$type %||% unique(vcapply(remote, "[[", "type"))
  if (length(type) != 1) stop("Invalid remote or remote list, multiple types?")

  resolve <- remote_types[[type]]$resolve
  if (is.null(resolve)) {
    stop("Cannot resolve type", format_items(type))
  }

  asNamespace("pkgcache")$async(resolve)(
    remote, direct = direct, config = config, cache = cache,
    dependencies = dependencies
  )$
    then(function(value) {
      value[["dep_types"]] <-
        if (NROW(value)) list(dependencies[[2-direct]]) else list()
      value
  })
}

resolve_from_description <- function(path, sources, remote, direct,
                                     config, cache, dependencies) {

  dsc <- desc::desc(file = path)
  deps <- resolve_ref_deps(dsc$get_deps(), dsc$get("Remotes")[[1]])

  rversion <- tryCatch(
    get_minor_r_version(dsc$get_built()$R),
    error = function(e) "*"
  )

  platform <- tryCatch(
    dsc$get_built()$Platform %|z|% "source",
    error = function(e) "source"
  )

  nc <- dsc$get_field("NeedsCompilation", NA)
  if  (!is.na(nc)) nc <- tolower(nc) %in% c("true", "yes")

  unknown <- deps$ref[deps$type %in% dependencies]

  meta <- c(
    RemoteRef = remote$ref,
    RemoteType = remote$type,
    RemoteSha = NULL,                   # TODO
    RemoteUrl = NULL,                   # TODO
    RemoteUsername = NULL,              # TODO
    RemoteRepo = NULL,                  # TODO
    RemoteBranch = NULL                 # TODO
  )

  list(
    ref = remote$ref,
    type = remote$type,
    direct = direct,
    status = "OK",
    package = dsc$get_field("Package"),
    version = dsc$get_field("Version"),
    license = dsc$get_field("License", NA_character_),
    needscompilation = nc,
    md5sum = dsc$get_field("MD5sum", NA_character_),
    built = dsc$get_field("Built", NA_character_),
    platform = platform,
      rversion = rversion,
    deps = list(deps),
    sources = sources,
    remote = list(remote),
    unknown_deps = setdiff(unknown, "R"),
    extra = list(list(description = dsc)),
    metadata = meta
  )
}

resolve_from_metadata <- function(remotes, direct, config, cache,
                                  dependencies) {

  remotes; direct; config; cache; dependencies

  ## Single remote, or a list of remotes
  if ("ref" %in% names(remotes)) {
    packages <- remotes$package
    refs <- remotes$ref
    types <- remotes$type
  } else  {
    packages <- vcapply(remotes, "[[", "package")
    refs <- vcapply(remotes,  "[[", "ref")
    types <-  vcapply(remotes, "[[", "type")
  }

  if (!direct) dependencies <- dependencies$indirect
  "!DEBUG resolving `length(refs)` batch resolution"
  cache$metadata$async_deps(packages, dependencies = dependencies)$
    then(function(data) {
      cols <-  c(
        "ref", "type", "status", "package", "version", "license",
        "needscompilation", "priority", "md5sum", "platform",
        "rversion", "repodir", "target", "deps", "sources", "mirror",
        "filesize", "sha256")

      res <- data[cols]
      res$built <- data[["built"]] %||% rep(NA_character_, nrow(res))
      idx <- match(res$package, packages)
      res$ref[!is.na(idx)] <- stats::na.omit(refs[idx])
      res$type[] <- "standard"
      res$type[!is.na(idx)] <- stats::na.omit(types[idx])
      res$needscompilation <-
        tolower(res$needscompilation) %in% c("yes", "true")
      res$direct <- direct & res$ref %in% refs

      res$metadata <- get_standard_metadata(res)

      if (length(bad <- attr(data, "unknown"))) {
        idx <- match(bad, packages)
        bad[!is.na(idx)] <- stats::na.omit(refs[idx])
        failed <- make_failed_resolution(
          bad, ifelse(!is.na(idx), types[idx], "standard"),
          direct & bad %in% refs)
        res <- rbind_expand(res, res_add_defaults(failed))
      }

      res
    })
}

get_standard_metadata <- function(tab) {
  meta <- replicate(nrow(tab), character(), simplify = FALSE)
  for (i in seq_len(nrow(tab))) {
    meta[[i]] <-
      c(RemoteType = tab$type[i],
        RemoteRef = tab$ref[i],
        RemoteRepos = tab$mirror[i],
        RemotePkgType = tab$platform[i],
        RemoteSha = tab$version[i])
  }
  meta
}

make_failed_resolution <- function(refs, type, direct) {
  err <- structure(
    list(message = "Cannot find standard package"),
    class = c("error", "condition"))
  tibble::tibble(
    ref = refs,
    type = type,
    package = refs,
    version = NA_character_,
    sources = replicate(length(refs), NA_character_, simplify = FALSE),
    direct = direct,
    status = "FAILED",
    remote = parse_remotes(refs),
    error = replicate(length(refs), err, simplify = FALSE)
  )
}

satisfies_remote <- function(resolution, candidate, config,
                             remote_types = NULL, ...) {
  remote_types <- c(default_remote_types(), remote_types)
  sat <- remote_types[[resolution$type]]$satisfy
  if (is.null(sat)) return(resolution$ref == candidate$ref)

  sat(resolution, candidate, config, ...)
}

solve_dummy_obj <- 1000000000

remotes_solve <- function(self, private, policy) {
  "!DEBUG starting to solve `length(private$resolution$packages)` packages"
  if (is.null(private$library)) {
    stop("No package library specified, see 'library' in new()")
  }
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  metadata <- list(solution_start = Sys.time())
  pkgs <- self$get_resolution()

  prb <- private$create_lp_problem(pkgs, policy)
  sol <- private$solve_lp_problem(prb)

  if (sol$status != 0) {
    stop("Cannot solve installation, internal lpSolve error ", sol$status)
  }

  selected <- as.logical(sol$solution[seq_len(nrow(pkgs))])
  res <- list(
    status = if (sol$objval < solve_dummy_obj - 1) "OK" else "FAILED",
    data = private$subset_resolution(selected),
    problem = prb,
    solution = sol
  )

  res$data$lib_status <- calculate_lib_status(res$data, pkgs)
  res$data$cache_status <-
    calculate_cache_status(res$data, private$cache)

  metadata$solution_end <- Sys.time()
  attr(res, "metadata") <- utils::modifyList(attr(pkgs, "metadata"), metadata)
  class(res) <- unique(c("remotes_solution", class(res)))

  if (res$status == "FAILED") {
    res$failures <- describe_solution_error(pkgs, res)
  }

  private$solution$result <- res
  self$get_solution()
}

remotes_stop_for_solve_error <- function(self, private) {
  if (is.null(private$solution)) {
    stop("No solution found, need to call $solve()")
  }

  sol <- self$get_solution()

  if (sol$status != "OK") {
    msg <- paste(format(sol$failures), collapse = "\n")
    stop("Cannot install packages:\n", msg, call. = FALSE)
  }
}

#' Create the LP problem that solves the installation
#'
#' Each row in the resolution data frame is an installation candidate.
#' Each row corresponds to a binary variable \eqn{p_i}{p[i]}, which is
#' 1 if that package will be installed.
#'
#' TODO
#'
#' And we want to minimize package downloads and package compilation:
#' 6. If a package is already installed, prefer the installed version,
#'    if possible.
#' 7. If a package is available as a binary, prefer the binary version,
#'    if possible.
#'
#' We do this by assigning cost 1 to installed versions, cost 2 to
#' binary packages, and cost 3 to source packages. Then we minimize the
#' total cost, while satisfying the constraints.
#'
#' Other cost schemes will be added later.
#'
#' @param pkgs Resolution data frame, that contains the locally installed
#'   packages as well.
#' @param policy Version selection policy.
#' @return An S3 object for a linear (integer) optimization problem,
#'   to be used with [lpSolve::lp()] (eventually).
#'
#' @keywords internal
#' @noRd

remotes__create_lp_problem <- function(self, private, pkgs, policy) {
  remotes_i_create_lp_problem(pkgs, policy)
}

## Add a condition, for a subset of variables, with op and rhs
remotes_i_lp_add_cond <- function(
  lp, vars, op = "<=", rhs = 1, coef = rep(1, length(vars)),
  type = NA_character_, note = NULL) {

  lp$conds[[length(lp$conds)+1]] <-
    list(vars = vars, coef = coef, op = op, rhs = rhs, type = type,
         note = note)
  lp
}

## This is a separate function to make it testable without a `remotes`
## object.
##
## Variables:
## * 1:num are candidates
## * (num+1):(num+num_direct_pkgs) are the relax variables for direct refs

remotes_i_create_lp_problem <- function(pkgs, policy) {
  "!DEBUG creating LP problem"

  ## TODO: we could already rule out (standard) source packages if binary
  ## with the same version is present

  ## TODO: we could already rule out (standard) source and binary packages
  ## if an installed ref with the same version is present

  lp <- remotes_i_lp_init(pkgs, policy)
  lp <- remotes_i_lp_objectives(lp)
  lp <- remotes_i_lp_no_multiples(lp)
  lp <- remotes_i_lp_satisfy_direct(lp)
  lp <- remotes_i_lp_failures(lp)
  lp <- remotes_i_lp_prefer_installed(lp)
  lp <- remotes_i_lp_prefer_binaries(lp)
  lp <- remotes_i_lp_dependencies(lp)

  lp
}

remotes_i_lp_init <- function(pkgs, policy) {
  num_candidates <- nrow(pkgs)
  packages <- unique(pkgs$package)
  direct_packages <- unique(pkgs$package[pkgs$direct])
  indirect_packages <- setdiff(packages, direct_packages)
  num_direct <- length(direct_packages)

  structure(list(
    ## Number of package candidates
    num_candidates = num_candidates,
    ## Number of directly specified ones
    num_direct = num_direct,
    ## Total number of variables. For direct ones, we have an extra variable
    total = num_candidates + num_direct,
    ## Constraints to fill in
    conds = list(),
    pkgs = pkgs,
    policy = policy,
    ## All package names
    packages = packages,
    ## The names of the direct packages
    direct_packages = direct_packages,
    ## The names of the indirect packages
    indirect_packages = indirect_packages,
    ## Candidates (indices) that have been ruled out. E.g. resolution failed
    ruled_out = integer()
  ), class = "remotes_lp_problem")
}

## Coefficients of the objective function, this is very easy
## TODO: rule out incompatible platforms
## TODO: use rversion as well, for installed and binary packages

remotes_i_lp_objectives <- function(lp) {

  pkgs <- lp$pkgs
  policy <- lp$policy
  num_candidates <- lp$num_candidates

  if (policy == "lazy") {
    ## Simple: installed < binary < source
    lp$obj <- ifelse(pkgs$type == "installed", 1,
              ifelse(pkgs$platform == "source", 3, 2))

  } else if (policy == "upgrade") {
    ## Sort the candidates of a package according to version number
    lp$obj <- rep((num_candidates + 1) * 100, num_candidates)
    whpp <- pkgs$status == "OK" & !is.na(pkgs$version)
    pn <- unique(pkgs$package[whpp])
    for (p in pn) {
      whp <-  whpp & pkgs$package == p
      v <- pkgs$version[whp]
      r <- rank(package_version(v), ties.method = "min")
      lp$obj[whp] <- (max(r) - r + 1) * 100
      lp$obj[whp] <- lp$obj[whp] - min(lp$obj[whp])
    }
    lp$obj <- lp$obj + ifelse(pkgs$type == "installed", 0,
                       ifelse(pkgs$platform == "source", 2, 1))
    lp$obj <- lp$obj - min(lp$obj)

  } else {
    stop("Unknown version selection policy")
  }

  lp$obj <- c(lp$obj, rep(solve_dummy_obj, lp$num_direct))

  lp
}

remotes_i_lp_no_multiples <- function(lp) {

  ## 1. Each directly specified package exactly once.
  ##    (We also add a dummy variable to catch errors.)
  for (p in seq_along(lp$direct_packages)) {
    pkg <- lp$direct_packages[p]
    wh <- which(lp$pkgs$package == pkg)
    lp <- remotes_i_lp_add_cond(
      lp, c(wh, lp$num_candidates + p),
      op = "==", type = "exactly-once")
  }

  ## 2. Each non-direct package must be installed at most once
  for (p in seq_along(lp$indirect_packages)) {
    pkg <- lp$indirect_packages[p]
    wh <- which(lp$pkgs$package == pkg)
    lp <- remotes_i_lp_add_cond(lp, wh, op = "<=", type = "at-most-once")
  }

  lp
}

remotes_i_lp_satisfy_direct <-  function(lp) {

  ## 3. Direct refs must be satisfied
  satisfy <- function(wh) {
    pkgname <- lp$pkgs$package[[wh]]
    res <- lp$pkgs[wh, ]
    others <- setdiff(which(lp$pkgs$package == pkgname), wh)
    for (o in others) {
      res2 <- lp$pkgs[o, ]
      if (! isTRUE(satisfies_remote(res, res2))) {
        lp <<- remotes_i_lp_add_cond(
          lp, o, op = "==", rhs = 0, type = "satisfy-refs", note = wh)
      }
    }
  }
  lapply(seq_len(lp$num_candidates)[lp$pkgs$direct], satisfy)

  lp
}

remotes_i_lp_dependencies <- function(lp) {

  pkgs <- lp$pkgs
  num_candidates <- lp$num_candidates
  ruled_out <- lp$ruled_out
  base <- base_packages()

  ## 4. Package dependencies must be satisfied
  depconds <- function(wh) {
    if (pkgs$status[wh] != "OK") return()
    deps <- pkgs$deps[[wh]]
    deptypes <- pkgs$dep_types[[wh]]
    deps <- deps[deps$ref != "R", ]
    deps <- deps[! deps$ref %in% base, ]
    deps <- deps[tolower(deps$type) %in% tolower(deptypes), ]
    if (pkgs$platform[wh] != "source") {
      deps <- deps[tolower(deps$type) != "linkingto", ]
    }
    for (i in seq_len(nrow(deps))) {
      depref <- deps$ref[i]
      depver <- deps$version[i]
      depop  <- deps$op[i]
      deppkg <- deps$package[i]
      ## See which candidate satisfies this ref
      res <- pkgs[match(depref, pkgs$ref), ]
      cand <- which(pkgs$package == deppkg)
      good_cand <- Filter(
        x = cand,
        function(c) {
          candver <- pkgs$version[c]
          pkgs$status[[c]] != "FAILED" &&
            isTRUE(satisfies_remote(res, pkgs[c, ])) &&
            (depver == "" || version_satisfies(candver, depop, depver))
        })
      bad_cand <- setdiff(cand, good_cand)

      report <- c(
        if (length(good_cand)) {
          gc <- paste(pkgs$ref[good_cand], pkgs$version[good_cand])
          paste0("version ", paste(gc, collapse = ", "))
        },
        if (length(bad_cand)) {
          bc <- paste(pkgs$ref[bad_cand], pkgs$version[bad_cand])
          paste0("but not ", paste(bc, collapse = ", "))
        },
        if (! length(cand)) "but no candidates"
      )
      txt <- glue::glue("{pkgs$ref[wh]} depends on {depref}: \\
                   {glue::glue_collapse(report, sep = ', ')}")
      note <- list(wh = wh, ref = depref, cand = cand,
                   good_cand = good_cand, txt = txt)

      lp <<- remotes_i_lp_add_cond(
        lp, c(wh, good_cand), "<=", rhs = 0,
        coef = c(1, rep(-1, length(good_cand))),
        type = "dependency", note = note
      )
    }
  }
  lapply(setdiff(seq_len(num_candidates), ruled_out), depconds)

  lp
}

remotes_i_lp_failures <- function(lp) {

  ## 5. Can't install failed resolutions
  failedconds <- function(wh) {
    if (lp$pkgs$status[wh] != "FAILED") return()
    lp <<- remotes_i_lp_add_cond(lp, wh, op = "==", rhs = 0,
                                 type = "ok-resolution")
    lp$rules_out <<- c(lp$ruled_out, wh)
  }
  lapply(seq_len(lp$num_candidates), failedconds)

  lp
}

remotes_i_lp_prefer_installed <- function(lp) {
  pkgs <- lp$pkgs
  inst <- which(pkgs$type == "installed")
  for (i in inst) {
    ## If not a CRAN or BioC package, skip it
    repotype <- pkgs$extra[[i]]$repotype
    if (is.null(repotype) || ! repotype %in% c("cran", "bioc")) next

    ## Look for others with cran/bioc/standard type and same name & ver
    package <- pkgs$package[i]
    version <- pkgs$version[i]

    ruledout <- which(pkgs$type %in% c("cran", "bioc", "standard") &
                      pkgs$package == package & pkgs$version == version)
    lp$ruled_out <- c(lp$ruled_out, ruledout)
    for (r in ruledout) {
      lp <- remotes_i_lp_add_cond(lp, r, op = "==", rhs = 0,
                                  type = "prefer-installed")
    }
  }

  lp
}

remotes_i_lp_prefer_binaries <- function(lp) {
  pkgs <- lp$pkgs
  str <- paste0(pkgs$type, "::", pkgs$package, "@", pkgs$version)
  for (ustr in unique(str)) {
    same <- which(ustr == str)
    ## We can't do this for other packages, because version is
    ## exclusive for those
    if (! pkgs$type[same[1]] %in% c("cran", "bioc", "standard")) next
    ## TODO: choose the right one for the current R version
    selected <- same[pkgs$platform[same] != "source"][1]
    ## No binary package
    if  (is.na(selected)) next
    ruledout <- setdiff(same, selected)
    lp$ruled_out <- c(lp$ruled_out, ruledout)
    for (r in ruledout) {
      lp <- remotes_i_lp_add_cond(lp, r, op = "==", rhs = 0,
                                  type = "prefer-binary")
    }
  }

  lp
}

#' @noRd

print.remotes_lp_problem <- function(x, ...) {
  cat(format(x, ...))
}

#' @noRd

format.remotes_lp_problem <- function(x, ...) {

  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  format_cond <- function(cond) {
    if (cond$type == "dependency") {
      push(format_line(" * {cond$note$txt}"))

    } else if (cond$type == "satisfy-refs") {
      ref <- x$pkgs$ref[cond$note]
      cand <- x$pkgs$ref[cond$vars]
      push(format_line(" * `{ref}` is not satisfied by `{cand}`"))

    } else if (cond$type == "ok-resolution") {
      ref <- x$pkgs$ref[cond$vars]
      push(format_line(" * `{ref}` resolution failed"))

    } else if (cond$type == "prefer-installed") {
      ref <- x$pkgs$ref[cond$vars]
      push(format_line(" * installed is preferred for `{ref}`"))

    } else if (cond$type == "prefer-binary")  {
      ref <- x$pkgs$ref[cond$vars]
      push(format_line(" * binary is preferred for `{ref}`"))

    } else if (cond$type == "exactly-once") {
      ## Do nothing

    } else if (cond$type == "at-most-once") {
      ## Do nothing

    } else {
      push(format_line(" * Unknown condition"))
    }
  }

  push(format_line("LP problem for {x$num_candidates} refs:"))
  pn <- sort(x$pkgs$ref)
  push(format_line(strwrap(paste(pn, collapse = ", "), indent = 2, exdent = 2)))
  nc <- length(x$conds) - x$num_direct

  if (nc > 0) {
    push(format_line("Constraints:"))
    lapply(x$conds, format_cond)
  } else {
    push(format_line("No constraints"))
  }

  paste0(result, collapse = "")
}

remotes__solve_lp_problem <- function(self, private, problem) {
  res <- remotes_i_solve_lp_problem(problem)
  res
}

remotes_i_solve_lp_problem <- function(problem) {
  "!DEBUG solving LP problem"
  condmat <- matrix(0, nrow = length(problem$conds), ncol = problem$total)
  for (i in seq_along(problem$conds)) {
    cond <- problem$conds[[i]]
    condmat[i, cond$vars] <- cond$coef
  }

  dir <- vcapply(problem$conds, "[[", "op")
  rhs <- vapply(problem$conds, "[[", "rhs", FUN.VALUE = double(1))
  lpSolve::lp("min", problem$obj, condmat, dir, rhs, int.vec = seq_len(problem$total))
}

remotes_get_solution <- function(self, private) {
  if (is.null(private$solution)) {
    stop("No solution found, need to call $solve()")
  }
  private$solution$result
}

remotes_install_plan <- function(self, private) {
  "!DEBUG creating install plan"
  sol <- self$get_solution_download()
  if (inherits(sol, "remotes_solve_error")) return(sol)

  deps <- lapply(
    seq_len(nrow(sol)),
    function(i) {
      x <- sol$deps[[i]]
      x$package[tolower(x$type) %in% tolower(sol$dep_types[[i]])]
    })
  deps <- lapply(deps, setdiff, y = c("R", base_packages()))
  installed <- ifelse(
    sol$type == "installed",
    file.path(private$library, sol$package),
    NA_character_)

  res <- self$get_resolution()
  direct_packages <- res$package[res$direct]
  direct <- sol$direct |
    (sol$type == "installed" & sol$package %in% direct_packages)

  binary = sol$platform != "source"
  vignettes <- ! binary & ! sol$type %in% c("cran", "bioc", "standard")

  sol$binary <- binary
  sol$direct <- direct
  sol$dependencies <- I(deps)
  sol$file <- sol$fulltarget
  sol$installed <- installed
  sol$vignettes <- vignettes

  sol
}

calculate_lib_status <- function(sol, res) {
  ## Possible values at the moment:
  ## - virtual: not really a package
  ## - new: newly installed
  ## - current: up to date, not installed
  ## - update: will be updated
  ## - no-update: could update, but won't

  sres <- res[res$package %in% sol$package, c("package", "version", "type")]

  ## Check if it is not new
  lib_ver <- vcapply(sol$package, function(p) {
    c(sres$version[sres$package == p & sres$type == "installed"],
      NA_character_)[1]
  })

  ## If not new, and not "installed" type, that means update
  status <- ifelse(
    sol$type == "deps", "virtual",
      ifelse(is.na(lib_ver), "new",
        ifelse(sol$type == "installed", "current", "update")))

  ## Check for no-update
  could_update <- vlapply(seq_along(sol$package), function(i) {
    p <- sol$package[i]
    v <- if (is.na(sol$version[i])) NA_character_ else package_version(sol$version[i])
    g <- sres$package == p & !is.na(sres$version)
    any(v < sres$version[g])
  })
  status[status == "current" & could_update] <- "no-update"

  status
}

## TODO: non-CRAN packages? E.g. GH based on sha.

calculate_cache_status <- function(soldata, cache) {
  toinst <- soldata$sha256[soldata$type != "installed"]
  cached <- cache$package$find(sha256 = toinst)
  ifelse(soldata$type == "installed", NA_character_,
         ifelse(soldata$sha256 %in% cached$sha256, "hit", "miss"))
}

describe_solution_error <- function(pkgs, solution) {
  assertthat::assert_that(
    ! is.null(pkgs),
    ! is.null(solution),
    solution$solution$objval >= solve_dummy_obj - 1L
  )

  num <- nrow(pkgs)
  if (!num) stop("No solution errors to describe")
  sol <- solution$solution$solution
  sol_pkg <- sol[1:num]
  sol_dum <- sol[(num+1):solution$problem$total]

  ## For each candidate, we work out if it _could_ be installed, and if
  ## not, why not. Possible cases:
  ## 1. it is is in the install plan, so it can be installed, YES
  ## 2. it failed resolution, so NO
  ## 3. it does not satisfy a direct ref for the same package, so NO
  ## 4. it conflicts with another to-be-installed candidate of the
  ##    same package, so NO
  ## 5. one of its (downstream) dependencies cannot be installed, so NO
  ## 6. otherwise YES

  FAILS <- c("failed-res", "satisfy-direct", "conflict", "dep-failed")

  state <- rep("maybe-good", num)
  note <- replicate(num, NULL)
  downstream <- replicate(num, character(), simplify = FALSE)

  state[sol_pkg == 1] <- "installed"

  ## Candidates that failed resolution
  cnd <- solution$problem$conds
  typ <- vcapply(cnd, "[[", "type")
  var <- lapply(cnd, "[[", "vars")
  fres_vars <- unlist(var[typ == "ok-resolution"])
  state[fres_vars] <- "failed-res"
  for (fv in fres_vars) {
    if (length(e <- pkgs$error[[fv]])) {
      note[[fv]] <- c(note[[fv]], conditionMessage(e))
    }
  }

  ## Candidates that conflict with a direct package
  for (w in which(typ == "satisfy-refs")) {
    sv <- var[[w]]
    down <- pkgs$ref[sv]
    up <- pkgs$ref[cnd[[w]]$note]
    state[sv] <- "satisfy-direct"
    note[[sv]] <- c(note[[sv]], glue::glue("Conflicts {up}"))
  }

  ## Find "conflict". These are candidates that are not installed,
  ## and have an "at-most-once" constraint with another package that will
  ## be installed. So we just go over these constraints.
  for (c in cnd[typ == "at-most-once"]) {
    is_in <- sol_pkg[c$vars] != 0
    if (any(is_in)) {
      state[c$vars[!is_in]] <- "conflict"
      package <- pkgs$package[c$vars[1]]
      inst <- pkgs$ref[c$vars[is_in]]
      vv <- c$vars[!is_in]
      for (v in vv) {
        note[[v]] <- c(
          note[[v]],
          glue::glue("{pkgs$ref[v]} conflict with {inst}, to be installed"))
      }
    }
  }

  ## Find "dep-failed". This is the trickiest. First, if there are no
  ## condidates at all
  type_dep <- typ == "dependency"
  dep_up <- viapply(cnd[type_dep], function(x) x$vars[1])
  dep_cands <- lapply(cnd[type_dep], function(x) x$vars[-1])
  no_cands <- which(! viapply(dep_cands, length) &
                    state[dep_up] == "maybe-good")
  for (x in no_cands) {
    pkg <- cnd[type_dep][[x]]$note$ref
    state[dep_up[x]] <- "dep-failed"
    note[[ dep_up[x] ]] <-
      c(note[[ dep_up[x] ]], glue::glue("Cannot install dependency {pkg}"))
    downstream[[ dep_up[x] ]] <- c(downstream[[ dep_up[x] ]], pkg)
  }

  ## Then we start with the already known
  ## NO answers, and see if they rule out upstream packages
  new <- which(state %in% FAILS)
  while (length(new)) {
    dep_cands <- lapply(dep_cands, setdiff, new)
    which_new <- which(!viapply(dep_cands, length) & state[dep_up] == "maybe-good")
    for (x in which_new) {
      pkg <- cnd[type_dep][[x]]$note$ref
      state[ dep_up[x] ] <- "dep-failed"
      note[[ dep_up[x] ]] <- c(
        note[[ dep_up[x] ]], glue::glue("Cannot install dependency {pkg}"))
      downstream[[ dep_up[x] ]] <- c(downstream[[ dep_up[x] ]], pkg)
    }
    new <- dep_up[which_new]
  }

  ## The rest is good
  state[state == "maybe-good"] <- "could-be"

  wh <- state %in% FAILS
  fails <- pkgs[wh, ]
  fails$failure_type <- state[wh]
  fails$failure_message <-  note[wh]
  fails$failure_down <- downstream[wh]
  class(fails) <- unique(c("remote_solution_error", class(fails)))

  fails
}

#' @noRd

format.remote_solution_error <- function(x, ...) {
  fails <- x
  if (!nrow(fails)) return()

  done <- rep(FALSE, nrow(x))
  res <- character()

  do <- function(i) {
    if (done[i]) return()
    done[i] <<- TRUE
    msgs <- unique(fails$failure_message[[i]])
    res <<- c(
      res, paste0(
             glue::glue("  * Cannot install `{fails$ref[i]}`."),
             if (length(msgs)) paste0("\n    - ", msgs)
           )
    )
    down <- which(fails$ref %in% fails$failure_down[[i]])
    lapply(down, do)
  }

  direct_refs <- which(fails$direct)
  lapply(direct_refs, do)

  paste(unique(res), collapse = "\n")
}

#' @noRd

print.remote_solution_error <- function(x, ...) {
  cat(format(x, ...))
}

#' @noRd

format.remote_resolution_error  <- function(x, ...) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))
  push(crayon::bold(crayon::red("Errors:")), sep = "\n")
  push(format(x), sep = "\n")
  paste0(result, collapse = "")
}

print.remotes_solution <- function(x, ...) {
  cat(format(x, ...))
}

format.remotes_solution <- function(x, ...) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  meta <- attr(x, "metadata")
  data <- x$data

  direct <- unique(data$ref[data$direct])
  dt <- prettyunits::pretty_dt(meta$resolution_end - meta$resolution_start)
  dt2 <- prettyunits::pretty_dt(meta$solution_end - meta$solution_start)
  sol <- if (x$status == "OK") "SOLUTION" else "FAILED SOLUTION"
  head <- glue::glue(
    "PAK {sol}, {length(direct)} refs, resolved in {dt}, ",
    "solved in {dt2} ")
  width <- getOption("width") - crayon::col_nchar(head, type = "width") - 1
  head <- paste0(head, strrep(cli::symbol$line, max(width, 0)))
  if (x$status == "OK") {
    push(crayon::blue(crayon::bold(head)), sep = "\n")
  } else {
    push(crayon::red(crayon::bold(head)), sep = "\n")
  }

  push(format_refs(data, data$direct, header = NULL))

  push(format_refs(data, (! data$direct), header = "Dependencies", by_type = TRUE))

  if (!is.null(x$failures)) push(format(x$failures))

  paste0(result, collapse = "")
}

remotes_draw_tree <- function(self, private, pkgs) {

  assertthat::assert_that(is.null(pkgs) || is_character(pkgs))

  sol <- self$get_solution()$data
  pkgs <- pkgs %||% sol$package[sol$direct]

  data <- sol[, c("package", "deps")]
  deps <- lapply(sol$deps, "[[", "package")
  deps <- lapply(deps, intersect, data$package)
  data$deps <- deps
  data$label <- paste(
    data$package,
    crayon::silver(paste0("(", sol$version, ")"))
  )
  data$label[sol$direct] <- crayon::italic(crayon::bold(crayon::cyan(data$label[sol$direct])))

  trees <- unlist(lapply(pkgs, function(p) c(cli::tree(data, root = p), "")))
  class(trees) <- c("tree", "character")
  trees
}

## ------------------------------------------------------------------------
## API

parse_remote_bioc <- function(specs, config, ...) {

  ## BioC is the same as CRAN, except for cran:: -> bioc::
  parsed_specs <- rematch2::re_match(specs, standard_rx("bioc"))
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "bioc"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_bioc <- function(remote, direct, config, cache,
                                dependencies, progress_bar, ...) {
  resolve_from_metadata(remote, direct, config, cache, dependencies)
}

download_remote_bioc <- function(resolution, target, config, cache,
                                 which, on_progress) {

  download_ping_if_not_source(resolution, target, config, cache,
                              on_progress)
}

satisfy_remote_bioc <- function(resolution, candidate,
                                config, ...) {

  ## 1. candidate must be a bioc, standard or installed ref
  if (!candidate$type %in% c("bioc", "standard", "installed")) {
    return(structure(
      FALSE, reason = "Type must be 'bioc', 'standard' or 'installed'"))
  }

  ## 2. installed refs must be from bioc
  if (candidate$type == "installed") {
    dsc <- candidate$extra[[1]]$description
    if (is.null(dsc) || is.na(dsc$get("biocViews"))) {
      return(structure(FALSE, reason = "Installed package not from BioC"))
    }
  }

  ## 3. package names must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 4. version requirements must be satisfied. Otherwise good.
  if (resolution$remote[[1]]$version == "") {
    return(TRUE)
  }

  if (!version_satisfies(
         candidate$version,
         resolution$remote[[1]]$atleast,
         resolution$remote[[1]]$version)) {
    return(structure(FALSE, reason = "Insufficient version"))
  }

  TRUE
}

## ----------------------------------------------------------------------
## Internal functions

type_bioc_matching_bioc_version <- function(r_version) {
  if (r_version >= "3.5") {
    "3.7"
  } else if (r_version >= "3.4") {
    "3.6"
  } else if (r_version >= "3.3.0") {
    "3.4"
  } else if (r_version >= "3.2") {
    "3.2"
  } else if (r_version >= "3.1.1") {
    "3.0"
  } else if (r_version == "3.1.0") {
    "2.14"
  } else if (r_version >= "2.15" && r_version <= "2.16") {
    "2.11"
  } else {
    stop("Cannot get matching Bioconductor version for ", r_version)
  }
}

type_bioc_get_bioc_repos <- function(r_version) {
  bv <- type_bioc_matching_bioc_version(r_version)
  tmpl <- c(
    BioCsoft  = "https://bioconductor.org/packages/{bv}/bioc",
    BioCann   = "https://bioconductor.org/packages/{bv}/data/annotation",
    BioCexp   = "https://bioconductor.org/packages/{bv}/data/experiment",
    BioCextra = if (package_version(bv) <= 3.5) {
                  "https://bioconductor.org/packages/{bv}/extra"
                }
  )
  list(
    repos = vcapply(tmpl, glue::glue_data, .x = list(bv = bv)),
    version = bv
  )
}

## ------------------------------------------------------------------------
## API

parse_remote_cran <- function(specs, ...) {

  parsed_specs <- rematch2::re_match(specs, standard_rx("cran"))

  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "cran"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_cran <- function(remote, direct, config, cache,
                                dependencies, ...) {
  force(remote); force(direct); force(dependencies)
  versions <- if ("type" %in% names(remote)) {
    remote$version
  } else  {
    vcapply(remote, "[[", "version")
  }

  if (all(versions %in% c("", "current"))) {
    type_cran_resolve_current(remote, direct, config, cache, dependencies)
  } else {
    type_cran_resolve_version(remote, direct, config, cache, dependencies)
  }
}

download_remote_cran <- function(resolution, target, config, cache,
                                 which, on_progress) {

  download_ping_if_no_sha(resolution, target, config, cache,
                          on_progress)
}

satisfy_remote_cran <- function(resolution, candidate, config, ...) {

  ## 1. candidate must be a cran, standard or installed ref
  if (!candidate$type %in% c("cran", "standard", "installed")) {
    return(structure(
      FALSE, reason = "Type must be 'cran', 'standard' or 'installed'"))
  }

  ## 2. installed refs must be from CRAN
  if (candidate$type == "installed") {
    dsc <- candidate$extra[[1]]$description
    if (is.null(dsc) ||
        ! identical(dsc$get("Repository")[[1]], "CRAN")) {
      return(structure(FALSE, reason = "Installed package not from CRAN"))
    }
  }

  ## 3. package names must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 4. version requirements must be satisfied. Otherwise good.
  if (resolution$remote[[1]]$version == "") return(TRUE)

  if (!version_satisfies(
         candidate$version,
         resolution$remote[[1]]$atleast,
         resolution$remote[[1]]$version)) {
    return(structure(FALSE, reason = "Insufficient version"))
  }

  TRUE
}

## ----------------------------------------------------------------------
## Internal functions

type_cran_resolve_current <- function(remote, direct, config, cache,
                                      dependencies) {
  resolve_from_metadata(remote, direct, config, cache, dependencies)
}

type_cran_resolve_version <- function(remote, direct, config,
                                      crancache, dependencies) {
  ## TODO
  stop("Not implemented yet")
}

parse_remote_deps <- function(specs, config, ...) {
  parsed_specs <- rematch2::re_match(specs, type_deps_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "deps"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_deps <- function(remote, direct, config, cache,
                                     dependencies, ...) {

  ret <- resolve_remote_local(remote, direct, config, cache,
                              dependencies, ...)
  ret$sources <- list(character())
  ret
}

download_remote_deps <- function(resolution, target, config, cache,
                                  which, on_progress) {
  ## Nothing to do here
  "Had"
}

satisfy_remote_deps <- function(resolution, candidate, config, ...) {
  ## TODO: we can probably do better than this
  FALSE
}

## ----------------------------------------------------------------------
## Internal functions

type_deps_rx <- function() {
  paste0(
    "^",
    "(?:deps::)",
    "(?<path>.*)",
    "$"
  )
}

### -----------------------------------------------------------------------
### API

parse_remote_github <- function(specs, config, ...) {

  pds <- rematch2::re_match(specs, github_rx())
  if (any(unk <- is.na(pds$.match))) {
    pds[unk] <- rematch2::re_match(specs[unk], github_url_rx())
    pds[unk, "subdir"] <- ""
  }

  pds$ref <- pds$.text
  cn <- setdiff(colnames(pds), c(".match", ".text"))
  pds <- pds[, cn]
  pds$type <- "github"
  pds$package <- ifelse(nzchar(pds$package), pds$package, pds$repo)
  lapply(
    seq_len(nrow(pds)),
    function(i) as.list(pds[i,])
  )
}

resolve_remote_github <- function(remote, direct, config, cache,
                                  dependencies, ...) {

  force(direct); force(dependencies)
  ## Get the DESCRIPTION data, and the SHA we need
  desc <- type_github_get_github_description_data(remote)
  sha <- type_github_get_github_commit_sha(remote)
  asNamespace("pkgcache")$when_all(
    desc = desc, sha = sha, remote = remote, direct = direct,
    dependencies = dependencies[[2 - direct]])$
    then(type_github_make_resolution)
}

download_remote_github <- function(resolution, target, config, cache,
                                   which, on_progress) {

  ## A GitHub package needs to be built, from the downloaded repo
  ## If we are downloading a solution, then we skip building the vignettes,
  ## because these will be built later by pkginstall.
  ##
  ## We cache both the downloaded repo snapshot and the built package in
  ## the package cache. So this is how we go:
  ##
  ## 1. If there is a built package in the cache (including vignettes
  ##    if they are needed), then we just use that.
  ## 2. If there is a repo snapshot in the cache, we build an R package
  ##    from it. (Add also add it to the cache.)
  ## 3. Otherwise we download the repo, add it to the cache, build the
  ##    R package, and add that to the cache as well.

  package <- resolution$package
  sha <- resolution$extra[[1]]$sha
  need_vignettes <- which == "resolution"

  ## 1. Check if we have a built package in the cache. We don not check the
  ## ref or the type, so the package could have been built from a local
  ## ref or from another repo. As long as the sha is the same, we are
  ## fine. If we don't require vignetted, then a package with or without
  ## vignettes is fine.

  hit <- cache$package$copy_to(
    target, package = package, sha = sha, built = TRUE,
    .list = c(if (need_vignettes) c(vignettes = TRUE)))
  if (nrow(hit)) {
    "!DEBUG found GH `resolution$ref`@`sha` in the cache"
    return("Had")
  }

  ## 2. Check if we have a repo snapshot in the cache.

  target_zip <- sub("\\.tar\\.gz$", ".zip", target)
  rel_target <- resolution$target
  subdir <- resolution$remote[[1]]$subdir
  hit <- cache$package$copy_to(
    target_zip, package = package, sha = sha, built = FALSE)
  if (nrow(hit)) {
    "!DEBUG found GH zip for `resolution$ref`@`sha` in the cache"
    return(type_github_build_package(target_zip, target, rel_target, subdir,
                                     package, sha, need_vignettes, cache))
  }

  ## 3. Need to download the repo

  "!DEBUG Need to download GH package `resolution$ref`@`sha`"
  urls <- resolution$sources[[1]]
  rel_zip <- sub("\\.tar\\.gz$", ".zip", rel_target)
  type_github_download_repo(urls, target_zip, rel_zip, sha, package, cache,
                            on_progress)$
    then(function() {
      "!DEBUG Building package `resolution$package`"
      type_github_build_package(target_zip, target, rel_target, subdir,
                                package, sha, need_vignettes, cache)
    })
}

type_github_build_package <- function(repo_zip, target, rel_target, subdir,
                                      package, sha, vignettes, cache) {
  mkdirp(tmpdir <- tempfile())
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  zipfile <- file.path(tmpdir, basename(repo_zip))
  file.copy(repo_zip, zipfile)

  pkgdir <- file.path(tmpdir, unzip(zipfile))[1]
  if (!nzchar(subdir)) pkgdir <- file.path(pkgdir, subdir)
  pkgfile <- build_package(
    pkgdir, build_args = list(vignettes = vignettes))

  file.copy(pkgfile, target)
  cache$package$add(
    target, rel_target, package = package, sha = sha, built = TRUE,
    vignettes = vignettes)
  "Built"
}

type_github_download_repo <- function(urls, repo_zip, rel_zip, sha,
                                      package, cache, on_progress) {
  ## TODO: progress
  asNamespace("pkgcache")$download_file(urls, repo_zip,
                                        on_progress = on_progress)$
    then(function() {
      cache$package$add(
        repo_zip, rel_zip, package = package, sha = sha, built = FALSE)
      "Got"
    })
}

## ----------------------------------------------------------------------

satisfy_remote_github <- function(resolution, candidate,
                                    config, ...) {

  ## 1. package name must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 1. installed ref is good, if it has the same sha
  if (candidate$type == "installed") {
    sha1 <- candidate$extra[[1]]$remotesha
    sha2 <- resolution$extra[[1]]$sha
    ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
    if (!ok) {
      return(structure(FALSE, reason = "Installed package sha mismatch"))
    } else {
      return(TRUE)
    }
  }

  ## 2. other refs are also good, as long as they have the same sha
  sha1 <- candidate$extra[[1]]$sha
  sha2 <- resolution$extra[[1]]$sha
  ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
  if (!ok) {
    return(structure(FALSE, reason = "Candidate package sha mismatch"))
  } else {
    return(TRUE)
  }
}

## ----------------------------------------------------------------------
## Internal functions

type_github_get_github_headers <- function() {
  headers <- c("Accept" = "application/vnd.github.v3+json")

  if (nzchar(token <- Sys.getenv("GITHUB_TOKEN",
                                 Sys.getenv("GITHUB_PAT")))) {
    headers <- c(headers, c("Authorization" = paste("token", token)))
  }
  headers
}

type_github_get_github_description_url <- function(rem) {
  commitish <- if (nzchar(rem$commitish)) rem$commitish else "master"
  subdir <- if (!is.null(rem$subdir)) utils::URLencode(rem$subdir)
  glue::glue(
    "https://api.github.com/repos/{rem$username}/{rem$repo}/",
    "contents/{subdir}/DESCRIPTION?ref={commitish}")
}

type_github_get_github_commit_url <- function(rem) {
  glue::glue(
    "https://api.github.com/repos/{rem$username}/{rem$repo}",
    "/git/trees/{commitish}",
    commitish = if (nzchar(rem$commitish)) rem$commitish else "master"
  )
}

## Returns a deferred value

type_github_get_github_description_data <- function(rem) {
  description_url <- type_github_get_github_description_url(rem)
  github_get(description_url)$
    then(function(resp) {
      obj <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      txt <- rawToChar(base64enc::base64decode(obj$content))
      desc::desc(text = txt)
    })
}

## Returns a deferred value

type_github_get_github_commit_sha <- function(rem) {
  commit_url <- type_github_get_github_commit_url(rem)
  github_get(commit_url)$
    then(function(resp) {
      cdata <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      cdata$sha
    })
}

type_github_make_resolution <- function(data) {

  deps <- resolve_ref_deps(data$desc$get_deps(), data$desc$get("Remotes"))

  sha <- data$sha
  username <- data$remote$username
  repo <- data$remote$repo
  subdir <- data$remote$subdir %|z|% NULL
  commitish <- data$remote$commitish %|z|% NULL
  pull <- data$remote$pull %|z|% NULL
  release <- data$remote$release %|z|% NULL
  package <- data$desc$get_field("Package")
  version <- data$desc$get_field("Version")
  dependencies <- data$dependencies
  unknown <- deps$ref[deps$type %in% dependencies]
  unknown <- setdiff(unknown, c(base_packages(), "R"))

  meta <- c(
    RemoteType = "github",
    RemoteHost = "api.github.com",
    RemoteRepo = repo,
    RemoteUsername = username,
    RemoteRef = data$remote$ref,
    RemoteSha = sha,
    RemoteSubdir = subdir,
    GithubRepo = repo,
    GithubUsername = username,
    GithubRef = data$remote$ref,
    GithubSHA1 = sha,
    GithubSubdir = subdir)

  list(
    ref = data$remote$ref,
    type = data$remote$type,
    direct = data$direct,
    status = "OK",
    package = package,
    version = version,
    license = data$desc$get_field("License", NA_character_),
    sources = glue::glue(
      "https://api.github.com/repos/{username}/{repo}/zipball/{sha}"),
    target = glue::glue("src/contrib/{package}_{version}_{sha}.tar.gz"),
    remote = list(data$remote),
    deps = list(deps),
    unknown_deps = unknown,
    extra = list(list(sha = sha)),
    metadata = meta
  )
}

github_get <- function(url, headers = character(), ...) {

  headers <- c(headers, type_github_get_github_headers())
  asNamespace("pkgcache")$http_get(url, headers = headers, ...)$
    then(function(res) {
      if (res$status_code >= 300) {
        stop(github_error(res))
      }
      res
    })
}

github_error <- function(res) {
  res_headers <- curl::parse_headers_list(res$headers)
  ratelimit_limit <- res_headers$`x-ratelimit-limit`
  ratelimit_remaining <- res_headers$`x-ratelimit-remaining`
  ratelimit_reset <- .POSIXct(res_headers$`x-ratelimit-reset`, tz = "UTC")
  error_details <- jsonlite::fromJSON(rawToChar(res$content))$message

  pat_guidance <- ""
  if (identical(as.integer(ratelimit_remaining), 0L)) {
    pat_guidance <-
      sprintf(
"\n\nTo increase your GitHub API rate limit
  - Use `usethis::browse_github_pat()` to create a Personal Access Token.
  - Use `usethis::edit_r_environ()` and add the token as `GITHUB_PAT`."
)
  }

  msg <- sprintf(
"HTTP error %s.
  %s

Rate limit remaining: %s/%s
Rate limit reset at: %s%s",

    res$status_code,
    paste(strwrap(error_details), collapse = "\n"),
    ratelimit_remaining,
    ratelimit_limit,
    format(ratelimit_reset, usetz = TRUE),
    pat_guidance
  )

  structure(
    list(message = msg, call = NULL),
    class = c("async_http_error", "simpleError", "error", "condition"))
}

## ------------------------------------------------------------------------
## API

parse_remote_installed <- function(specs, config, ...) {
  parsed_specs <- rematch2::re_match(specs, type_installed_rx())

  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "installed"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_installed <- function(remote, direct, config,
                                     cache, dependencies, ...) {

  deps <- setdiff(dependencies[[2 - direct]], c("LinkingTo", "linkingto"))
  resolve_installed(cache, remote, direct, deps)
}

download_remote_installed <- function(resolution, target, config, cache,
                                      which, on_progress) {
  "Had"
}

satisfy_remote_installed <- function(resolution, candidate,
                                     config, ...) {
  TRUE
}

## ----------------------------------------------------------------------
## Internal functions

type_installed_rx <- function() {
  paste0(
    "^",
    "(?:installed::)?",
    "(?<library>.*)/",
    "(?<package>", package_name_rx(), ")",
    "$"
  )
}

make_installed_cache <- function(library, packages = NULL) {
  pkgs <- packages %||% list.files(library, pattern = "^[a-zA-Z]")
  meta <- drop_nulls(lapply_with_names(pkgs, function(pkg) {
    tryCatch(
      suppressWarnings(
        readRDS(file.path(library, pkg, "Meta", "package.rds"))
      ),
      error = function(e) NULL)
  }))

  all_fields <- unique(unlist(lapply(
    meta, function(x) names(x$DESCRIPTION))))
  fields <- unique(c(
    "Package", "Title", "Version", "Depends", "Suggests", "Imports",
    "LinkingTo", "Enhances", "Built", "MD5sum", "NeedsCompilation",
    "Platform", "License", "Priority", "Repository", "biocViews",
    grep("^Remote", all_fields, value = TRUE)))

  ret <- matrix(NA_character_, nrow = length(meta), ncol = length(fields))
  colnames(ret) <- tolower(fields)
  for (i in seq_along(meta)) ret[i,] <- meta[[i]]$DESCRIPTION[fields]

  pkgs <- tibble::as_tibble(ret)

  if (nrow(pkgs) == 0) {
    pkgs$ref <- character()
  } else {
    pkgs$ref <- paste0("installed::", library, "/", pkgs$package)
  }
  pkgs$type <- rep("installed", nrow(pkgs))
  pkgs$status <- rep("OK", nrow(pkgs))
  pkgs$rversion <- vcapply(meta, function(x) as.character(x$Built$R))
  pkgs$platform <- vcapply(meta, function(x) x$Built$Platform)
  pkgs$platform[pkgs$platform == ""] <- "*"
  pkgs$sources <- replicate(nrow(pkgs), character(), simplify = FALSE)
  pkgs$needscompilation <- ifelse(
    is.na(pkgs$needscompilation), NA,
    tolower(pkgs$needscompilation) %in% c("true", "yes"))

  cran <- !is.na(pkgs$repository) & pkgs$repository == "CRAN"
  bioc <- !is.na(pkgs$biocviews) & pkgs$biocviews != ""
  pkgs$repotype <- ifelse(cran, "cran", ifelse(bioc, "bioc", NA_character_))

  deps <- packages_parse_deps(pkgs)
  pkgs_deps <- split(
    deps[,-(1:2)], factor(deps$idx, levels = seq_len(nrow(pkgs))))
  pkgs$deps <- unname(pkgs_deps)
  list(pkgs = pkgs, deps = deps)
}

#' Status of packages in a ibrary
#'
#' @param library Path to library.
#' @param packages If not `NULL`, then only these packages are shown.
#' @return Data frame (tibble) the contains data about the packages
#'   installed in the library.
#'
#' @noRd

pkgdepends_lib_status <- function(library = .libPaths()[1], packages = NULL) {
  make_installed_cache(library, packages)$pkgs
}

packages_parse_deps <- function(pkgs) {
  no_pkgs <- nrow(pkgs)
  cols <- intersect(colnames(pkgs), tolower(dep_types()))
  ## as.character is for empty tibbles, e.g. from empty BioC repos
  deps <- as.character(unlist(pkgs[, cols], use.names = FALSE))
  nna <- which(!is.na(deps))
  if (length(nna)) {
    not_na_deps <- deps[nna]
    sp <- strsplit(not_na_deps, ",", fixed = TRUE)
    ll <- sapply(sp, length, USE.NAMES = FALSE)
    sp <- unlist(sp, use.names = FALSE)
    parsed <- rematch2::re_match(sp,
      paste0("^\\s*(?<package>[^(\\s]+)\\s*",
             "(?:\\((?<op>[^0-9\\s]+)\\s*(?<version>[^)\\s]+)\\))?\\s*$"))
    parsed$idx <- rep(rep(seq_len(no_pkgs), length(cols))[nna], ll)
    parsed$type <- rep(rep(cols, each = no_pkgs)[nna], ll)
    parsed$ref <- parsed$package
    parsed$upstream <- pkgs$package[parsed$idx]
    parsed <- parsed[, c("upstream", "idx", "ref", "type", "package",
                         "op", "version")]
    parsed <- parsed[order(parsed$idx), ]

  } else {
    parsed <- tibble::tibble(
      upstream = character(),
      idx = integer(),
      ref = character(),
      type = character(),
      package = character(),
      version = character(),
      op = character())
  }

  parsed
}

resolve_installed  <- function(cache, remotes, direct, dependencies) {

  dependencies <- tolower(dependencies)

  ## Single remote, or a list of remotes
  if ("ref" %in% names(remotes)) {
    packages <- remotes$package
  } else  {
    packages <- vcapply(remotes, "[[", "package")
  }

  pkgs <- cache$installed$pkgs
  cols <- c(
    "ref", "type", "status", "package", "version", "license",
    "needscompilation", "priority", "md5sum", "platform", "rversion",
    "sources", "built", "deps")
  res <- pkgs[pkgs$package %in% packages, cols]
  repotype <- pkgs$repotype[pkgs$package %in% packages]

  res$direct <- direct
  res$metadata <- get_installed_metadata(res)
  res$deps <- lapply(res$deps, function(x) x[x$type %in% dependencies,])

  extracols <- c("repotype", grep("^remote", names(pkgs), value = TRUE))
  extra <- pkgs[pkgs$package %in% packages, extracols]
  res$extra <- lapply(seq_len(nrow(res)), function(i) extra[i,])

  attr(res, "unknown_deps") <-
    setdiff(unique(unlist(lapply(res$deps, "[[", "package"))), "R")

  res
}

get_installed_metadata <- function(tab) {
  meta <- replicate(nrow(tab), character(), simplify = FALSE)
  for (i in seq_len(nrow(tab))) {
    meta[[i]] <-
      c(RemoteType = tab$type[i],
        RemoteRef = tab$ref[i],
        RemotePkgType = tab$platform[i],
        RemoteSha = tab$version[i])
  }
  meta
}

### -----------------------------------------------------------------------
### API

parse_remote_local <- function(specs, config, ...) {
  parsed_specs <- rematch2::re_match(specs, type_local_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "local"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_local <- function(remote, direct, config, cache,
                                 dependencies, ...) {

  sources <- paste0("file://", normalizePath(remote$path, mustWork = FALSE))
  resolve_from_description(remote$path, sources, remote, direct,
                           config, cache, dependencies[[2 - direct]])
}

download_remote_local <- function(resolution, target, config, cache,
                                  which, on_progress) {

  source_file <- sub("^file://",  "",  resolution$sources[[1]])
  if (! file.copy(source_file, target, overwrite =  TRUE)) {
    stop("No local file found")
  }
  "Had"
}

satisfy_remote_local <- function(resolution, candidate, config, ...) {
    ## TODO: we can probably do better than this
    FALSE
  }

## ----------------------------------------------------------------------
## Internal functions

type_local_rx <- function() {
  paste0(
    "^",
    "(?:local::)",
    "(?<path>.*)",
    "$"
  )
}

## ------------------------------------------------------------------------
## API

parse_remote_standard <- function(specs, config, ...) {

  ## This is the same as CRAN, but possibly with standard::
  parsed_specs <- rematch2::re_match(specs, standard_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "standard"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_standard <- function(remote, direct, config,
                                    cache, dependencies, ...) {
  resolve_from_metadata(remote, direct, config, cache, dependencies)
}

download_remote_standard <- function(resolution, target, config, cache,
                                     which, on_progress) {

  download_ping_if_no_sha(resolution, target, config, cache,
                          on_progress)
}

satisfy_remote_standard <- function(resolution, candidate, config, ...) {

  ## A standard ref is special, in that any ref source can satisfy it,
  ## as long as the package name is the same, and the version
  ## requirements are satisfied.

  ## 1. package name must be the same
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 2. version requirements must be satisfied
  if (resolution$remote[[1]]$version == "") return(TRUE)

  if (!version_satisfies(
         candidate$version,
         resolution$remote[[1]]$atleast,
         resolution$remote[[1]]$version)) {
    return(structure(FALSE, reason = "Insufficient version"))
  }

  TRUE
}

default_remote_types <- function() {
  default <- list(
    cran = list(
      parse = parse_remote_cran,
      resolve = resolve_remote_cran,
      download = download_remote_cran,
      satisfy = satisfy_remote_cran),
    bioc = list(
      parse = parse_remote_bioc,
      resolve = resolve_remote_bioc,
      download = download_remote_bioc,
      satisfy = satisfy_remote_bioc),
    standard = list(
      parse = parse_remote_standard,
      resolve = resolve_remote_standard,
      download = download_remote_standard,
      satisfy = satisfy_remote_standard),
    github = list(
      parse = parse_remote_github,
      resolve = resolve_remote_github,
      download = download_remote_github,
      satisfy = satisfy_remote_github),
    local = list(
      parse = parse_remote_local,
      resolve = resolve_remote_local,
      download = download_remote_local,
      satisfy = satisfy_remote_local),
    deps = list(
      parse = parse_remote_deps,
      resolve = resolve_remote_deps,
      download = download_remote_deps,
      satisfy  = satisfy_remote_deps),
    installed = list(
      parse = parse_remote_installed,
      resolve = resolve_remote_installed,
      download = download_remote_installed,
      satisfy = satisfy_remote_installed)
  )

  utils::modifyList(default, as.list(getOption("pkg.remote_types")))
}

unzip <- function(zipfile) {
  withr::with_dir(
    dirname(zipfile),
    utils::unzip(zipfile, exdir = ".", unzip = "internal")
  )

  files <- utils::unzip(zipfile, list = TRUE, unzip = "internal")[,1]

  root_files <- grep("^[^/]+/?$", files, value = TRUE)

  sub("/$", "", root_files)
}

zip_list <- function(zipfile) {
  utils::unzip(zipfile, list = TRUE, unzip = "internal")[,1]
}

repoman_data <- new.env(parent = emptyenv())

`%||%` <- function(l, r) if (is.null(l)) r else l

`%|z|%` <- function(l, r) {
  if (identical(l, "")) r else l
}

get_platform <- function() {
  .Platform
}

current_r_platform <- function() {
  type <- get_platform()$pkgType
  if (!is_string(type))
    "source"
  else if (grepl("^mac", type)) {
    "macos"
  } else if (grepl("^win", type)) {
    "windows"
  } else {
    "source"
  }
}

default_platforms <- function() unique(c(current_r_platform(), "source"))

default_cran_mirror <- function() {
  mirror <- getOption("repos")["CRAN"]
  if (is.null(mirror) || is.na(mirror) || mirror == "@CRAN@") {
    "https://cran.rstudio.com"
  } else {
    mirror
  }
}

current_r_version <- function() {
  as.character(getRversion())
}

read.dcf.gz <- function(x) {
  con <- gzfile(x, open = "r")
  on.exit(close(con))
  read.dcf(con)
}

lapply_with_names <- function(X, FUN, ...) {
  structure(
    lapply(X, FUN, ...),
    names = names(X) %||% (if (is.character(X)) X)
  )
}

update_named_vector <- function(old, new) {
  assertthat::assert_that(all_named(old), all_named(new))
  comm <- intersect(names(old), names(new))
  add <- setdiff(names(new), names(old))
  old[comm] <- new[comm]
  old <- c(old, new[add])
  old
}

make_dl_status <- function(status, url, target, bytes, error = NULL) {
  obj <- list(
    status = status,
    url = url,
    target = target,
    bytes = NA_real_,
    error = NULL
  )

  if (status == "Got") {
    obj$bytes <- as.double(bytes)

  } else if (status == "Failed") {
    obj$error <- error

  } else if (status == "Had") {
    obj$bytes <- as.double(bytes)
  }

  obj
}

write_bin_atomic <- function(object, file) {
  tmp <- paste0(file, ".tmp")
  on.exit(try(unlink(tmp), silent = TRUE))
  writeBin(object, tmp)
  file.rename(tmp, file)
}

save_rds_atomic <- function(object, file, ...) {
  tmp <- paste(file, ".tmp")
  on.exit(try(unlink(tmp), silent = TRUE))
  saveRDS(object, tmp, ...)
  file.rename(tmp, file)
}

comma_wrap <- function(x, indent = 2, exdent = indent, sep = ", ") {
  w <- strwrap(paste(x, collapse = sep), indent = indent, exdent = exdent)
  paste(w, collapse = "\n")
}

make_error <- function(message, class = character(), call = NULL, ...) {
  structure(
    c(list(message = message, call = call), list(...)),
    class = c(class, "error", "condition")
  )
}

add_class <- function(x, cl) {
  class(x) <- c(cl, class(x))
  x
}

is_na_scalar <- function(x) {
  length(x) == 1 && is.na(x)
}

omit_cols <- function(df, omit) {
  if (!length(omit)) {
    df
  } else {
    df[ , setdiff(names(df), omit), drop = FALSE]
  }
}

get_all_package_dirs <- function(platforms, rversions) {
  minors <- unique(get_minor_r_version(rversions))
  res <- lapply(platforms, function(pl) {
    if (pl == "source") {
      cbind("source", "*", "src/contrib")

    } else if (pl == "windows") {
      cbind("windows", minors, paste0("bin/windows/contrib/", minors))

    } else if (pl == "macos") {
      res1 <- lapply(minors, function(v) {
        if (package_version(v) <= "2.15") {
          cbind("macos", v, paste0("bin/macosx/leopard/contrib/", v))
        } else if (package_version(v) == "3.0") {
          cbind("macos", v, paste0("bin/macosx/contrib/", v))
        } else if (package_version(v) <= "3.2") {
          cbind("macos", v, paste0(c("bin/macosx/contrib/",
                                     "bin/macosx/mavericks/contrib/"), v))
        } else if (package_version(v) == "3.3") {
          cbind("macos", v, paste0("bin/macosx/mavericks/contrib/", v))
        } else {
          cbind("macos", v, paste0("bin/macosx/el-capitan/contrib/", v))
        }
      })
      do.call(rbind, res1)
    }
  })

  mat <- do.call(rbind, res)
  colnames(mat) <- c("platform", "rversion", "contriburl")
  res <- tibble::as_tibble(mat)
  res$prefix <- paste0(
    "/",
    ifelse(res$rversion == "*", "*", paste0("R-", res$rversion)),
    "/", res$platform, "/"
  )

  res
}

same_sha <- function(s1, s2) {
  assertthat::assert_that(is_string(s1), is_string(s2))
  len <- min(nchar(s1), nchar(s2))
  substr(s1, 1, len) == substr(s2, 1, len)
}

format_iso_8601 <- function (date) {
  format(as.POSIXlt(date, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
}

format_line <- function(txt, sep = "\n") {
  txt2 <- vcapply(txt, glue::glue_data, .x = parent.frame())
  paste(paste0(txt2, sep), collapse = "")
}

read_lines <- function(con, ...) {
  if (is.character(con)) {
    con <- file(con)
    on.exit(close(con))
  }
  readLines(con, ...)
}

all_ok <- function(x) {
  if (all(vcapply(x, "[[", "status") == "OK")) "OK" else "FAILED"
}

isFALSE <- function(x) {
  identical(x, FALSE)
}

file.size <- function(x) {
  file.info(x)$size
}

zip_lists <- function(...) {
  mapply(list, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

zip_vecs <- function(...) {
  mapply(c, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

lapply_rows <-  function(df, fun, ...) {
  lapply(seq_len(nrow(df)), function(i) fun(df[i,], ...))
}

`%||%` <- function(l, r) if (is.null(l)) r else l

add_attr <- function(x, attr, value) {
  attr(x, attr) <- value
  x
}

detect_download_cache_dir <- function() {
  tempfile()
}

rbind_expand <- function(..., .list = list()) {
  data <- c(list(...), .list)
  cols <- unique(unlist(lapply(data, function(x) colnames(x))))
  for (i in seq_along(data)) {
    miss_cols <- setdiff(cols, colnames(data[[i]]))
    if (length(miss_cols)) {
      na_df <- tibble::as_tibble(structure(
        replicate(
          length(miss_cols),
          if (nrow(data[[i]])) NA else logical(),
          simplify = FALSE),
        names = miss_cols))
      data[[i]] <- tibble::as_tibble(cbind(data[[i]], na_df))
    }
  }

  do.call(rbind, data)
}

version_satisfies <- function(ver, op, cond) {
  ver <- package_version(ver)
  switch(
    op,
    "<"  = ver <  cond,
    "<=" = ver <= cond,
    "==" = ver == cond,
    ">=" = ver >= cond,
    ">"  = ver >  cond,
    "!=" = ver != cond
  )
}
