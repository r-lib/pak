#' Dependency resolution
#'
#' Collect information about dependencies of R packages, recursively.
#'
#' [`pkg_deps`], [`pkg_download_proposal`] and [`pkg_installation_proposal`]
#' all resolve their dependencies recursively, to obtain information about
#' all packages needed for the specified [package references][pkg_refs].
#'
#' ## CRAN and Bioconductor packages
#'
#' Resolution currently start by downloading the CRAN and Bioconductor
#' metadata, if it is out of date. For CRAN, we also download additional
#' metadata, that includes file sizes, SHA hashes, system requirements,
#' and "built" (for binary packages) and "packaged" time stamps. The extra
#' meta information is updated daily currently, so for some packages it
#' might be incorrect or missing.
#'
#' ## GitHub packages
#'
#' For GitHub packages, we query their download URL to be able to
#' download the package later, and also download their `DESCRIPTION`
#' file, to learn about their dependencies.
#'
#' ## Local packages
#'
#' From local package files we extract the `DESCRIPTION` file, to learn
#' about their dependencies.
#'
#' ## The `remotes` field in `DESCRIPTION`
#'
#' We support the non-standard `Remotes` field in the package `DESCRIPTION`
#' file. This field may contain a list of package references for any of the
#' dependencies that are specified in one of the `Depends`, `Includes`,
#' `Suggests` or `Enhances` fields. The syntax is a comma separated list of
#' [package references][pkg_refs].
#'
#' ## The result
#'
#' The result of the resolution is a data frame with information about the
#' packages and their dependencies.
#'
#' ```{r child = {options(rx_downloads=NULL); "tools/doc/resolution-result.Rmd"}}
#' ```
#' `r { options(rx_downloads = NULL); doc_share_rmd("tools/doc/resolution-result.Rmd", "inst/docs/resolution-result.rds")}`
#'
#' ## Resolution failures
#'
#' The resolution process does not stop on error. Instead, failed
#' resolutions return and error object in the `error` column of the result
#' data frame.
#'
#' @name pkg_resolution
#' @aliases pkg_resolution_result
NULL

pkgplan_resolve <- function(self, private) {
  "!DEBUG pkgplan_resolve (sync)"
  synchronise(self$async_resolve())
}

pkgplan_async_resolve <- function(self, private) {
  "!DEBUG pkgplan_resolve (async)"
  ## We remove this, to avoid a discrepancy between them
  private$downloads <- NULL
  private$solution <- NULL
  private$system_packages <- NULL
  private$sysreqs <- NULL

  private$dirty <- TRUE
  private$resolution <- new_resolution(
    config = private$config,
    cache = private$cache,
    library = private$config$get("library"),
    remote_types = private$remote_types
  )

  private$resolution$push(direct = TRUE, .list = private$remotes)

  private$resolution$when_complete()$then(function(x) {
    private$dirty <- FALSE
    x
  })
}

pkgplan_get_resolution <- function(self, private) {
  if (is.null(private$resolution$result)) {
    throw(pkg_error(
      "No resolution yet.",
      i = "You need to call {.code $resolve()} first."
    ))
  }
  private$resolution$result
}

pkgplan__subset_resolution <- function(self, private, which) {
  if (is.null(private$resolution$result)) {
    throw(pkg_error(
      "No resolution yet.",
      i = "You need to call {.code $resolve()} first."
    ))
  }
  res <- private$resolution$result[which, ]
  attr(res, "metadata") <- attr(private$resolution$result, "metadata")
  res
}

new_resolution <- function(config, cache, library = NULL, remote_types = NULL) {
  resolution$new(config, cache, library, remote_types)
}

resolution <- R6::R6Class(
  "resolution",
  public = list(
    result = NULL,
    initialize = function(config, cache, library = NULL, remote_types = NULL)
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
    params = NULL,
    dependencies = NULL,
    metadata = NULL,
    system_packages = NULL,
    sysreqs = NULL,
    bar = NULL,

    delayed = list(),
    delayed_refs = character(),
    resolve_delayed = function(resolve)
      res__resolve_delayed(self, private, resolve),

    create_progress_bar = function() res__create_progress_bar(self, private),
    done_progress_bar = function() res__done_progress_bar(self, private),

    set_result = function(row_idx, value)
      res__set_result(self, private, row_idx, value),
    sysreqs_match = function() res__sysreqs_match(self, private),
    try_finish = function(resolve) res__try_finish(self, private, resolve)
  )
)

res_init <- function(self, private, config, cache, library, remote_types) {
  "!DEBUG resolution init"
  private$config <- config
  private$cache <- cache
  private$library <- library
  private$remote_types <- remote_types %||% default_remote_types()
  private$metadata <- list(resolution_start = Sys.time())
  private$dependencies <- as_pkg_dependencies(config$get("dependencies"))
  private$bar <- private$create_progress_bar()

  self$result <- res_make_empty_df()

  private$state <- data_frame(
    ref = character(),
    remote = list(),
    status = character(),
    direct = logical(),
    async_id = integer(),
    started_at = Sys.time()[FALSE]
  )

  private$deferred <- asNamespace("pkgcache")$deferred$new(
    type = "resolution_queue",
    parent_resolve = function(value, resolve) {
      "!DEBUG resolution done"
      # maybe a non-resolution task ended, e.g. sysreqs
      if (!"id" %in% names(value)) return(private$try_finish(resolve))
      id <- value$id
      value <- value$value
      wh <- which(id == private$state$async_id)
      private$state$status[wh] <- "OK"

      ## Rule out installed:: refs and packages with ?source param
      not_inst <- value$type != "installed"
      prms <- value[["params"]]
      if (!is.data.frame(value)) prms <- list(prms)
      want_source <- vlapply(prms, is_true_param, "source")
      want_reinst <- vlapply(prms, is_true_param, "reinstall")

      package <- value$package
      for (par in private$params) {
        if (is_true_param(par$params, "source")) {
          want_source[package %in% par$package | par$package == ""] <- TRUE
        }
        if (is_true_param(par$params, "reinstall")) {
          want_reinst[package %in% par$package | par$package == ""] <- TRUE
        }
      }

      npkgs <- value$package[not_inst & !want_source & !want_reinst]

      ## Installed already? Resolve that as well
      if (!is.null(private$library) && length(npkgs)) {
        ml <- file.exists(file.path(private$library, npkgs))
        rc <- file.exists(file.path(.Library, npkgs)) &
          npkgs %in% recommended_packages()
        npkgs <- npkgs[ml | rc]
        if (length(npkgs)) {
          lib <- normalizePath(
            private$library,
            winslash = "/",
            mustWork = FALSE
          )
          refs <- paste0("installed::", lib, "/", npkgs)
          refs <- setdiff(refs, private$state$ref)
          self$push(.list = parse_pkg_refs(refs))
        }
      }

      private$set_result(wh, value)
      private$try_finish(resolve)
    },

    parent_reject = function(value, resolve) {
      "!DEBUG resolution failed"
      id <- value$id
      wh <- which(id == private$state$async_id)
      private$state$status[wh] <- "FAILED"
      rec <- private$state[wh, ]
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
    }
  )

  # If sysreqs is supported on this platform, then
  # 1. look up system packages, and
  # 2. (try to) update sysreqs mapping
  sys_sup <- sysreqs_is_supported(private$config$get("sysreqs_platform"))
  sys_lookup <- private$config$get("sysreqs_lookup_system")
  if (sys_sup && sys_lookup) {
    private$system_packages <- NA # nocovif !is_linux()
    async_system_list_packages(private$config)$then(function(x) {
      # nocovif !is_linux()
      private$system_packages <- x
      NULL
    })$then(private$deferred) # nocovif !is_linux() # nocovif !is_linux()
  }
  if (sys_sup) {
    private$sysreqs <- NA
    sysreqs2_async_update_metadata(config = private$config)$then(function() {
      private$sysreqs <- TRUE
      NULL
    })$then(private$deferred)
  }
}

res_push <- function(self, private, ..., direct, .list = .list) {
  self
  private
  new <- c(list(...), .list)

  ## Set (new) parameters
  new_types <- vcapply(new, "[[", "type")
  params <- new[new_types == "param"]
  new <- new[new_types != "param"]

  if (length(params) + length(private$params)) {
    private$params <- c(private$params, params)
    update_params(self, private, private$params)
  }

  ## Drop the ones already resolving up front
  new_refs <- vcapply(new, "[[", "ref")
  keep <- !new_refs %in% c(private$state$ref, private$delayed_refs)
  new <- new[keep]
  new_refs <- new_refs[keep]

  ## Drop duplicates as well
  uni_refs <- !duplicated(new_refs)
  if (!all(uni_refs)) {
    new <- new[uni_refs]
    new_refs <- new_refs[uni_refs]
  }

  ## We do CRAN/BioC/standard in batches
  ## TODO: direct ones in one go as well
  batch_types <- setdiff(
    c("cran", "standard", "bioc", "installed"),
    private$remote_types
  )
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
    dx <- resolve_remote(
      n,
      direct,
      private$config,
      private$cache,
      private$dependencies,
      remote_types = private$remote_types
    )

    private$state <- rbind(
      private$state,
      data_frame(
        ref = n$ref,
        remote = list(n),
        status = NA_character_,
        direct = direct,
        async_id = dx$id,
        started_at = Sys.time()
      )
    )

    dx$dx$then(private$deferred)
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

  if (length(n)) {
    types <- vcapply(n, "[[", "type")
    utypes <- unique(types)
    for (t in utypes) {
      n2 <- n[types == t]

      dx <- resolve_remote(
        n2,
        direct = FALSE,
        private$config,
        private$cache,
        private$dependencies,
        remote_types = private$remote_types
      )

      private$state <- rbind(
        private$state,
        data_frame(
          ref = vcapply(n2, "[[", "ref"),
          remote = n2,
          status = NA_character_,
          direct = FALSE,
          async_id = dx$id,
          started_at = Sys.time()
        )
      )
      dx$dx$then(private$deferred)
    }
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

  if (is.data.frame(value)) {
    res__set_result_df(self, private, row_idx, value)
  } else {
    res__set_result_list(self, private, row_idx, value)
  }

  "!DEBUG resolution setting result, total: `nrow(self$result)`"
  if (length(unknown)) self$push(.list = parse_pkg_refs(unknown))
}

res__set_result_df <- function(self, private, row_idx, value) {
  # remove the ones that are already done
  done <- value$ref %in% self$result$ref
  value <- value[!done, ]

  # avoid removing the ones that are direct and already on the TODO list
  running <- intersect(value$ref, private$state$ref)
  avoid <-
    !value$direct[match(running, value$ref)] &
    private$state$direct[match(running, private$state$ref)]
  value <- value[!value$ref %in% running[avoid], ]

  if (nrow(value)) self$result <- res_add_df_entries(self$result, value)
}

res__set_result_list <- function(self, private, row_idx, value) {
  # already done?
  if (all(value$ref %in% self$result$ref)) return()
  if (is.data.frame(value)) {
    # if a data frame, then some might be done
    done <- value$ref %in% private$state$ref
    value <- value[!done, drop = FALSE]
  }
  self$result <- res_add_df_entries(self$result, value)
}

res__sysreqs_match <- function(self, private) {
  if ("sysreqs" %in% names(self$result)) {
    sys <- sysreqs2_match(self$result$sysreqs, config = private$config)
    if (!is.null(spkgs <- private$system_packages)) {
      spkgs <- spkgs[grepl("^.i$", spkgs$status), ] # nocovif !is_linux()
      allspkgs <- unique(unlist(c(spkgs$package, spkgs$provides))) # nocovif !is_linux()
      for (i in seq_along(sys)) {
        # nocovif !is_linux()
        elt <- sys[[i]] # nocovif !is_linux()
        for (j in seq_along(elt)) {
          # nocovif !is_linux()
          elt[[j]]$packages_missing <- setdiff(elt[[j]]$packages, allspkgs) # nocovif !is_linux()
        } # nocovif !is_linux()
        if (!is.null(elt)) sys[[i]] <- elt # nocovif !is_linux()
      }
    }
    self$result$sysreqs_packages <- sys
    platform <- private$config$get("sysreqs_platform")
    cmd_upd <- sysreqs2_command(platform, "update_command")
    cmd_inst <- sysreqs2_command(platform, "install_command")
    pre <- lapply(sys, function(x) unlist(lapply(x, "[[", "pre_install")))
    post <- lapply(sys, function(x) unlist(lapply(x, "[[", "post_install")))
    spkg <- lapply(sys, function(x) {
      unlist(lapply(x, function(xx) xx$packages_missing %||% xx$packages))
    })

    pre <- vcapply(pre, paste, collapse = ";")
    post <- vcapply(post, paste, collapse = ";")
    spkg <- vcapply(spkg, paste, collapse = " ")
    if (!is.na(cmd_upd)) {
      pre <- ifelse(spkg == "", pre, paste0(cmd_upd, ";", pre))
    }
    spkg <- ifelse(spkg == "", "", paste(cmd_inst, spkg))

    self$result$sysreqs_pre_install <- pre
    self$result$sysreqs_post_install <- post
    self$result$sysreqs_install <- spkg
  } else {
    self$result$sysreqs_packages <- list(NULL) # nocovif !is_linux()
  }
}

res__try_finish <- function(self, private, resolve) {
  "!DEBUG resolution trying to finish with `nrow(self$result)` results"
  if (length(private$delayed)) return(private$resolve_delayed(resolve))
  if (
    all(!is.na(private$state$status)) &&
      !identical(private$system_packages, NA) &&
      !identical(private$sysreqs, NA)
  ) {
    "!DEBUG resolution finished"
    update_params(self, private, private$params)
    update_dep_types(self, private)
    private$metadata$resolution_end <- Sys.time()
    self$result$cache_status <-
      calculate_cache_status(self$result, private$cache)
    attr(self$result, "metadata") <- private$metadata
    class(self$result) <- c("pkg_resolution_result", class(self$result))
    private$done_progress_bar()
    if (sysreqs_is_supported(private$config$get("sysreqs_platform"))) {
      private$sysreqs_match()
    }
    resolve(self$result)
  }
}

resolve_remote <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  remote_types = NULL
) {
  remote
  direct
  config
  cache
  dependencies
  remote_types

  remote_types <- c(default_remote_types(), remote_types)

  type <- remote$type %||% unique(vcapply(remote, "[[", "type"))
  if (length(type) != 1) {
    throw(pkg_error(
      "Invalid remote or remote list, multiple types: {.val {type}}.",
      i = msg_internal_error()
    ))
  }

  resolve <- remote_types[[type]]$resolve
  if (is.null(resolve)) {
    throw(pkg_error(
      "Don't know how to resolve remote type {.val {type}}.",
      i = msg_internal_error()
    ))
  }

  id <- get_id()
  dx <- async(resolve)(
    remote,
    direct = direct,
    config = config,
    cache = cache,
    dependencies = dependencies
  )$then(function(value) list(value = value, id = id))$catch(error = function(
    err
  ) {
    err$id <- id
    err$call <- NULL
    throw(
      pkg_error(
        "{pak_or_pkgdepends()} resolution error for {.pkg {remote$ref}}.",
        .data = list(id = id)
      ),
      parent = err
    )
  })

  list(dx = dx, id = id)
}

update_params <- function(self, private, params) {
  for (par in params) {
    if (par$package %in% c("", "*") && length(self$result$params)) {
      self$result$params <- lapply(self$result$params, function(p) {
        p[names(par$params)] <- par$params
        p
      })
      self$result$remote <- lapply(self$result$remote, function(rem) {
        if (is.list(rem)) rem$params[names(par$params)] <- par$params
        rem
      })
    } else {
      sel <- self$result$package == par$package
      if (any(sel)) {
        self$result$params[sel] <- lapply(self$result$params[sel], function(p) {
          p[names(par$params)] <- par$params
          p
        })
        self$result$remote[sel] <- lapply(
          self$result$remote[sel],
          function(rem) {
            if (is.list(rem)) rem$params[names(par$params)] <- par$params
            rem
          }
        )
      }
    }
  }
}

update_dep_types <- function(self, private) {
  # these are the default
  def <- vlapply(
    self$result$dep_types,
    function(x) length(x) == 1 && x == "default"
  )
  dep_types <- self$result$dep_types

  directdef <- self$result$direct[def]
  directpkgdef <- self$result$directpkg[def]

  # dependencies get the default
  dep_types[def][!directpkgdef] <- list(private$dependencies[["indirect"]])

  # direct refs that use the default
  dep_types[def][directdef] <- list(private$dependencies[["direct"]])

  # direct packages that are not direct refs will get their default from
  # their direct ref(s)
  dpkgs <- unique(self$result$package[def][directpkgdef & !directdef])
  for (pkg in dpkgs) {
    myrefs <- self$result$package == pkg & self$result$direct
    myrefdeps <- unique(unlist(dep_types[myrefs]))
    mypkgs <- self$result$package[def] == pkg
    dep_types[def][mypkgs & directpkgdef & !directdef] <- list(myrefdeps)
  }

  self$result$dep_types <- dep_types
}

resolve_from_description <- function(
  path,
  sources,
  remote,
  direct,
  config,
  cache,
  dependencies
) {
  dsc <- desc::desc(file = path)
  deps <- resolve_ref_deps(
    dsc$get_deps(),
    dsc$get("Remotes")[[1]],
    dsc$get(extra_config_fields(dsc$fields()))
  )

  rversion <- tryCatch(
    get_minor_r_version(dsc$get_built()$R),
    error = function(e) "*"
  )

  platform <- if (dsc$has_fields("Built")) {
    built <- dsc$get_built()
    archs <- gsub(" ", "", dsc$get("Archs"))
    if (built$OStype == "windows") {
      if (is.na(archs) || archs == "i386,x64" || archs == "x64,i386") {
        "i386+x86_64-w64-mingw32"
      } else {
        built$Platform
      }
    } else {
      built$Platform
    }
  } else {
    "source"
  }

  nc <- dsc$get_field("NeedsCompilation", NA)
  if (!is.na(nc)) nc <- tolower(nc) %in% c("true", "yes")

  unknown <- deps$ref[deps$type %in% dependencies]

  meta <- c(
    RemotePkgRef = remote$ref,
    RemoteType = remote$type,
    RemoteSha = NULL, # TODO
    RemoteUrl = NULL, # TODO
    RemoteUsername = NULL, # TODO
    RemoteRepo = NULL, # TODO
    RemoteBranch = NULL # TODO
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
    metadata = meta,
    params = list(remote$params),
    sysreqs = dsc$get_field("SystemRequirements", "")
  )
}

drop_config_needs <- function(x) {
  x[!grepl("^config/needs", x, ignore.case = TRUE)]
}

# TODO: Parse remotes and Config/Needs/* fields

resolve_from_metadata <- function(
  remotes,
  direct,
  config,
  cache,
  dependencies
) {
  remotes
  direct
  config
  cache
  dependencies
  if (is.list(dependencies)) {
    dependencies$direct <- drop_config_needs(dependencies$direct)
    dependencies$indirect <- drop_config_needs(dependencies$indirect)
  }

  ## Single remote, or a list of remotes
  if ("ref" %in% names(remotes)) {
    packages <- remotes$package
    refs <- remotes$ref
    types <- remotes$type
    params <- list(remotes$params)
  } else {
    packages <- vcapply(remotes, "[[", "package")
    refs <- vcapply(remotes, "[[", "ref")
    types <- vcapply(remotes, "[[", "type")
    params <- lapply(remotes, "[[", "params")
  }

  if (!direct) dependencies <- dependencies$indirect
  "!DEBUG resolving `length(refs)` batch resolution"
  cache$metadata$async_deps(packages, dependencies = dependencies)$then(
    function(data) {
      cols <- c(
        "ref",
        "type",
        "status",
        "package",
        "version",
        "license",
        "needscompilation",
        "priority",
        "md5sum",
        "platform",
        "rversion",
        "repodir",
        "target",
        "deps",
        "sources",
        "mirror",
        "filesize",
        "sha256",
        "sysreqs",
        "os_type"
      )

      cols <- intersect(names(data), cols)

      res <- data[cols]
      res$built <- data[["built"]] %||% rep(NA_character_, nrow(res))
      idx <- match(res$package, packages)
      res$ref[!is.na(idx)] <- na.omit(refs[idx])
      res$repotype <- res$type
      res$type[] <- "standard"
      res$type[!is.na(idx)] <- na.omit(types[idx])
      res$needscompilation <-
        tolower(res$needscompilation) %in% c("yes", "true")
      res$direct <- direct & res$ref %in% refs
      res$params <- replicate(nrow(res), character())
      res$params[!is.na(idx)] <- params[na.omit(idx)]

      res$metadata <- get_standard_metadata(res)

      # Drop binaries if source package was requested
      want_source <- vlapply(res$params, is_true_param, "source")
      todrop <- res$platform != "source" & want_source
      if (any(todrop)) res <- res[!todrop, ]

      if (length(bad <- attr(data, "unknown"))) {
        idx <- match(bad, packages)
        bad[!is.na(idx)] <- na.omit(refs[idx])
        failed <- make_failed_resolution(
          bad,
          ifelse(!is.na(idx), types[idx], "standard"),
          direct & bad %in% refs
        )
        failed <- res_add_defaults(failed)
        ## This is added later, we don't want to add it here, because
        ## it messes up the dep_types of the successful resolutions
        failed <- failed[names(failed) != "dep_types"]
        res <- rbind_expand(res, failed)
      }

      res
    }
  )
}

get_standard_metadata <- function(tab) {
  meta <- replicate(nrow(tab), character(), simplify = FALSE)
  for (i in seq_len(nrow(tab))) {
    meta[[i]] <-
      c(
        RemoteType = tab$type[i],
        RemotePkgRef = tab$ref[i],
        RemoteRef = tab$ref[i],
        RemoteRepos = tab$mirror[i],
        RemotePkgPlatform = tab$platform[i],
        RemoteSha = tab$version[i]
      )
  }
  meta
}

make_failed_resolution <- function(refs, type, direct) {
  rstr <- paste(refs, collapse = ", ")
  err <- structure(
    list(message = paste0("Can't find package called ", rstr, ".")),
    class = c("error", "condition")
  )
  data_frame(
    ref = refs,
    type = type,
    package = sub("^[a-z]+::", "", refs),
    version = NA_character_,
    sources = replicate(length(refs), NA_character_, simplify = FALSE),
    direct = direct,
    status = "FAILED",
    remote = parse_pkg_refs(refs),
    error = replicate(length(refs), err, simplify = FALSE)
  )
}

#' @export

`[.pkg_resolution_result` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pkg_resolution_result")
  NextMethod("[")
}
