
## ------------------------------------------------------------------------
## API

#' @importFrom stats na.omit

parse_remote_cran <- function(specs, ...) {

  parsed_specs <- re_match(specs, standard_rx("cran"))

  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  eq <- parsed_specs$atleast == "" & parsed_specs$version != ""
  parsed_specs$atleast[eq] <- "=="
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
    vcapply(remote, "[[", "version")                                        # nocov
  }

  if (all(versions %in% c("", "current"))) {
    type_cran_resolve_current(remote, direct, config, cache, dependencies)
  } else {
    type_cran_resolve_version(remote, direct, config, cache, dependencies) # nocov
  }
}

download_remote_cran <- function(resolution, target, target_tree, config,
                                 cache, which, on_progress) {

  # if we don't have a shain the metadata, then we need to ping.
  # otherwise we can use the cache.
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
    repotype <- candidate$extra[[1]]$repotype
    if (! identical(repotype, "cran")) {
      return(structure(FALSE, reason = "Installed package not from CRAN"))
    }
  }

  ## 3. package names must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 4. installed package must not be older for direct refs
  if (resolution$direct) {
    if (candidate$type == "installed" &&
        package_version(resolution$version) > candidate$version) {
      return(structure(FALSE, reason = "Direct ref needs update"))
    }
  }

  ## 5. version requirements must be satisfied. Otherwise good.
  if (resolution$remote[[1]]$version == "") return(TRUE)

  if (!version_satisfies(
         candidate$version,
         resolution$remote[[1]]$atleast,
         resolution$remote[[1]]$version)) {
    return(structure(FALSE, reason = "Insufficient version"))
  }

  TRUE
}

installedok_remote_cran <- function(installed, solution, config, ...) {

  if (solution$platform != "source") {
    # Binary packages are simple. We need to match `$built` to make sure
    # that we are installing the same build.
    identical(installed$package, solution$package) &&
      identical(installed$version, solution$version) &&
      installedok_remote_standard_platform(installed, solution, config, ...) &&
      identical(installed[["built"]], solution[["built"]])

  } else {
    # Source packages are different, because only the installed one has a
    # 'Built' field. So we require `Repository == "CRAN"` for these.
    identical(installed$package, solution$package) &&
      identical(installed$version, solution$version) &&
      identical(installed[["repository"]], "CRAN")
  }
}

## ----------------------------------------------------------------------
## Internal functions

type_cran_resolve_current <- function(remote, direct, config, cache,
                                      dependencies) {
  resolve_from_metadata(remote, direct, config, cache, dependencies)
}

type_cran_resolve_version <- function(remote, direct, config,
                                      cache, dependencies) {

  remote; direct; config; cache; dependencies

  # remote can be a list
  if (!inherits(remote, "remote_ref")) {
    return(async_map(
      remote,
      type_cran_resolve_version,
      direct = direct,
      config = config,
      cache = cache,
      dependencies = dependencies
    )$then(function(reslist) res_list_to_df(reslist)))
  }

  if (remote$atleast == ">=") {
    throw(pkg_error(
      "Version ranges are not implemented yet."
    ))
  }

  url_remote <- remote
  url_remote$url <- paste0(
    config$get("cran-mirror")[[1]],
    "/src/contrib/Archive/", remote$package, "/",
    remote$package, "_", remote$version, ".tar.gz"
  )
  url_remote$hash <- url_hash(paste0(remote$package, "=url::", remote$url))

  resolve_remote_url(url_remote, direct, config, cache, dependencies)$
    then(function(res) {
      # Also resolve as the main package. This should not be needed,
      # but we do it in case the package file is both in the main repo
      # and Archive, or in case the binary version matches the requested
      # one, which is typical shortly after a new release.
      res$unknown_deps <- c(res$unknown_deps, remote$package)

      # Metadata for standard::, not url::
      res$metadata[["RemoteRef"]] <- remote$package
      res$metadata[["RemoteRepos"]] <- config$get("cran-mirror")[[1]]
      res$metadata[["RemotePkgPlatform"]] <- "source"
      res$metadata[["RemoteSha"]] <- remote$version
      res
    })$
    catch(error = function(err) {
      # if it is not in Archive, try the main package.
      # This will fail if not the right vesion
      resolve_from_metadata(remote, direct, config, cache, dependencies)
    })
}
