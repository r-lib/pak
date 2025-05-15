## ------------------------------------------------------------------------
## API

parse_remote_bioc <- function(specs, config, ...) {
  ## BioC is the same as CRAN, except for cran:: -> bioc::
  parsed_specs <- re_match(specs, standard_rx("bioc"))
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  eq <- parsed_specs$atleast == "" & parsed_specs$version != ""
  parsed_specs$atleast[eq] <- "=="
  parsed_specs$type <- "bioc"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i, ])
  )
}

resolve_remote_bioc <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  progress_bar,
  ...
) {
  force(remote)
  force(direct)
  force(dependencies)
  versions <- if ("type" %in% names(remote)) {
    remote$version
  } else {
    vcapply(remote, "[[", "version") # nocov
  }

  if (all(versions %in% c("", "current"))) {
    resolve_from_metadata(remote, direct, config, cache, dependencies)
  } else {
    type_cran_resolve_version(remote, direct, config, cache, dependencies) # nocov
  }
}

download_remote_bioc <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  # we need to check if there is a new binary build. for source packages
  # we can always use the cache
  download_ping_if_not_source(resolution, target, config, cache, on_progress)
}

satisfy_remote_bioc <- function(resolution, candidate, config, ...) {
  ## 1. candidate must be a bioc, standard or installed ref
  if (!candidate$type %in% c("bioc", "standard", "installed")) {
    return(structure(
      FALSE,
      reason = "Type must be 'bioc', 'standard' or 'installed'"
    ))
  }

  ## 2. installed refs must be from bioc
  if (candidate$type == "installed") {
    repotype <- candidate$extra[[1]]$repotype
    if (!identical(repotype, "bioc")) {
      return(structure(FALSE, reason = "Installed package not from BioC"))
    }
  }

  ## 3. package names must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 4. installed package must not be older for direct refs
  if (resolution$direct) {
    if (
      candidate$type == "installed" &&
        package_version(resolution$version) > candidate$version
    ) {
      return(structure(FALSE, reason = "Direct ref needs update"))
    }
  }

  ## 5. version requirements must be satisfied. Otherwise good.
  if (resolution$remote[[1]]$version == "") {
    return(TRUE)
  }

  if (
    !version_satisfies(
      candidate$version,
      resolution$remote[[1]]$atleast,
      resolution$remote[[1]]$version
    )
  ) {
    return(structure(FALSE, reason = "Insufficient version"))
  }

  TRUE
}

installedok_remote_bioc <- function(installed, solution, config, ...) {
  if (solution$platform != "source") {
    # Binary packages are simple. We need to match `$build` to make sure
    # that we are installing the same build.
    identical(installed$package, solution$package) &&
      identical(installed$version, solution$version) &&
      installedok_remote_standard_platform(installed, solution, config, ...) &&
      identical(installed[["built"]], solution[["built"]])
  } else {
    # Source packages are different, because only the installed one has a
    # 'Built' field.
    identical(installed$package, solution$package) &&
      identical(installed$version, solution$version) &&
      identical(installed[["repotype"]], "bioc")
  }
}
