parse_remote_deps <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, type_deps_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "deps"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i, ])
  )
}

resolve_remote_deps <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  in_pkg <- is_package_root(remote$path)
  if (in_pkg) {
    ret <- resolve_remote_local(
      remote,
      direct,
      config,
      cache,
      dependencies,
      ...
    )
  } else {
    ret <- resolve_remote_local_autodeps(
      remote,
      direct,
      config,
      cache,
      dependencies,
      ...
    )
  }

  # We need to do some extra work for the case when a dependency
  # depends on the ref itself. E.g. when in pak::local_install_dev_deps()
  # a soft dependency depends on the local package.
  # - Because of this, we add a "-deps" postfix to the package name
  #   of the deps:: ref, so it not taken as the solution for the package
  #   by the solver.
  # - We also add a local:: ref for the package, because maybe the local
  #   package is not available from CRAN, or the upgrade policy is used
  #   and the local version is more recent. (This is typically the case
  #   while working on a package, and also on the CI.)

  # Workaround to be able to put this in a data frame
  ret$metadata <- list(ret$metadata)

  # Need to pass this to te new result
  unknown <- ret$unknown_deps
  ret$unknown_deps <- NULL

  # Create the local:: ref and update it properly
  local <- ret
  local$type <- "local"
  local$ref <- sub("^deps::", "local::", local$ref)
  local$direct <- FALSE
  local$remote[[1]] <- parse_pkg_ref(remote$ref)

  # Now make sure deps:: is not used by the solver.
  # The installer will treat deps:: specially and just ignore it, anyway.
  ret$package <- paste0(ret$package, "-deps")
  ret$sources <- list(character())

  # rbind.data.frame is not good with the list columns, TODO
  ret2 <- rbind(as_data_frame(ret), as_data_frame(local))
  attr(ret2, "unknown_deps") <- unknown

  ret2
}

resolve_remote_local_autodeps <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  proc <- cli::cli_process_start(
    "Scanning dependencies in {.path {remote$path}}"
  )
  deps <- scan_deps(remote$path, root = remote$path)
  cli::cli_process_done(proc)
  cli::cli_verbatim(paste(c(format(deps), ""), collapse = "\n"))
  cli::cli_verbatim(" ")
  tmpdesc <- tempfile()
  on.exit(unlink(tmpdesc), add = TRUE)
  dsc <- desc::desc("!new")
  hard <- deps$package[deps$type == "prod"]
  soft <- deps$package[deps$type != "prod"]
  dsc$set(
    Package = "localprojectautoscan",
    Version = "1.0.0",
    Title = "Local Project",
    License = "Unknown"
  )
  for (p in hard) dsc$set_dep(p, type = "Depends")
  for (s in soft) dsc$set_dep(p, type = "Suggests")
  dsc$write(tmpdesc)
  resolve_from_description(
    tmpdesc,
    paste0("file://", normalizePath(tmpdesc)),
    remote,
    direct,
    config,
    cache,
    dependencies[[2 - direct]]
  )
}

download_remote_deps <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  ## Nothing to do here
  "Had"
}

satisfy_remote_deps <- function(resolution, candidate, config, ...) {
  ## TODO: we can probably do better than this
  FALSE
}

installedok_remote_deps <- function(installed, solution, config, ...) {
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
