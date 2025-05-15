### -----------------------------------------------------------------------
### API

parse_remote_local <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, local_rx())
  parsed_specs$ref <- paste0(
    ifelse(parsed_specs$package == "", "", paste0(parsed_specs$package, "=")),
    "local::",
    parsed_specs$path
  )
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs$package[parsed_specs$package == ""] <- NA_character_
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "local"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i, ])
  )
}

resolve_remote_local <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  sources <- paste0("file://", normalizePath(remote$path, mustWork = FALSE))
  resolve_from_description(
    remote$path,
    sources,
    remote,
    direct,
    config,
    cache,
    dependencies[[2 - direct]]
  )
}

download_remote_local <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  source_file <- sub("^file://", "", resolution$sources[[1]])
  isdir <- file.info(source_file)$isdir
  if (is.na(isdir)) {
    throw(pkg_error(
      "Could not find local package file/directory at
       {.path {resolution$sources[[1]]}}"
    ))
  }

  if (isdir) {
    unlink(target_tree, recursive = TRUE)
    mkdirp(target_tree)
    if (!file.copy(source_file, target_tree, recursive = TRUE)) {
      # deleted after the resolution?
      throw(pkg_error(
        # nocov start
        "Could not find local package file/directory at
       {.path {resolution$sources[[1]]}}"
      )) # nocov end
    }
  } else {
    if (!file.copy(source_file, target, overwrite = TRUE)) {
      # deleted after the resolution?
      throw(pkg_error(
        # nocov start
        "Could not find local package file/directory at
       {.path {resolution$sources[[1]]}}"
      )) # nocov end
    }
  }

  "Got"
}

satisfy_remote_local <- function(resolution, candidate, config, ...) {
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  # package name was explicitly given, check for match
  req <- resolution$remote[[1]]$package
  if (!is.null(req) && !is.na(req)) {
    got <- candidate$package
    if (req != got) {
      return(structure(
        FALSE,
        reason = paste0("Requested package ", req, " but got ", got)
      ))
    }
  }

  if (candidate$type == "local") {
    if (candidate$remote[[1]]$path == resolution$remote[[1]]$path) return(TRUE)
    return(structure(FALSE, reason = "Paths differ"))
  }

  structure(FALSE, reason = "Package source mismatch")
}

installedok_remote_local <- function(installed, solution, config, ...) {
  ## TODO: we can probably do better than this
  FALSE
}
