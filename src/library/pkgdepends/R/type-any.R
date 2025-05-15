parse_remote_any <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, standard_rx("any"))
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "any"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i, ])
  )
}

resolve_remote_any <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  resolve_remote_standard(
    remote,
    direct = FALSE,
    config,
    cache,
    dependencies,
    ...
  )$then(function(res) {
    refs <- if ("type" %in% names(remote)) {
      remote$ref
    } else {
      vcapply(remote, "[[", "ref") # nocov
    }
    res$direct[res$ref %in% refs] <- TRUE
    res$dep_types <- list(dependencies[[2]])
    res
  })
}

download_remote_any <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  download_remote_standard(
    resolution,
    target,
    target_tree,
    config,
    cache,
    which,
    on_progress
  )
}

satisfy_remote_any <- function(resolution, candidate, config, ...) {
  # package name must be the same
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ")) # nocov
  }

  # otherwise we are good
  TRUE
}

installedok_remote_any <- function(installed, solution, config, ...) {
  # We still need the one we planned for, because the other one might
  # have different dependencies.
  installedok_remote_cran(installed, solution, config, ...)
}
