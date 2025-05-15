parse_remote_param <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, param_rx())
  parsed_specs$ref <- "*"
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "param"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i, ])
  )
}

resolve_remote_param <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  ## Handled specially by the resolver
  NULL
}

download_remote_param <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  ## this is never actually called
  stop("Internal error")
}

satisfy_remote_param <- function(resolution, candidate, config, ...) {
  ## this is never actually called
  stop("Internal error")
}

installedok_remote_param <- function(installed, solution, config, ...) {
  ## this is never actually called
  stop("Internal error")
}
