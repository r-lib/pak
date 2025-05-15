satisfies_remote <- function(
  resolution,
  candidate,
  config,
  remote_types = NULL,
  ...
) {
  remote_types <- c(default_remote_types(), remote_types)
  sat <- remote_types[[resolution$type]]$satisfy
  if (is.null(sat)) return(resolution$ref == candidate$ref)

  sat(resolution, candidate, config, ...)
}

installedok_remote <- function(
  installed,
  solution,
  config,
  remote_types = NULL,
  ...
) {
  remote_types <- c(default_remote_types(), remote_types)
  ok <- remote_types[[solution$type]]$installedok
  if (is.null(ok)) return(FALSE)
  ok(installed, solution, config, ...)
}
