sort_by_name <- function(x) {
  x[order(names(x))]
}

default_library <- function(config) {
  lp <- .libPaths()
  if (config$get("ignore_dev_library")) {
    lp <- lp[basename(lp) != "__dev_lib__"]
  }
  lp[1L]
}

pak_config <- sort_by_name(list(
  # -----------------------------------------------------------------------
  ignore_dev_library = list(
    type = "flag",
    default = TRUE,
    docs = "Whether to ignore library directories called `__dev_lib__`."
  ),

  # -----------------------------------------------------------------------
  library = list(
    type = "character_or_null",
    default = function(config) default_library(config),
    docs = "Package library to install packages to. It is also used for
       already installed packages when considering dependencies in
       [dependency lookup][pkg_deps] or
       [package installation][pkg_installation_proposal]. Defaults to the
       first path in [.libPaths()].",
    docs_pak = "Package library to install packages to. It is also used for
       already installed packages when considering dependencies."
  )
))

current_config <- function() {
  conf <- config$new("pkg")

  map_named(pak_config, function(name, entry) {
    type <- entry$type %||% "character"
    conf$add(
      name,
      type,
      default = entry$default,
      check = entry$check %||% type,
      env_decode = entry$env_decode %||% type
    )
  })

  conf$lock()
  conf
}
