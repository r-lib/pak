
pak_repo <- function() {
  "https://r-lib.github.io/p/pak/dev/"
}

#' Update pak itself
#'
#' Use this function to update the released or development version of pak.
#'
#' @param force Whether to force an update, even if no newer version is
#'   available.
#'
#' @return Nothing.
#'
#' @export

pak_update <- function(force = FALSE) {
  if (is.null(pkg_data$pak_version)) {
    stop(
      "Cannot find pak version data, are your trying to update a package ",
      "loaded with `load_all()`?"
    )
  }

  repo <- pak_repo()

  os <- get_os()
  if (os %in% c("win", "mac")) {
    av <- utils::available.packages(
      repos = repo,
      type = "binary",
      fields = c("Built", "File")
    )
  } else {
    av <- utils::available.packages(
      repos = repo,
      type = "source",
      filters = list(
        add = TRUE,
        "R_version",
        "OS_type",
        "subarch",
        av_filter,
        "duplicates"
      ),
      fields = c("Built", "File")
    )
  }

  if (nrow(av) == 0) {
    stop("Cannot find a pak binary for this platform. :(")
  }

  upd <- should_update_to(av)
  if (!upd && !force) {
    message("\nCurrent version is the latest, no need to update.")
    return(invisible())
  }

  message("\nUpdating to version ", av[1, "Version"], "\n")

  url <- paste0(av[1, "Repository"], "/", av[1, "File"])
  tgt <- file.path(tempdir(), av[1, "File"])
  utils::download.file(url, tgt)

  # Otherwise the subprocess might be locking some DLLs
  try(pkg_data$remote$kill(), silent = TRUE)

  lib <- dirname(getNamespaceInfo("pak", "path"))
  utils::install.packages(tgt, repos = NULL, type = "source", lib = lib)

  attached <- "package:pak" %in% search()
  message("\nReloading pak.")
  eapply(asNamespace("pak"), force, all.names = TRUE)
  unloadNamespace("pak")
  loadNamespace("pak")
  if (attached) library(pak)

  invisible()
}

av_filter <- function(av) {
  if (! "File" %in% colnames(av)) return(av)
  file <- sub("[.]tar[.]gz$", "", av[, "File"])
  pcs <- strsplit(file, "_", fixed = TRUE)
  arch <- vcapply(pcs, function(x) paste(x[-(1:3)], collapse = "_"))
  curr <- R.Version()$platform
  mtch <- vlapply(arch, platform_match, curr)
  av[mtch, , drop = FALSE]
}

should_update_to <- function(av) {
  dsc <- utils::packageDescription("pak")
  if (package_version(dsc$Version) < av[1, "Version"]) return(TRUE)
  blt_cur <- get_built_date(dsc$Built)
  blt_new <- get_built_date(av[1, "Built"])
  if (blt_cur < blt_new) return(TRUE)
  FALSE
}

get_built_date <- function(x) {
  # We can compare these dates as strings, so no need to parse
  strsplit(x, "[ ]*;[ ]*")[[1]][3]
}
