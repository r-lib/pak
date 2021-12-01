
pak_repo <- function() {
  base <- "https://r-lib.github.io/p/pak/"
  if (tolower(Sys.getenv("PAK_DEVEL")) == "true") {
    paste0(base, "devel")
  } else {
    paste0(base, "stable")
  }
}

#' Update pak itself
#'
#' Use this function to update the released or development version of pak.
#'
#' @param force Whether to force an update, even if no newer version is
#'   available.
#' @param type Package type. Like the `type` argument of
#'   [utils::install.packages()]. You can set this to `mac.binary` or
#'   `mac.binary.big-sur-arm64` if you have a non-CRAN R build, but want
#'   to install a binary pak package.
#'
#' @return Nothing.
#'
#' @export

pak_update <- function(
  force = FALSE,
  type = getOption("pkgType")) {

  stopifnot(is_flag(force), is_string(type))

  repo <- pak_repo()

  if (!is.null(.getNamespace("pak")$.__DEVTOOLS__)) {
    warning(
      "`load_all()`-d pak package, updating in default library",
      immediate. = TRUE
    )
    lib <- .libPaths()[1]
  } else {
    lib <- dirname(getNamespaceInfo("pak", "path"))
  }

  os <- get_os()
  arch <- Sys.info()["machine"]
  if (os == "win") {
    av <- utils::available.packages(
      repos = repo,
      type = "win.binary",
      fields = c("Built", "File")
    )
  } else if (os == "mac" && arch == "x86_64") {
    av <- utils::available.packages(
      repos = repo,
      type = "mac.binary",
      fields = c("Built", "File")
    )
  } else if (os == "mac" && arch == "arm64") {
    av <- utils::available.packages(
      repos = repo,
      type = "mac.binary.big-sur-arm64",
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

  utils::install.packages(tgt, repos = NULL, type = "source", lib = lib)

  attached <- "package:pak" %in% search()
  message("\nReloading pak.")
  eapply(asNamespace("pak"), force, all.names = TRUE)
  unloadNamespace("pak")
  loadNamespace("pak")
  if (attached) library(pak)

  # Try to use it to see if it was successful
  tryCatch(
    suppressWarnings(tools::Rd_db(package = "pak")),
    error = function(err) {
      message("\nFailed to reload pak. Please restart your R session.")
    }
  )

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
  # check if the right platform was installed
  current <- R.Version()$platform
  if (!platform_match(pak_sitrep_data$platform, current)) {
    message("\npak platform mismatch, trying to update to fix this...")
    return(TRUE)
  }

  # otherwise use version number first
  dsc <- utils::packageDescription("pak")
  if (package_version(dsc$Version) < av[1, "Version"]) return(TRUE)

  # or the build date
  blt_cur <- get_built_date(dsc$Built)
  blt_new <- get_built_date(av[1, "Built"])
  if (is.na(blt_cur) || blt_cur < blt_new) return(TRUE)
  FALSE
}

get_built_date <- function(x) {
  if (!is_string(x)) return(NA_character_)
  # We can compare these dates as strings, so no need to parse
  strsplit(x, "[ ]*;[ ]*")[[1]][3]
}
