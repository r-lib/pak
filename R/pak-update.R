
detect_platform <- function() {
  me <- list(
    os = R.Version()$os,
    arch = R.Version()$arch,
    rver = get_minor_r_version(getRversion())
  )

  if (me$os %in% c("linux-dietlibc", "linux-gnu", "linux-musl",
                   "linux-uclibc", "linux-unknown")) {
    me$os <- "linux"
  }
  me
}

pak_stream <- function(stream) {
  if (stream == "auto") {
    version <- unclass(package_version(utils::packageVersion("pak")))[[1]]
    stream <- if (length(version) >= 4 && version[4] == 9999) {
      "rc"
    } else if (length(version) >= 4 && version[4] >= 9000) {
      "devel"
    } else {
      "stable"
    }
  }
  stream
}

pak_repo <- function(stream = "auto") {
  stream <- pak_stream(stream)
  paste0("https://r-lib.github.io/p/pak/", stream, "/")
}

pak_repo_metadata <- function(repo = NULL, stream = "auto") {
  repo <- repo %||% pak_repo(stream)
  url <- paste0(repo, "metadata.json")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  df <- json$parse_file(tmp)
  meta <- do.call(rbind, lapply(df, as.data.frame, stringsAsFactors = FALSE))
  rver <- sub("R ", "", sapply(strsplit(meta$Built, ";"), "[[", 1))
  meta$RVersion <- get_minor_r_version(rver)
  meta
}

#' Update pak itself
#'
#' Use this function to update the released or development version of pak.
#'
#' @param force Whether to force an update, even if no newer version is
#'   available.
#' @param stream Whether to update to the
#'   * `"stable"`,
#'   * `"rc"` (release candidate) or
#'   * `"devel"` (development) version.
#'   * `"auto"` updates to the same stream as the current one.
#'
#'   Often there is no release candidate version, then `"rc"` also
#'   installs the stable version.
#'
#' @return Nothing.
#'
#' @export

pak_update <- function(
  force = FALSE,
  stream = c("auto", "stable", "rc", "devel")) {

  stopifnot(is_flag(force))
  stream <- match.arg(stream)
  stream <- pak_stream(stream)

  repo <- pak_repo()

  if (!is.null(.getNamespace("pak")$.__DEVTOOLS__)) {
    lib <- .libPaths()[1]
    warning(
      "`load_all()`-d pak package, updating in default library at\n  ",
      "`", lib, "`",
      immediate. = TRUE
    )
  } else {
    lib <- dirname(getNamespaceInfo("pak", "path"))
  }

  repo <- pak_repo(stream)
  meta <- pak_repo_metadata(repo)

  me <- detect_platform()
  cand <- which(
    meta$OS == me$os &
    meta$Arch == me$arch &
    meta$RVersion == me$rver
  )

  if (length(cand) == 0) {
    pak_update_unsupported_platform(stream, me, meta)
  } else if (length(cand) > 1) {
    warning("Multiple pak candidates are available for this platform, ",
            "this should not happen. Using the first one.")
    cand <- cand[1]
  }
  check_mac_cran_r(me, meta)

  upd <- should_update_to(meta[cand, , drop = FALSE])
  if (!upd && !force) {
    message("\nCurrent version is the latest, no need to update.")
    return(invisible())
  }

  url <- paste0(repo, me$os, "/", me$arch, "/", meta$File[cand])
  tgt <- file.path(tempdir(), meta$File[cand])
  utils::download.file(url, tgt, mode = "wb")

  date <- get_built_date(meta$Built[cand])
  message("\nUpdating to version ", meta$Version[cand], " (", date, ")\n")

  # Otherwise the subprocess might be locking some DLLs
  try(pkg_data$remote$kill(), silent = TRUE)

  # Windows cannot install binaries with arbitrary names, apparently.
  ext <- tools::file_ext(tgt)
  if (.Platform$OS.type == "windows" && ext == "zip") {
    dir.create(tmp <- tempfile(), recursive = TRUE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    tgt2 <- file.path(tmp, "pak.zip")
    if (!file.copy(tgt, tgt2)) {
      stop("Failed to copy downloaded file :(")
    }
    tgt <- tgt2
  }
  utils::install.packages(tgt, repos = NULL, type = "source", lib = lib)

  attached <- "package:pak" %in% search()
  message("\nReloading pak.")

  # Try to use it to see if it was successful
  suppressWarnings(tryCatch({
    eapply(asNamespace("pak"), base::force, all.names = TRUE)
    unloadNamespace("pak")
    # This works around the help lazy load DB errors
    intern <- base::.Internal
    lazyLoadDBflush <- function(...) NULL
    tryCatch(
      intern(lazyLoadDBflush(file.path(lib, "pak", "help", "pak.rdb"))),
      error = function(e) NULL
    )
    loadNamespace("pak")
    if (attached) library(pak)
    suppressWarnings(tools::Rd_db(package = "pak"))
  }, error = function(err) {
    message("\nFailed to reload pak. Please restart your R session.")
  }))

  invisible()
}

pak_update_unsupported_platform <- function(stream, me, meta) {
  message("pak has ", stream, " binaries for the following platforms:")
  meta$OS <- sub("^darwin", "macOS darwin", meta$OS)
  meta$OS <- sub("^mingw32", "Windows (mingw32)", meta$OS)
  pl <- paste0(meta$OS, ", ", meta$Arch)
  rv <- tapply(
    meta$RVersion,
    pl,
    function(x) paste0("R ", sort(x), ".x"),
    simplify = FALSE
  )
  pl2 <- sapply(rv, paste, collapse = ", ")
  message(paste0(" * ", names(pl2), ", ", pl2, "\n"))
  stop(
    "pak is not available for ", me$os, ", ",
    me$arch, ", R ", me$rver, ".x. Aborting now"
  )
  # TODO: tell how to install from source
}

check_mac_cran_r <- function(me, meta) {
  if (! grepl("^darwin", me$os)) return()
  if (.Platform$pkgType == "source") {
    stop(
      "pak only has binaries for the CRAN build of R, and this ",
      "seems to be a brew or another non-CRAN build."
    )
    # TODO: tell how to install from source
  }
}

should_update_to <- function(new) {
  # check if the right platform was installed
  current <- R.Version()$platform
  if (!platform_match(pak_sitrep_data$platform, current)) {
    message("\npak platform mismatch, trying to update to fix this...")
    return(TRUE)
  }

  # otherwise use version number first
  dsc <- utils::packageDescription("pak")
  if (package_version(dsc$Version) < new$Version) return(TRUE)

  # or the build date
  blt_cur <- get_built_date(dsc$Built)
  blt_new <- get_built_date(new$Built)
  if (is.na(blt_cur) || blt_cur < blt_new) return(TRUE)
  FALSE
}

get_built_date <- function(x) {
  if (!is_string(x)) return(NA_character_)
  # We can compare these dates as strings, so no need to parse
  strsplit(x, "[ ]*;[ ]*")[[1]][3]
}
