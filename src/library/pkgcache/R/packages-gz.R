packages_gz_cols <- function() {
  list(
    pkgs = c(
      "ref",
      "type",
      "direct",
      "status",
      "package",
      "version",
      "platform",
      "rversion",
      "repodir",
      "sources",
      "target",
      "needscompilation",
      "priority",
      "filesize",
      "sha256",
      "sysreqs",
      "built",
      "published"
    ),
    deps = c("upstream", "idx", "ref", "type", "package", "op", "version")
  )
}

#' @importFrom tools file_ext

read_packages_file <- function(
  path,
  mirror,
  repodir,
  platform,
  type = "standard",
  meta_path = NA_character_,
  bin_path = NA_character_,
  orig_r_version = NULL,
  rversion,
  ...,
  .list = list()
) {
  # We might have empty PACKAGES.gz files, we treat them as empty here
  if (file.exists(path) && file.size(path) == 0) {
    pkgs <- data_frame()
  } else {
    pkgs <- parse_packages(path)
  }
  meta <- read_metadata_file(meta_path)
  bin <- read_ppm_binaries(bin_path)
  extra <- c(
    list(repodir = repodir),
    list(...),
    .list
  )
  assert_that(all_named(extra))
  pkgs[names(extra)] <-
    if (nrow(pkgs)) extra else replicate(length(extra), character())
  names(pkgs) <- tolower(names(pkgs))

  # rversion might be in PACKAGES, then we keep it as is
  if (nrow(pkgs)) {
    if (!"rversion" %in% names(pkgs)) {
      pkgs$rversion <- rversion
    } else {
      pkgs$rversion[is.na(pkgs$rversion)] <- rversion
    }
  } else {
    pkgs$rversion <- character()
  }

  ## If Windows, then we need to check which binary has i386 support
  if (nrow(pkgs)) {
    if (
      platform %in%
        c("i386+x86_64-w64-mingw32", "i386-w64-mingw32", "x86_64-w64-mingw32")
    ) {
      xplatform <- rep("i386+x86_64-w64-mingw32", nrow(pkgs))
      if ("archs" %in% colnames(pkgs)) {
        archs <- gsub(" ", "", fixed = TRUE, pkgs$archs)
        p32 <- !is.na(archs) & archs == "i386"
        xplatform[p32] <- "i386-w64-mingw32"
        p64 <- !is.na(archs) & archs == "x64"
        xplatform[p64] <- "x86_64-w64-mingw32"
      }
    } else {
      xplatform <- rep(platform, nrow(pkgs))
    }
    if ("platform" %in% names(pkgs)) {
      pkgs$platform[is.na(pkgs$platform)] <- xplatform[is.na(pkgs$platform)]
    } else {
      pkgs$platform <- xplatform
    }
  } else {
    pkgs$platform <- character()
  }

  if (!"needscompilation" %in% names(pkgs)) {
    pkgs$needscompilation <- if (!"built" %in% names(pkgs)) {
      if (nrow(pkgs)) NA_character_ else character()
    } else {
      ifelse(is.na(pkgs$built), NA_character_, "no")
    }
  }

  if (!"priority" %in% names(pkgs)) {
    pkgs$priority <- rep(NA_character_, nrow(pkgs))
  }

  if (!nrow(pkgs)) {
    pkgs$package <- character()
    pkgs$version <- character()
  }
  pkgs$ref <- pkgs$package
  pkgs$type <- if (nrow(pkgs)) type else character()
  pkgs$direct <- if (nrow(pkgs)) FALSE else logical()
  pkgs$status <- if (nrow(pkgs)) "OK" else character()
  pkgs$target <- packages_make_target(
    pkgs$platform,
    repodir,
    pkgs$package,
    pkgs$version,
    pkgs[["file"]],
    pkgs[["path"]]
  )
  pkgs$mirror <- if (nrow(pkgs)) mirror else character()
  pkgs$sources <- packages_make_sources(
    mirror,
    pkgs$platform,
    pkgs$target,
    repodir,
    pkgs$package,
    pkgs$version,
    type,
    pkgs$downloadurl
  )

  if (!is.null(meta)) {
    metatarget <- paste0(repodir, "/", meta$file)
    map <- match(pkgs$target, metatarget)
    pkgs$filesize <- meta$size[map]
    pkgs$sha256 <- meta$sha[map]
    pkgs$sysreqs <- as.character(meta$sysreqs[map])
    pkgs$built <- as.character(meta$built[map])
    pkgs$published <- meta$published[map]
    pkgs$published[pkgs$published == ""] <- NA_character_
    pkgs$published <- as.POSIXct(pkgs$published, tz = "GMT")

    # fall back to previous version for filesize and sysreqs
    nameonly <- function(x) sub("_[^_]*$", "", x)
    nameonly_metatarget <- nameonly(metatarget)
    map2 <- match(nameonly(pkgs$target), nameonly_metatarget)
    # handle the case when file names not package names, e.g. R Universe
    miss_sr <- is.na(map2)
    map2[miss_sr] <- match(
      pkgs$package[miss_sr],
      basename(nameonly_metatarget)
    )
    filesize2 <- meta$size[map2]
    sysreqs2 <- as.character(meta$sysreqs[map2])
    pkgs$filesize <- ifelse(is.na(pkgs$filesize), filesize2, pkgs$filesize)
    pkgs$sysreqs <- ifelse(is.na(pkgs$sysreqs), sysreqs2, pkgs$sysreqs)
  } else {
    pkgs$filesize <- pkgs$filesize %||% rep(NA_integer_, nrow(pkgs))
    pkgs$sha256 <- pkgs$sha256 %||% rep(NA_character_, nrow(pkgs))
    pkgs$sysreqs <- pkgs$sysreqs %||%
      pkgs$systemrequirements %||%
      rep(NA_character_, nrow(pkgs))
    pkgs$built <- pkgs$built %||% rep(NA_character_, nrow(pkgs))
    pkgs$published <- pkgs$published %||% rep(NA_character_, nrow(pkgs))
  }

  # We add some Bioconductor system requirements manually
  if (type == "bioc") {
    mch <- match(pkgenv$bioc_sysreqs$Package, pkgs$package)
    curval <- pkgs$sysreqs[mch]
    toadd <- is.na(curval) & !is.na(mch)
    pkgs$sysreqs[mch[toadd]] <- pkgenv$bioc_sysreqs$SystemRequirements[toadd]
  }

  # If it was explicitly in the metadata, keep it
  if ("systemrequirements" %in% names(pkgs)) {
    pkgs$sysreqs <- ifelse(
      !is.na(pkgs$systemrequirements),
      pkgs$systemrequirements,
      pkgs$sysreqs
    )
    pkgs$systemrequirements <- NULL
  }

  # PPM sources are really binaries for the current platform
  hasbin <- pkgs$package %in% bin$Package
  if (length(orig_r_version) == 1 && sum(hasbin) > 0) {
    plat <- current_r_platform()
    pkgs$platform[hasbin] <- plat
    pkgs$rversion[hasbin] <- orig_r_version
    pkgs$target[hasbin] <- paste0(
      dirname(pkgs$target[hasbin]),
      "/",
      plat,
      "/",
      orig_r_version,
      "/",
      basename(pkgs$target[hasbin])
    )
    pkgs$filesize[hasbin] <- NA_integer_
    pkgs$sha256[hasbin] <- NA_integer_
    pkgs$needscompilation[hasbin] <- NA
  }

  # Assume that R-universe Linux binaries are for the current platform.
  # They seem to have a Built field, so use that for the R version.
  if (grepl("r-universe.dev/bin/linux", mirror, fixed = TRUE)) {
    built <- strsplit(pkgs$built, ";")
    # add $rversion from Built, if not there already
    miss_r <- pkgs$rversion == "*"
    built_r <- substr(trimws(vcapply(built[miss_r], "[[", 1)), 3, 1000)
    pkgs$rversion[miss_r] <- sub(
      "^([0-9]+[.][0-9]+)[.][0-9]+$",
      "\\1",
      built_r,
      perl = TRUE
    )
    pkgs$rversion[is.na(pkgs$rversion)] <- "*"

    # add $platform from build, assume current platform if missing
    miss_plat <- pkgs$platform == "source" & !is.na(pkgs$built)
    built_plat <- trimws(vcapply(built[miss_plat], "[[", 2))
    current_plat <- current_r_platform()
    built_plat[built_plat == ""] <- current_plat
    pkgs$platform[miss_plat][
      built_plat == current_plat | startsWith(current_plat, built_plat)
    ] <- current_plat
  }

  # If we only want one Windows platform, then filter here
  if (platform %in% c("i386-w64-mingw32", "x86_64-w64-mingw32")) {
    drop <- pkgs$platform != platform &
      pkgs$platform != "i386+x86_64-w64-mingw32"
    if (any(drop)) {
      pkgs <- pkgs[!drop, ]
    }
  }

  deps <- packages_parse_deps(pkgs)
  pkgs_deps <- split(
    deps[, -(1:2)],
    factor(deps$idx, levels = seq_len(nrow(pkgs)))
  )
  pkgs_deps <- lapply(pkgs_deps, function(x) {
    rownames(x) <- NULL
    x
  })
  pkgs$deps <- unname(pkgs_deps)
  list(pkgs = pkgs, deps = deps)
}

#' @importFrom utils read.csv

read_metadata_file <- function(path) {
  if (is.na(path)) return(NULL)
  on.exit(tryCatch(close(con), error = function(x) NULL), add = TRUE)
  tryCatch(
    suppressWarnings({
      md <- read.csv(
        con <- gzfile(path, open = "r"),
        stringsAsFactors = FALSE
      )
      if ("filesize" %in% names(md)) {
        md$filesize <- as.integer(md$filesize)
      }
      md
    }),
    error = function(e) NULL
  )
}

read_ppm_binaries <- function(path) {
  if (is.na(path) || !file.exists(path) || file.size(path) == 0) {
    pkgs <- data_frame()
  } else {
    pkgs <- parse_packages(path)
  }
  pkgs
}

packages_parse_deps <- function(pkgs) {
  no_pkgs <- nrow(pkgs)
  cols <- intersect(colnames(pkgs), tolower(dep_types()))
  ## as.character is for empty data frame, e.g. from empty BioC repos
  deps <- as.character(unlist(pkgs[, cols], use.names = FALSE))
  nna <- which(!is.na(deps))
  if (length(nna)) {
    not_na_deps <- deps[nna]
    sp <- strsplit(not_na_deps, ",", fixed = TRUE)
    ll <- sapply(sp, length, USE.NAMES = FALSE)
    sp <- unlist(sp, use.names = FALSE)
    parsed <- re_match(
      sp,
      paste0(
        "^\\s*(?<package>[^(\\s]+)\\s*",
        "(?:\\((?<op>[^0-9\\s]+)\\s*(?<version>[^)\\s]+)\\))?\\s*$"
      )
    )
    parsed$idx <- rep(rep(seq_len(no_pkgs), length(cols))[nna], ll)
    parsed$type <- rep(rep(cols, each = no_pkgs)[nna], ll)
    parsed$ref <- parsed$package
    parsed$upstream <- pkgs$package[parsed$idx]
    parsed <- parsed[, c(
      "upstream",
      "idx",
      "ref",
      "type",
      "package",
      "op",
      "version"
    )]
    parsed <- parsed[order(parsed$idx), ]
  } else {
    parsed <- data_frame(
      upstream = character(),
      idx = integer(),
      ref = character(),
      type = character(),
      package = character(),
      version = character(),
      op = character()
    )
  }

  parsed <- parsed[order(parsed$idx, parsed$package, parsed$type), ]
  parsed
}

packages_make_target <- function(
  platform,
  repodir,
  package,
  version,
  file,
  path
) {
  if (!length(platform)) return(character())
  platform <- rep_len(platform, length(package))

  assert_that(
    is_character(platform),
    is_string(repodir),
    is_character(package),
    is_character(version),
    length(version) == length(package),
    is.null(file) || (is.character(file) && length(file) == length(package)),
    is.null(path) || (is.character(path) && length(path) == length(package))
  )

  res <- rep(NA_character_, length(package))
  ext <- get_cran_extension(platform)

  ## 'File' field, if present
  if (!is.null(file)) {
    wh <- !is.na(file)
    if (any(wh)) {
      res[wh] <- paste0(repodir, "/", file[wh])
    }
  }

  ## 'Path' field, if present
  if (!is.null(path)) {
    wh <- is.na(res) & !is.na(path)
    if (any(wh)) {
      res[wh] <- paste0(
        repodir,
        "/",
        path[wh],
        "/",
        package[wh],
        "_",
        version[wh],
        ext[wh]
      )
    }
  }

  ## Otherwise default
  if (anyNA(res)) {
    wh <- is.na(res)
    res[wh] <- paste0(repodir, "/", package[wh], "_", version[wh], ext[wh])
  }

  res
}

packages_make_sources <- function(
  mirror,
  platform,
  target,
  repodir,
  package,
  version,
  type,
  downloadurl
) {
  assert_that(
    is_string(mirror),
    is_character(platform),
    is_character(target),
    is_string(repodir),
    is_character(package),
    is_character(version),
    length(version) == length(package),
    is.null(downloadurl) || is.character(downloadurl)
  )

  if (!length(package)) return(list())
  platform <- rep_len(platform, length(package))

  result <- replicate(length(package), NULL)

  url <- paste0(mirror, "/", target)
  url2 <- paste0(
    mirror,
    "/",
    repodir,
    "/Archive/",
    package,
    "/",
    package,
    "_",
    version,
    ".tar.gz"
  )
  macurl <- paste0("https://mac.r-project.org/", target)

  os <- parse_platform(platform)$os
  macbin <- type == "cran" & !is.na(os) & grepl("^darwin", os)
  result[macbin] <- zip_vecs(url[macbin], macurl[macbin])

  # We don't use Archive for recommended packages, until this is
  # fixed in RSPM: https://github.com/rstudio/package-manager/issues/10471
  # To work around: https://github.com/r-lib/pak/issues/467

  # We also don't use it on PPM (Linux), because PPM might serve a source
  # package on Archive, or a different binary build:
  # https://github.com/r-lib/pak/issues/623
  recommended <- package %in% recommended_packages()
  ppm <- grepl("__linux__", url)
  cransrc <- type == "cran" & platform == "source" & !recommended & !ppm
  result[cransrc] <- zip_vecs(url[cransrc], url2[cransrc])

  others <- vlapply(result, is.null)
  result[others] <- as.list(url[others])

  if (!is.null(downloadurl)) {
    result[!is.na(downloadurl)] <- as.list(na_omit(downloadurl))
  }

  result
}

merge_packages_data <- function(..., .list = list()) {
  pkgslist <- c(list(...), .list)

  pkgs <- rbind_expand(.list = lapply(pkgslist, "[[", "pkgs"))

  ## Need to shift deps indices first to merge deps
  num_pkgs <- viapply(pkgslist, function(x) nrow(x$pkgs), USE.NAMES = FALSE)
  shifts <- c(0L, cumsum(num_pkgs))
  for (i in seq_along(pkgslist)) {
    pkgslist[[i]]$deps$idx <- pkgslist[[i]]$deps$idx + shifts[i]
  }
  deps <- rbind_expand(.list = lapply(pkgslist, "[[", "deps"))

  list(pkgs = pkgs, deps = deps)
}

rbind_expand <- function(..., .list = list()) {
  data <- c(list(...), .list)
  cols <- unique(unlist(lapply(data, function(x) colnames(x))))
  for (i in seq_along(data)) {
    miss_cols <- setdiff(cols, colnames(data[[i]]))
    if (length(miss_cols)) {
      na_df <- as_data_frame(structure(
        replicate(
          length(miss_cols),
          if (nrow(data[[i]])) NA else logical(),
          simplify = FALSE
        ),
        names = miss_cols
      ))
      data[[i]] <- as_data_frame(cbind(data[[i]], na_df))
    }
  }

  do.call(rbind, data)
}
