## ------------------------------------------------------------------------
## API

parse_remote_installed <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, type_installed_rx())

  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "installed"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i, ])
  )
}

resolve_remote_installed <- function(
  remote,
  direct,
  config,
  cache,
  dependencies,
  ...
) {
  deps <- setdiff(dependencies[[2 - direct]], c("LinkingTo", "linkingto"))
  resolve_installed(cache, remote, direct, deps)
}

download_remote_installed <- function(
  resolution,
  target,
  target_tree,
  config,
  cache,
  which,
  on_progress
) {
  "Had"
}

satisfy_remote_installed <- function(resolution, candidate, config, ...) {
  TRUE
}

installedok_remote_installed <- function(installed, solution, config, ...) {
  # Keep the plan, we were already using the installed package
  FALSE
}

## ----------------------------------------------------------------------
## Internal functions

type_installed_rx <- function() {
  paste0(
    "^",
    "(?:installed::)?",
    "(?<library>.*)/",
    "(?<package>",
    package_name_rx(),
    ")",
    "$"
  )
}

make_installed_cache <- function(library, packages = NULL, priority = NULL) {
  inst <- pkgcache::parse_installed(
    library = library,
    priority = priority,
    lowercase = TRUE,
    reencode = FALSE
  )

  # Need nrow(inst) > 0 because otherwise it drops to a vector, and
  # R ignores 'drop' in this case. (?!)
  if (!is.null(packages) && nrow(inst) > 0) {
    inst <- inst[inst$package %in% packages, ]
  }

  all_fields <- names(inst)
  fields <- tolower(unique(c(
    "Package",
    "Title",
    "Version",
    "Depends",
    "Suggests",
    "Imports",
    "LinkingTo",
    "Enhances",
    "Built",
    "MD5sum",
    "NeedsCompilation",
    "Platform",
    "License",
    "Priority",
    "Repository",
    "biocViews",
    grep("^remote", all_fields, value = TRUE),
    grep("^config/needs/", all_fields, value = TRUE)
  )))

  miss <- setdiff(fields, names(inst))
  for (nm in miss) {
    inst[[nm]] <- if (nrow(inst)) NA_character_ else character()
  }

  pkgs <- inst[, names(inst) %in% fields]

  # Called `sysreqs` in pkgcache, so we call it the same here
  pkgs$sysreqs <- inst[["systemrequirements"]] %||%
    rep(NA_character_, nrow(pkgs))

  if (nrow(pkgs) == 0) {
    pkgs$ref <- character()
  } else {
    pkgs$ref <- paste0("installed::", library, "/", pkgs$package)
  }
  pkgs$type <- rep("installed", nrow(pkgs))
  pkgs$status <- rep("OK", nrow(pkgs))
  built <- parse_built(inst$built)
  pkgs$rversion <- built$R
  pkgs$platform <- built$Platform

  # This is usually a broken installation, but 'Built' is also missing
  # for the core translations package
  pkgs$rversion[is.na(pkgs$rversion)] <- current_r_version()
  pkgs$platform[is.na(pkgs$platform)] <- current_r_platform()

  # On Windows, we need to check the Archs field
  winbin <- pkgs$platform != "" &
    !is.na(built$OStype) &
    built$OStype == "windows"
  if (any(winbin)) {
    # `archs` could be missing, because of a base R bug:
    # https://github.com/r-lib/pak/issues/448#issuecomment-1354807441
    if (is.null(inst[["archs"]])) {
      inst$archs <- rep(NA_character_, nrow(inst))
    }
    # we assume x64 on newer R, and i386 + x64 on older R, as this is
    # what typically happens on CRAN
    rver <- sub("R ", "", pkgs$rversion)
    inst[["archs"]] <- ifelse(
      winbin & is.na(inst[["archs"]]),
      ifelse(package_version(rver) < "4.2.0", "i386,x64", "x64"),
      inst[["archs"]]
    )
    archs <- gsub(" ", "", inst$archs[winbin])
    pkgs$platform[winbin] <- ifelse(
      is.na(archs) | archs %in% c("i386,x64", "x64,i386"),
      "i386+x86_64-w64-mingw32",
      pkgs$platform[winbin]
    )
  }
  pkgs$platform[pkgs$platform == ""] <- "*"

  pkgs$sources <- replicate(nrow(pkgs), character(), simplify = FALSE)
  pkgs$needscompilation <- ifelse(
    is.na(pkgs$needscompilation),
    NA,
    tolower(pkgs$needscompilation) %in% c("true", "yes")
  )

  cran <- !is.na(pkgs$repository) & pkgs$repository == "CRAN"
  bioc <- !is.na(pkgs$biocviews) & pkgs$biocviews != ""
  pkgs$repotype <- ifelse(cran, "cran", ifelse(bioc, "bioc", NA_character_))

  deps <- packages_parse_deps(pkgs)
  pkgs_deps <- split(
    deps[, -(1:2)],
    factor(deps$idx, levels = seq_len(nrow(pkgs)))
  )
  pkgs$deps <- unname(pkgs_deps)
  list(pkgs = pkgs, deps = deps)
}

merge_installed_caches <- function(c1, c2) {
  newdeps <- c2$deps
  newdeps$idx <- newdeps$idx + nrow(c1$pkgs)
  list(
    pkgs = rbind_expand(c1$pkgs, c2$pkgs),
    deps = rbind(c1$deps, newdeps)
  )
}

#' Status of packages in a library
#'
#' Query data of all packages in a package library.
#'
#' @param library Path to library.
#' @param packages If not `NULL`, then only these packages are shown.
#' @return Data frame that contains data about the packages
#'   installed in the library.
#'   ```{r child = "tools/doc/lib-status-return.Rmd"}
#'   ```
#'  `r doc_share_rmd("tools/doc/lib-status-return.Rmd", "inst/docs/lib-status-return.rds")`
#'
#' @export

lib_status <- function(library = .libPaths()[1], packages = NULL) {
  st <- make_installed_cache(library, packages)$pkgs
  st$library <- if (nrow(st) > 0) library else character()
  st <- st[, c("library", setdiff(colnames(st), "library")), drop = FALSE]
  rm <- extra_config_fields(colnames(st))
  st[, setdiff(colnames(st), rm), drop = FALSE]
}

# TODO: parse Remotes and Config/Needs/* fields

packages_parse_deps <- function(pkgs) {
  no_pkgs <- nrow(pkgs)
  cols <- intersect(colnames(pkgs), tolower(pkg_dep_types()))
  ## as.character is for empty data frames, e.g. from empty BioC repos
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

  parsed
}

resolve_installed <- function(cache, remotes, direct, dependencies) {
  dependencies <- tolower(dependencies)

  ## Single remote, or a list of remotes
  if ("ref" %in% names(remotes)) {
    refs <- remotes$ref
    packages <- remotes$package
    params <- list(remotes$params %||% character())
  } else {
    refs <- vcapply(remotes, "[[", "ref")
    packages <- vcapply(remotes, "[[", "package")
    params <- lapply(remotes, "[[", "params")
  }

  pkgs <- cache$installed$pkgs
  cols <- c(
    "ref",
    "type",
    "status",
    "package",
    "version",
    "license",
    "needscompilation",
    "priority",
    "md5sum",
    "platform",
    "rversion",
    "sources",
    "built",
    "deps",
    "repotype",
    "sysreqs"
  )
  res <- pkgs[pkgs$package %in% packages, cols]
  repotype <- pkgs$repotype[pkgs$package %in% packages]

  res$direct <- direct
  res$metadata <- get_installed_metadata(res)
  res$deps <- lapply(res$deps, function(x) x[x$type %in% dependencies, ])
  # this might include extra rows from recommended packages
  idx <- match(res$ref, refs)
  res$params <- replicate(nrow(res), character())
  res$params[!is.na(idx)] <- params[na.omit(idx)]

  extracols <- c("repotype", grep("^remote", names(pkgs), value = TRUE))
  extra <- pkgs[pkgs$package %in% packages, extracols, drop = FALSE]
  res$extra <- lapply(seq_len(nrow(res)), function(i) extra[i, , drop = FALSE])

  attr(res, "unknown_deps") <-
    setdiff(unique(unlist(lapply(res$deps, "[[", "package"))), "R")

  res
}

get_installed_metadata <- function(tab) {
  meta <- replicate(nrow(tab), character(), simplify = FALSE)
  for (i in seq_len(nrow(tab))) {
    meta[[i]] <-
      c(
        RemoteType = tab$type[i],
        RemotePkgRef = tab$ref[i],
        RemotePkgPlatform = tab$platform[i],
        RemoteSha = tab$version[i]
      )
  }
  meta
}

parse_built <- function(x) {
  xp <- strsplit(x, ";", fixed = TRUE)
  data.frame(
    stringsAsFactors = FALSE,
    R = trimws(vapply(xp, "[", character(1), 1)),
    Platform = trimws(vapply(xp, "[", character(1), 2)),
    Date = trimws(vapply(xp, "[", character(1), 3)),
    OStype = trimws(vapply(xp, "[", character(1), 4))
  )
}
