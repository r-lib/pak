#' List installed tree-sitter parsers
#'
#' The ts package contains a common interface to several tree-sitter
#' parsers, implemented in other R packages. `ts_list_parsers()` lists
#' the available parsers installed in the system.
#'
#' To see tree-sitter parser packages that are available on CRAN, but not
#' installed on your system, see the packages that depend on ts and have a
#' name with a 'ts' prefix.
# TODO: add URLs here, or pkgsearch  example once ts is on CRAN.
#'
# NOTE: this only works if all suggested packages are installed.
#' Here is an example that includes all tree-sitter parsers at this time:
#'
#' ```{asciicast}
#' ts_list_parsers()
#' ```
#'
#' @param lib_path Library paths to search for installed packages.
#'   Default is [base::.libPaths()].
#' @return A data frame with columns:
#' * `package`: character, the name of the package.
#' * `version`: character, the version of the package.
#' * `title`: character, the title of the package.
#' * `library`: character, the library path where the package is installed.
#' * `loaded`: logical, whether the package is currently loaded.
#' @export
#' @examples
#' ts_list_parsers()

ts_list_parsers <- function(lib_path = .libPaths()) {
  # installed packages
  ipkgs <- list_installed_packages(lib_path)
  ipkgs <- ipkgs[grepl("^ts.", basename(ipkgs))]
  dscs1 <- lapply(ipkgs, function(pkg) {
    suppressWarnings(utils::packageDescription(
      basename(pkg),
      lib.loc = dirname(pkg)
    ))
  })

  # loaded packages
  lpkgs <- loadedNamespaces()
  lpkgs <- lpkgs[grepl("^ts.", lpkgs)]
  dscs2 <- lapply(lpkgs, function(pkg) {
    suppressWarnings(utils::packageDescription(pkg))
  })

  dscs <- Filter(has_ts_parser, c(dscs1, dscs2))
  path <- map_chr(dscs, attr, "file", exact = TRUE)
  dscs <- dscs[!duplicated(path)]
  path <- path[!duplicated(path)]

  pkgpath <- as.character(ifelse(
    basename(path) == "DESCRIPTION",
    dirname(path),
    dirname(dirname(path))
  ))

  tspkgs <- data_frame(
    package = map_chr(dscs, "[[", "Package"),
    version = map_chr(dscs, "[[", "Version"),
    title = map_chr(dscs, "[[", "Title"),
    library = dirname(pkgpath)
  )
  nspath <- map_chr(tspkgs$package, function(pkg) {
    if (pkg %in% loadedNamespaces()) {
      getNamespaceInfo(pkg, "path")
    } else {
      NA_character_
    }
  })
  tspkgs$loaded <- tspkgs$package %in% loadedNamespaces() & nspath == pkgpath

  tspkgs <- tspkgs[order(tspkgs$package, !tspkgs$loaded, tspkgs$version), ]

  class(tspkgs) <- c(
    "ts_parser_list",
    class(tspkgs)
  )
  tspkgs
}

format_rd_parser_list <- function(lst, method = NULL) {
  if (is.null(method)) {
    format_rd_parser_list_no_method(lst)
  } else {
    format_rd_parser_list_method(lst, method)
  }
}

no_ts_package_message <- function() {
  avail <- read.dcf(file.path(doc_path("ts"), "ts-packages.dcf"))
  paste0(
    "No tree-sitter parser packages are installed.\n",
    "Available tree-sitter parser packages:\n",
    "\\itemize{\n",
    paste0(
      collapse = "\n",
      "\\item \\strong{\\href{",
      avail[, "URL"],
      "}{",
      avail[, "Package"],
      "}}: ",
      avail[, 'Title'],
      "\\if{text}{ (",
      avail[, "URL"],
      ")}."
    ),
    "\n}\n"
  )
}

format_rd_parser_list_no_method <- function(lst) {
  if (nrow(lst) == 0) {
    return(no_ts_package_message())
  }
  lst <- lst[!duplicated(lst[, c("package", "version")]), ]

  hd <- "Available tree-sitter parsers"
  align <- "lll"
  loaded_hd <- ""
  loaded <- rep("", nrow(lst))
  if (Sys.getenv("IN_PKGDOWN") != "true") {
    hd <- "Installed tree-sitter parsers"
    align <- "llcl"
    loaded_hd <- "\\strong{Loaded} \\tab "
    loaded <- paste0(ifelse(lst$loaded, "yes", "no"), " \\tab ")
  }

  lines <- map_chr(
    seq_len(nrow(lst)),
    function(i) {
      pkg <- lst$package[i]
      ver <- lst$version[i]
      title <- lst$title[i]
      glue(
        "\\link[{pkg}:{pkg}-package]{{ {pkg}} \\tab \\
         {ver} \\tab \\
         {loaded[i]} \\
         {title}.
         "
      )
    }
  )

  paste0(
    "\\subsection{",
    hd,
    "}{\n",
    "\\tabular{",
    align,
    "}{\n",
    "\\strong{Package} \\tab \\strong{Version} \\tab ",
    loaded_hd,
    "\\strong{Title} \\cr\n",
    paste(lines, collapse = "\\cr\n"),
    "\n}\n",
    "}\n"
  )
}

format_rd_parser_list_method <- function(lst, method) {
  if (nrow(lst) == 0) {
    return(no_ts_package_message())
  }
  lst <- lst[!duplicated(lst[, c("package", "version")]), ]

  hd <- "Available tree-sitter parsers"
  loaded_hd <- ""
  loaded <- rep("", nrow(lst))
  align <- "llll"
  if (Sys.getenv("IN_PKGDOWN") != "true") {
    hd <- "Installed tree-sitter parsers"
    align <- "llcll"
    loaded_hd <- "\\strong{Loaded} \\tab "
    loaded <- paste0(ifelse(lst$loaded, "yes", "no"), " \\tab ")
  }

  lines <- map_chr(
    seq_len(nrow(lst)),
    function(i) {
      pkg <- lst$package[i]
      ver <- lst$version[i]
      title <- lst$title[i]
      method <- if (doc_has_method(method, pkg)) {
        cls <- sub("^ts", "ts_tree_", pkg)
        glue(
          "\\code{{\\link[{pkg}:{method}.{cls}]{{{method}(<ts_tree_{pkg}>)}}}}"
        )
      }
      glue(
        "\\strong{{\\link[{pkg}:{pkg}-package]{{{pkg}}}}} \\tab \\
         {ver} \\tab \\
         {loaded[i]} \\
         {title}. \\tab \\
         {method}"
      )
    }
  )
  paste0(
    "\\subsection{",
    hd,
    "}{\n",
    "This is the manual page of the \\code{",
    method,
    "()} S3 generic function.\n",
    "Methods in parser packages may override this generic.\n",
    "For the ones that do see the links to their manual pages in the table.\n",
    "\\tabular{",
    align,
    "}{\n",
    "\\strong{Package} \\tab \\strong{Version} \\tab ",
    loaded_hd,
    "\\strong{Title} \\tab \\strong{Method} \\cr\n",
    paste(lines, collapse = "\\cr\n"),
    "\n}\n",
    "}\n"
  )
}

has_ts_parser <- function(dsc) {
  is.list(dsc) &&
    !is.null(dsc$Imports) &&
    "ts" %in% parse_deps("Imports", dsc$Imports)$package
}

list_installed_packages <- function(lib_path = .libPaths()) {
  unlist(lapply(lib_path, dir, full.names = TRUE))
}

parse_deps <- function(type, deps) {
  deps <- str_trim(strsplit(deps, ",")[[1]])
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  deps <- deps[vapply(deps, function(x) length(x) != 0, FUN.VALUE = logical(1))]
  res <- data.frame(
    stringsAsFactors = FALSE,
    type = if (length(deps)) type else character(),
    package = vapply(deps, "[", "", 1),
    version = vapply(deps, "[", "", 2)
  )
  res$version <- gsub("\\s+", " ", res$version)
  res$version[is.na(res$version)] <- "*"
  res
}

str_trim <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x, useBytes = TRUE), useBytes = TRUE)
}
