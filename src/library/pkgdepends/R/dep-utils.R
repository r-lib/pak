#' @export
#' @rdname pkg_dep_types

pkg_dep_types_hard <- function() c("Depends", "Imports", "LinkingTo")

#' @export
#' @rdname pkg_dep_types

pkg_dep_types_soft <- function() c("Suggests", "Enhances")

#' Possible package dependency types
#'
#' Hard dependencies are needed for a package to load, soft dependencies
#' are optional.
#'
#' @return A string vector of dependency types, capitalized.
#'
#' @family package dependency utilities
#' @export

pkg_dep_types <- function() c(pkg_dep_types_hard(), pkg_dep_types_soft())

make_null_deps <- function() {
  data_frame(
    ref = character(),
    type = character(),
    package = character(),
    op = character(),
    version = character()
  )
}

parse_deps <- function(deps, type) {
  assert_that(length(deps) == length(type))
  deps <- lapply(strsplit(deps, ","), str_trim)
  rx <- paste0(
    "(?<type>)",
    "^\\s*",
    "(?<package>[^\\s]+)",
    "\\s*",
    "(?:[(](?<op>>|>=|==|<|<=)\\s*(?<version>[-0-9\\.]+)[)])?\\s*$"
  )
  base <- base_packages()
  lapply(seq_along(deps), function(i) {
    x <- omit_cols(re_match(deps[[i]], pattern = rx), c(".text", ".match"))
    x$type <- if (length(x$type) > 0) type[[i]] else character()
    x[!x$package %in% base, ]
  })
}

deps_from_desc <- function(deps, last) {
  op_ver <- strsplit(deps$version, "\\s+")
  deps$op <- vcapply(op_ver, "[", 1)
  deps$op[deps$op == "*"] <- ""
  deps$version <- vcapply(op_ver, "[", 2)
  deps$version[is.na(deps$version)] <- ""
  deps$ref <- paste0(deps$package, if (last) "@last")
  base <- base_packages()
  res <- as_data_frame(deps[
    !deps$package %in% base,
    c("ref", "type", "package", "op", "version")
  ])
  rownames(res) <- NULL
  res
}

parse_all_deps <- function(deps) {
  deps <- na.omit(deps)
  res <- do.call(rbind, parse_deps(deps, names(deps)))
  if (is.null(res)) res <- parse_deps("", "")[[1]]
  res$ref <- res$package
  res[, c("ref", setdiff(names(res), "ref"))]
}

resolve_ref_deps <- function(deps, remotes, extra) {
  deps <- deps_from_desc(deps, last = FALSE)

  parse <- function(x) {
    str_trim(strsplit(x, "\\s*,\\s*", perl = TRUE)[[1]])
  }

  check_names <- function(x) {
    nms <- vcapply(x, function(e) e$package %||% NA_character_)
    bad <- is.na(nms)
    if (any(bad)) {
      badpkgs <- vcapply(x, "[[", "ref")[bad]
      badtypes <- vcapply(x, "[[", "type")[bad]
      throw(pkg_error(
        "Cannot determine package {cli::qty(sum(bad))}name{?s} for
         {sum(bad)} package{?s}: {.val {badpkgs}}.",
        i = "Maybe you need to add a {.code <packagename>{zwnj()}=} prefix?"
      ))
    }
    nms
  }

  if (!is.na(remotes)) {
    remotes <- str_trim(na.omit(remotes))
    remotes <- parse(remotes)
    remotes_packages <- check_names(parse_pkg_refs(remotes))
    keep <- which(remotes_packages %in% deps$package)
    deps$ref[match(remotes_packages[keep], deps$package)] <- remotes[keep]
  }

  if (length(extra) > 0) {
    xdeps <- parse_all_deps(extra)
    xdeps$package <- check_names(parse_pkg_refs(xdeps$package))
    deps <- rbind(deps, xdeps)
  }

  deps
}

#' Shorthands for dependency specifications
#'
#' @details
#' ```{r child = "tools/doc/deps.Rmd"}
#' ```
#' `r doc_share_rmd("tools/doc/deps.Rmd", "inst/docs/deps.rds")`
#'
#' @param deps See below.
#' @return A named list with two character vectors: `direct`, `indirect`,
#' the dependency types to use for direct installations and dependent
#' packages.
#'
#' @family package dependency utilities
#' @export

as_pkg_dependencies <- function(deps) {
  assert_that(is_dependencies(deps))

  hard <- pkg_dep_types_hard()
  soft <- pkg_dep_types()

  res <- if (isTRUE(deps)) {
    list(c(hard, "Suggests"), hard)
  } else if (identical(deps, FALSE)) {
    list(character(), character())
  } else if (is_na_scalar(deps)) {
    list(hard, hard)
  } else if (is.list(deps) && all(names(deps) == c("direct", "indirect"))) {
    deps
  } else {
    list(deps, hard)
  }

  res <- lapply(res, function(x) {
    if ("hard" %in% x) {
      x <- unique(c(hard, setdiff(x, "hard")))
    }
    if (any(c("soft", "all") %in% x)) {
      x <- unique(c(soft, setdiff(x, c("all", "soft"))))
    }
    x
  })

  names(res) <- c("direct", "indirect")
  res
}

extra_config_fields <- function(x) {
  grep("^config/needs/", x, value = TRUE, ignore.case = TRUE)
}
