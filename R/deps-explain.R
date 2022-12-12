
#' Explain how a package depends on other packages
#'
#' Extract dependency chains from `pkg` to `deps`.
#'
#' This function is similar to [pkg_deps_tree()], but its output is easier
#' to read if you are only interested is certain packages (`deps`).
#'
#' @param deps Package names of the dependencies to explain.
#' @param upgrade Whether to use the most recent available package
#'   versions.
#' @inheritParams pkg_install
#' @return A named list with a print method. First entries are the
#'   function arguments: `pkg`, `deps`, `dependencies`, the last one is
#'   `paths` and it contains the results in a named list, the names are
#'   the package names in `deps`.
#'
#' @export
#' @examples
#' \dontrun{
#' # How does the GH version of usethis depend on cli and ps?
#' pkg_deps_explain("r-lib/usethis", c("cli", "ps"))
#' }

pkg_deps_explain <- function(pkg, deps, upgrade = TRUE, dependencies = NA) {
  stopifnot(length(pkg == 1) && is.character(pkg))
  remote(
    function(...) {
      get("pkg_deps_explain_internal", asNamespace("pak"))(...)
    },
    list(pkg = pkg, deps = deps, upgrade = upgrade,
         dependencies = dependencies)
  )
}

pkg_deps_explain_internal <- function(pkg, deps, upgrade, dependencies = NA) {
  data <- pkg_deps_internal2(pkg, upgrade, dependencies)$get_solution()$data
  wpkg <- match(pkg, data$ref)

  paths <- structure(vector("list", length(deps)), names = deps)

  types <- pkgdepends::as_pkg_dependencies(dependencies)
  deps1 <- local({
    d1 <- data$deps[[wpkg]]
    pk <- d1$package[ tolower(d1$type) %in% tolower(types[[1]]) ]
    na_omit(match(pk, data$package))
  })
  adjlist <- lapply(data$deps, function(di) {
    p <- di$package[ tolower(di$type) %in% tolower(types[[2]]) ]
    p <- setdiff(p, "R")
    na_omit(match(p, data$package))
  })
  adjlist[[wpkg]] <- deps1

  added <- rep(FALSE, length(adjlist))
  added[wpkg] <- TRUE
  nptr <- rep(1L, length(adjlist))
  stack <- wpkg
  ssize <- 1L

  while (ssize > 0L) {
    act <- stack[ssize]

    # select a node that hasn't been added yet, starting from nptr
    allneis <- adjlist[[act]]
    good <- seq_along(allneis) >= nptr[act] & !added[allneis]
    neiidx <- which(good)[1]

    if (!is.na(neiidx)) {
      nei <- allneis[neiidx]
      nptr[act] <- neiidx + 1L
      ssize <- ssize + 1L
      stack[ssize] <- nei
      added[nei] <- TRUE
      dpkg <- data$package[nei]
      if (dpkg %in% deps) {
        paths[[dpkg]] <- c(paths[[dpkg]], list(data$package[stack[1:ssize]]))
      }

    } else {
      ssize <- ssize - 1L
      nptr[act] <- 1L
      added[act] <- FALSE
    }
  }

  ret <- list(
    pkg = pkg, deps = deps, dependencies = dependencies, paths = paths
  )
  class(ret) <- "pak_deps_explain"

  ret
}

#' @export

format.pak_deps_explain <- function(x, ...) {

  format_path1 <- function(p1) {
    strwrap(paste0(p1, collapse = " -> "), exdent = 2L)
  }

  format_path <- function(path) {
    if (length(path) > 0) {
      c(unlist(lapply(path, format_path1)), "")
    }
  }

  nope <- names(x$paths)[viapply(x$paths, length) == 0L]
  fmt <- c(
    unlist(lapply(x$paths, format_path)),
    if (length(nope) > 0L) paste0("x ", nope)
  )
  if (fmt[[length(fmt)]] == "") fmt <- fmt[-length(fmt)]

  fmt
}

#' @export

print.pak_deps_explain <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
