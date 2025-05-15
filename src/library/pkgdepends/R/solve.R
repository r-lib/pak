#' The dependency solver
#'
#' The dependency solver takes the resolution information, and works out
#' the exact versions of each package that must be installed, such that
#' version and other requirements are satisfied.
#'
#' ## Solution policies
#'
#' The dependency solver currently supports two policies: `lazy` and
#' `upgrade`. The `lazy` policy prefers to minimize installation time,
#' and it does not perform package upgrades, unless version requirements
#' require them. The `upgrade` policy prefers to update all package to
#' their latest possible versions, but it still considers that version
#' requirements.
#'
#' ## The integer problem
#'
#' Solving the package dependencies requires solving an integer linear
#' problem (ILP). This subsection briefly describes how the problem is
#' represented as an integer problem, and what the solution policies
#' exactly mean.
#'
#' Every row of the package resolution is a candidate for the dependency
#' solver. In the integer problem, every candidate corresponds to a binary
#' variable. This is 1 if that candidate is selected as part of the
#' solution, and 0 otherwise.
#'
#' The objective of the ILP minimization is defined differently for
#' different solution policies. The ILP conditions are the same.
#'
#' 1. For the `lazy` policy, `installed::` packaged get 0 points, binary
#'    packages 1 point, sources packages 5 points.
#' 2. For the 'upgrade' policy, we rank all candidates for a given package
#'    according to their version numbers, and assign more points to older
#'    versions. Points are assigned by 100 and candidates with equal
#'    versions get equal points. We still prefer installed packages to
#'    binaries to source packages, so also add 0 point for already
#'    installed candidates, 1 extra points for binaries and 5 points for
#'    source packages.
#' 3. For directly specified refs, we aim to install each package exactly
#'    once. So for these we require that the variables corresponding to
#'    the same package sum up to 1.
#' 4. For non-direct refs (i.e. dependencies), we require that the
#'    variables corresponding to the same package sum up to at most one.
#'    Since every candidate has at least 1 point in the objective function
#'    of the minimization problem, non-needed dependencies will be
#'    omitted.
#' 5. For direct refs, we require that their candidates satisfy their
#'    references. What this means exactly depends on the ref types. E.g.
#'    for CRAN packages, it means that a CRAN candidate must be selected.
#'    For a standard ref, a GitHub candidate is OK as well.
#' 6. We rule out candidates for which the dependency resolution failed.
#' 7. We go over all the dependency requirements and rule out packages
#'    that do not meet them. For every package `A`, that requires
#'    package `B`, we select the `B(i, i=1..k)` candidates of `B` that
#'    satisfy `A`'s requirements and add a `A - B(1) - ... - B(k) <= 0`
#'    rule. To satisfy this rule, either we cannot install `A`, or if `A`
#'    is installed, then one of the good `B` candidates must be installed
#'    as well.
#' 8. We rule out non-installed CRAN and Bioconductor candidates for
#'    packages that have an already installed candidate with the same exact
#'    version.
#' 9. We also rule out source CRAN and Bioconductor candidates for
#'    packages that have a binary candidate with the same exact version.
#'
#' ## Explaining why the solver failed
#'
#' To be able to explain why a solution attempt failed, we also add a dummy
#' variable for each directly required package. This dummy variable has a
#' very large objective value, and it is only selected if there is no
#' way to install the directly required package.
#'
#' After a failed solution, we look the dummy variables that were selected,
#' to see which directly required package failed to solve. Then we check
#' which rule(s) ruled out the installation of these packages, and their
#' dependencies, recursively.
#'
#' ## The result
#'
#' The result of the solution is a `pkg_solution_result` object. It is a
#' named list with entries:
#'
#' * `status`: Status of the solution attempt, `"OK"` or `"FAILED"`.
#' * `data`: The selected candidates. This is very similar to a
#'   [pkg_resolution_result] object, but it has two extra columns:
#'     * `lib_status`: status of the package in the library, after the
#'        installation. Possible values: `new` (will be newly installed),
#'        `current` (up to date, not installed), `update` (will be updated),
#'        `no-update` (could update, but will not).
#'     * `old_version`: The old (current) version of the package in the
#'        library, or `NA` if the package is currently not installed.
#' * `problem`: The ILP problem. The exact representation is an
#'   implementation detail, but it does have an informative print method.
#' * `solution`: The return value of the internal solver.
#'
#' @name pkg_solution
#' @aliases pkg_solution_result
NULL

solve_dummy_obj <- 1000000000

pkgplan_solve <- function(self, private, policy) {
  "!DEBUG starting to solve `length(private$resolution$packages)` packages"
  if (is.null(private$config$get("library"))) {
    throw(pkg_error(
      # nocov start
      "No package library specified for installation plan.",
      i = "Maybe you need to specify {.code config = list(library = ...)}
       in {.code pkg_installation_plan$new()} or another initializer?"
    )) # nocov end
  }
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) {
    throw(pkg_error(
      # nocov start
      "Package list has changed, you need to call the {.code $resolve()}
       method again?"
    )) # nocov end
  }

  metadata <- list(solution_start = Sys.time())
  pkgs <- self$get_resolution()
  rversion <- private$config$get("r_versions")

  prb <- private$create_lp_problem(pkgs, policy)
  sol <- private$solve_lp_problem(prb)

  if (sol$status != 0) {
    throw(pkg_error(
      # nocov start
      "Error in dependency solver, cannot solve installation.",
      i = "Solver status: {sol$status}.",
      i = msg_internal_error()
    )) # nocov end
  }

  selected <- as.logical(sol$solution[seq_len(nrow(pkgs))])
  res <- list(
    status = if (sol$objval < solve_dummy_obj - 1) "OK" else "FAILED",
    data = private$subset_resolution(selected),
    problem = prb,
    solution = sol
  )

  lib_status <- calculate_lib_status(res$data, pkgs)
  res$data <- as_data_frame(cbind(res$data, lib_status))
  res$data$cache_status <-
    calculate_cache_status(res$data, private$cache)

  metadata$solution_end <- Sys.time()
  attr(res, "metadata") <- modifyList(attr(pkgs, "metadata"), metadata)
  class(res) <- unique(c("pkg_solution_result", class(res)))

  if (res$status == "FAILED") {
    res$failures <- describe_solution_error(pkgs, res)
  }

  private$solution$result <- res
  self$get_solution()
}

pkgplan_stop_for_solve_error <- function(self, private) {
  if (is.null(private$solution)) {
    throw(pkg_error(
      # nocov start
      "You need to call the {.code $solve()} method first."
    )) # nocov end
  }

  sol <- self$get_solution()

  if (sol$status != "OK") {
    msg <- paste(format(sol$failures), collapse = "\n")
    throw(new_error(
      "Could not solve package dependencies:\n",
      msg,
      call. = FALSE
    ))
  }
}

pkgplan__create_lp_problem <- function(self, private, pkgs, policy) {
  pkgplan_i_create_lp_problem(pkgs, private$config, policy)
}

## Add a condition, for a subset of variables, with op and rhs
pkgplan_i_lp_add_cond <- function(
  lp,
  vars,
  op = "<=",
  rhs = 1,
  coef = rep(1, length(vars)),
  type = NA_character_,
  note = NULL
) {
  lp$conds[[length(lp$conds) + 1]] <-
    list(vars = vars, coef = coef, op = op, rhs = rhs, type = type, note = note)
  lp
}

## This is a separate function to make it testable without a `remotes`
## object.
##
## Variables:
## * 1:num are candidates
## * (num+1):(num+num_direct_pkgs) are the relax variables for direct refs

pkgplan_i_create_lp_problem <- function(pkgs, config, policy) {
  "!DEBUG creating LP problem"

  ## TODO: we could already rule out (standard) source packages if binary
  ## with the same version is present

  ## TODO: we could already rule out (standard) source and binary packages
  ## if an installed ref with the same version is present

  rversion <- config$get("r_versions")

  lp <- pkgplan_i_lp_init(pkgs, config, policy)
  lp <- pkgplan_i_lp_objectives(lp)
  lp <- pkgplan_i_lp_os_type(config, lp)
  lp <- pkgplan_i_lp_force_source(lp)
  lp <- pkgplan_i_lp_failures(lp)
  lp <- pkgplan_i_lp_ignore(lp)
  lp <- pkgplan_i_lp_platforms(lp)
  lp <- pkgplan_i_lp_no_multiples(lp)
  lp <- pkgplan_i_lp_rversion(lp, rversion)
  lp <- pkgplan_i_lp_satisfy_direct(lp)
  lp <- pkgplan_i_lp_latest_direct(lp)
  lp <- pkgplan_i_lp_latest_within_repo(lp)
  lp <- pkgplan_i_lp_prefer_installed(lp)
  lp <- pkgplan_i_lp_deduplicate(lp, config)
  lp <- pkgplan_i_lp_prefer_binaries(lp)
  lp <- pkgplan_i_lp_prefer_new_binaries(lp)
  lp <- pkgplan_i_lp_dependencies(lp, config)

  lp
}

pkgplan_i_lp_init <- function(pkgs, config, policy) {
  num_candidates <- nrow(pkgs)
  packages <- unique(pkgs$package)
  direct_packages <- unique(pkgs$package[pkgs$direct])
  indirect_packages <- setdiff(packages, direct_packages)
  num_direct <- length(direct_packages)

  structure(
    list(
      ## Configuration
      config = config,
      ## Number of package candidates
      num_candidates = num_candidates,
      ## Number of directly specified ones
      num_direct = num_direct,
      ## Total number of variables. For direct ones, we have an extra variable
      total = num_candidates + num_direct,
      ## Constraints to fill in
      conds = list(),
      pkgs = pkgs,
      policy = policy,
      ## All package names
      packages = packages,
      ## The names of the direct packages
      direct_packages = direct_packages,
      ## The names of the indirect packages
      indirect_packages = indirect_packages,
      ## Candidates (indices) that have been ruled out. E.g. resolution failed
      ruled_out = integer()
    ),
    class = "pkgplan_lp_problem"
  )
}

## Coefficients of the objective function, this is very easy
## TODO: use rversion as well, for installed and binary packages

pkgplan_i_lp_objectives <- function(lp) {
  pkgs <- lp$pkgs
  policy <- lp$policy
  num_candidates <- lp$num_candidates

  if (policy == "lazy") {
    ## Simple: installed < binary < source
    lp$obj <- ifelse(
      pkgs$type == "installed",
      0,
      ifelse(pkgs$platform == "source", 5, 1)
    )
  } else if (policy == "upgrade") {
    ## Sort the candidates of a package according to version number
    lp$obj <- rep((num_candidates + 1) * 100, num_candidates)
    whpp <- pkgs$status == "OK" & !is.na(pkgs$version)
    pn <- unique(pkgs$package[whpp])
    for (p in pn) {
      whp <- whpp & pkgs$package == p
      v <- pkgs$version[whp]
      r <- rank(package_version(v), ties.method = "min")
      lp$obj[whp] <- (max(r) - r + 1) * 100
      lp$obj[whp] <- lp$obj[whp] - min(lp$obj[whp])
    }
    lp$obj <- lp$obj +
      ifelse(
        pkgs$type == "installed",
        1,
        ifelse(pkgs$platform == "source", 3, 2)
      )
    lp$obj <- lp$obj - min(lp$obj)
  } else {
    throw(pkg_error(
      # nocov start
      "Unknown version selection policy: {.val {policy}}.",
      i = "It has to be one of {.val lazy} or {.val upgrade}."
    )) # nocov end
  }

  lp$obj <- c(lp$obj, rep(solve_dummy_obj, lp$num_direct))

  lp
}

pkgplan_i_lp_os_type <- function(config, lp) {
  if (config$get("goal") != "install") return(lp)
  if (!"os_type" %in% names(lp$pkgs)) return(lp)
  os <- os_type()
  bad <- which(!is.na(lp$pkgs$os_type) & lp$pkgs$os_type != os)
  for (wh in bad) {
    lp <- pkgplan_i_lp_add_cond(
      lp,
      wh,
      op = "==",
      rhs = 0,
      type = "matching-platform"
    )
  }
  lp$ruled_out <- c(lp$ruled_out, bad)

  lp
}

pkgplan_i_lp_force_source <- function(lp) {
  # if source package is forced, then rule out binaries
  src_req <- vlapply(lp$pkgs$params, is_true_param, "source")
  not_src <- lp$pkgs$platform != "source"
  bad <- which(src_req & not_src)
  for (wh in bad) {
    lp <- pkgplan_i_lp_add_cond(
      lp,
      wh,
      op = "==",
      rhs = 0,
      type = "source-required"
    )
  }
  lp$ruled_out <- c(lp$ruled_out, bad)

  lp
}

pkgplan_i_lp_failures <- function(lp) {
  ## 5. Can't install failed resolutions
  failedconds <- function(wh) {
    if (lp$pkgs$status[wh] != "FAILED") return()
    lp <<- pkgplan_i_lp_add_cond(
      lp,
      wh,
      op = "==",
      rhs = 0,
      type = "ok-resolution"
    )
    lp$ruled_out <<- c(lp$ruled_out, wh)
  }
  lapply(seq_len(lp$num_candidates), failedconds)

  lp
}

pkgplan_i_lp_ignore <- function(lp) {
  ignored <- which(vlapply(lp$pkgs$params, is_true_param, "ignore"))
  for (wh in ignored) {
    lp <- pkgplan_i_lp_add_cond(
      lp,
      wh,
      op = "==",
      rhs = 0,
      type = "ignored-by-user"
    )
  }
  lp$ruled_out <- c(lp$ruled_out, ignored)

  lp
}

pkgplan_i_lp_platforms <- function(lp) {
  ## check if platform is good
  badplatform <- function(wh) {
    if (lp$pkgs$type[wh] %in% c("deps", "param")) return()
    ok <- platform_is_ok(
      lp$pkgs$platform[wh],
      lp$config$get("platforms"),
      lp$config$get("windows_archs")
    )
    if (!ok) {
      lp <<- pkgplan_i_lp_add_cond(
        lp,
        wh,
        op = "==",
        rhs = 0,
        type = "matching-platform"
      )
      lp$ruled_out <<- c(lp$ruled_out, wh)
    }
  }
  lapply(seq_len(lp$num_candidates), badplatform)

  lp
}

pkgplan_i_lp_no_multiples <- function(lp) {
  ## 1. Each directly specified package exactly once.
  ##    (We also add a dummy variable to catch errors.)
  for (p in seq_along(lp$direct_packages)) {
    pkg <- lp$direct_packages[p]
    wh <- which(lp$pkgs$package == pkg)
    lp <- pkgplan_i_lp_add_cond(
      lp,
      c(wh, lp$num_candidates + p),
      op = "==",
      type = "exactly-once"
    )
  }

  ## 2. Each non-direct package must be installed at most once
  for (p in seq_along(lp$indirect_packages)) {
    pkg <- lp$indirect_packages[p]
    wh <- which(lp$pkgs$package == pkg)
    lp <- pkgplan_i_lp_add_cond(lp, wh, op = "<=", type = "at-most-once")
  }

  lp
}

pkgplan_i_lp_rversion <- function(lp, rversion) {
  rversion <- package_version(rversion)
  pkgs <- lp$pkgs
  num_candidates <- lp$num_candidates
  ruled_out <- lp$ruled_out
  base <- base_packages()

  depconds <- function(wh) {
    if (pkgs$status[wh] != "OK") return()
    deps <- pkgs$deps[[wh]]
    deps <- deps[deps$ref == "R", , drop = FALSE]
    if (nrow(deps) == 0) return()
    type <- NA
    for (idx in seq_len(nrow(deps))) {
      need <- deps$version[idx]
      needrver <- paste0(deps$op[[idx]], " ", need)
      switch(
        deps$op[[idx]],
        "<" = if (!rversion < need) type <- "new-rversion",
        "<=" = if (!rversion <= need) type <- "new-rversion",
        "==" = if (!rversion == need) type <- "different-rversion",
        ">=" = if (!rversion >= need) type <- "old-rversion",
        ">" = if (!rversion > need) type <- "old-rversion",
        warning(paste0("Ignoring R version requirement: ", needrver))
      )
      # Enough to have one to rule out
      if (!is.na(type)) break
    }
    if (!is.na(type)) {
      lp <<- pkgplan_i_lp_add_cond(
        lp,
        wh,
        op = "==",
        rhs = 0,
        type = type,
        note = needrver
      )
      lp$ruled_out <<- c(lp$ruled_out, wh)
    }
  }

  lapply(setdiff(seq_len(num_candidates), ruled_out), depconds)

  lp
}

pkgplan_i_lp_satisfy_direct <- function(lp) {
  ## 3. Direct refs must be satisfied
  satisfy <- function(wh) {
    pkgname <- lp$pkgs$package[[wh]]
    res <- lp$pkgs[wh, ]
    others <- setdiff(which(lp$pkgs$package == pkgname), wh)
    for (o in others) {
      res2 <- lp$pkgs[o, ]
      if (!isTRUE(satisfies_remote(res, res2))) {
        lp <<- pkgplan_i_lp_add_cond(
          lp,
          o,
          op = "==",
          rhs = 0,
          type = "satisfy-refs",
          note = wh
        )
      }
    }
  }
  direct <- setdiff(which(lp$pkgs$direct), lp$ruled_out)
  lapply(direct, satisfy)

  lp
}

## Order matters. By the time this is called, the failed resolutions
## are ruled out, and R version requirements are also checked.
## So we only work with the packages that are not ruled out, and select
## the latest version. This cannot be ruled out later, only because of
## version requirements, but then it is up to the user to solve this.

pkgplan_i_lp_latest_direct <- function(lp) {
  pkgs <- lp$pkgs
  # these have version requirements
  vreq <- vlapply(
    lp$pkgs$remote,
    function(r) is.list(r) && !is.null(r$version) && r$version != ""
  )
  dirpkgs <- unique(lp$pkgs$package[lp$pkgs$direct & !vreq])
  for (pkg in dirpkgs) {
    cand <- which(
      pkgs$package == pkg &
        pkgs$type %in% c("cran", "bioc", "standard")
    )
    cand <- setdiff(cand, lp$ruled_out)
    if (length(cand) == 0) next
    vers <- package_version(pkgs$version[cand])
    bad <- vers < max(vers)
    for (wh in cand[bad]) {
      lp <- pkgplan_i_lp_add_cond(
        lp,
        wh,
        op = "==",
        rhs = 0,
        type = "direct-update"
      )
    }
    lp$ruled_out <- c(lp$ruled_out, cand[bad])
  }

  lp
}

# CRAN's repo sometimes relies on selecting the latest version of
# a package, if multiple versions are available. (This is after considering
# R version requirements.) So we need to do the same, within repo.
# Otherwise pak/pkgdepends would select the first candidate, and while that
# always (?) OK for CRAN, the order is not the same in RSPM, apparently.

pkgplan_i_lp_latest_within_repo <- function(lp) {
  nbr <- seq_len(nrow(lp$pkgs))
  oid <- ifelse(nbr %in% lp$ruled_out, nbr, 0)
  key <- paste0(
    oid,
    "/",
    lp$pkgs$mirror,
    "/",
    lp$pkgs$repodir,
    "/",
    lp$pkgs$platform,
    "/",
    lp$pkgs$ref
  )
  dups <- unique(key[duplicated(key)])
  for (dupkey in dups) {
    cand <- which(key == dupkey)
    if (length(cand) == 0) next
    vers <- package_version(lp$pkgs$version[cand])
    bad <- vers < max(vers)
    for (wh in cand[bad]) {
      lp <- pkgplan_i_lp_add_cond(
        lp,
        wh,
        op = "==",
        rhs = 0,
        type = "choose-latest"
      )
    }
    lp$ruled_out <- c(lp$ruled_out, cand[bad])
  }

  lp
}

pkgplan_i_lp_prefer_installed <- function(lp) {
  pkgs <- lp$pkgs
  inst <- which(
    pkgs$type == "installed" & !seq_along(pkgs$type) %in% lp$ruled_out
  )
  for (i in inst) {
    ## If not a CRAN or BioC package, skip it
    repotype <- pkgs$extra[[i]]$repotype
    if (is.null(repotype) || !repotype %in% c("cran", "bioc")) next

    ## Look for others with cran/bioc/standard type and same name & ver
    package <- pkgs$package[i]
    version <- pkgs$version[i]

    ruledout <- which(
      pkgs$type %in%
        c("cran", "bioc", "standard") &
        pkgs$package == package &
        pkgs$version == version
    )

    lp$ruled_out <- c(lp$ruled_out, ruledout)
    for (r in ruledout) {
      lp <- pkgplan_i_lp_add_cond(
        lp,
        r,
        op = "==",
        rhs = 0,
        type = "prefer-installed"
      )
    }
  }

  lp
}

# We only do this for source packages, because we already prefer new
# binaries, via `pkgplan_i_lp_prefer_new_binaries()`.
#
# We do this before we prefer binaries, because that rule will rule out
# any source package version that has a binary, and we want to compare all
# source versions here.

pkgplan_i_lp_deduplicate <- function(lp, config) {
  pkgs <- lp$pkgs
  whpp <- pkgs$status == "OK" & !is.na(pkgs$version)
  pn <- unique(pkgs$package[whpp])
  ruled_out <- integer()
  for (p in pn) {
    whp <- which(
      whpp &
        pkgs$package == p &
        pkgs$platform == "source" &
        pkgs$type %in% c("cran", "bioc", "standard")
    )
    whp <- setdiff(whp, lp$ruled_out)
    if (length(whp) <= 1) next
    v <- package_version(pkgs$version[whp])
    mv <- max(v)
    best <- which(v == mv)[1]
    for (i in whp[-best]) {
      if (same_deps(pkgs$deps[[i]], pkgs$deps[[whp[best]]])) {
        ruled_out <- c(ruled_out, i)
      }
    }
  }

  for (r in ruled_out) {
    lp <- pkgplan_i_lp_add_cond(
      lp,
      r,
      op = "==",
      rhs = 0,
      type = "choose-latest"
    )
  }

  lp$ruled_out <- unique(c(lp$ruled_out, ruled_out))
  lp
}

same_deps <- function(d1, d2) {
  d1 <- d1[order(d1$package, d1$type), ]
  rownames(d1) <- NULL
  d2 <- d2[order(d2$package, d2$type), ]
  rownames(d2) <- NULL
  identical(d1, d2)
}

pkgplan_i_lp_prefer_binaries <- function(lp) {
  pkgs <- lp$pkgs
  str <- paste0(pkgs$type, "::", pkgs$package, "@", pkgs$version)
  for (ustr in unique(str)) {
    same <- which(ustr == str)
    ## We can't do this for other packages, because version is not
    ## exclusive for those
    if (!pkgs$type[same[1]] %in% c("cran", "bioc", "standard")) next

    ## TODO: choose the right one for the current R version
    selected <- setdiff(same[pkgs$platform[same] != "source"], lp$ruled_out)[1]
    ## No binary package, maybe there is RSPM. This is temporary,
    ## until we get proper RSPM support.
    ## It would be better to merge the download URLs in this case.
    if (is.na(selected)) {
      selected <- setdiff(
        same[grepl("__linux__", pkgs$mirror[same])],
        lp$ruled_out
      )[1]
    }
    ## Same on Windows, to work around
    ## https://github.com/r-lib/pkgdepends/issues/276
    ## It would be better to merge the download URLs in this case.
    if (is.na(selected)) {
      selected <- setdiff(
        same[grepl(
          "^https://packagemanager[.]rstudio[.]com",
          pkgs$mirror[same]
        )],
        lp$ruled_out
      )[1]
    }
    if (is.na(selected)) next
    ruledout <- setdiff(same, selected)
    lp$ruled_out <- c(lp$ruled_out, ruledout)
    for (r in ruledout) {
      lp <- pkgplan_i_lp_add_cond(
        lp,
        r,
        op = "==",
        rhs = 0,
        type = "prefer-binary"
      )
    }
  }

  lp
}

pkgplan_i_lp_prefer_new_binaries <- function(lp) {
  # We rule out older binaries if there is a new one available
  # This is not always correct, but otherwise the solver will be slow.
  # https://github.com/r-lib/pkgdepends/issues/276
  # I tried adding a penalty to older versions, but that did not work.
  pkgs <- lp$pkgs
  whpp <- pkgs$status == "OK" & !is.na(pkgs$version)
  pn <- unique(pkgs$package[whpp])
  ruled_out <- integer()
  for (p in pn) {
    whp <- which(
      whpp &
        pkgs$package == p &
        pkgs$platform != "source" &
        pkgs$type %in% c("cran", "bioc", "standard")
    )
    whp <- setdiff(whp, lp$ruled_out)
    if (length(whp) == 0) next
    v <- package_version(pkgs$version[whp])
    ruled_out <- c(ruled_out, whp[v != max(v)])
  }

  for (r in ruled_out) {
    lp <- pkgplan_i_lp_add_cond(
      lp,
      r,
      op = "==",
      rhs = 0,
      type = "prefer-new-binary"
    )
  }

  lp$ruled_out <- unique(c(lp$ruled_out, ruled_out))
  lp
}

pkgplan_i_lp_dependencies <- function(lp, config) {
  pkgs <- lp$pkgs
  linkingto <- config$get("include_linkingto")
  num_candidates <- lp$num_candidates
  ruled_out <- lp$ruled_out
  base <- base_packages()

  ignored <- vlapply(pkgs$params, is_true_param, "ignore")
  ignore_rver <- vcapply(pkgs$params, get_param_value, "ignore-before-r")
  if (any(!is.na(ignore_rver))) {
    ignore_rver[is.na(ignore_rver)] <- "0.0.0"
    current <- min(lp$config$get("r_versions"))
    ignored2 <- package_version(ignore_rver) > current
    ignored <- ignored | ignored2
  }
  ignore_unavail <- vlapply(
    pkgs$params,
    is_true_param,
    "ignore-unavailable"
  )
  failed <- pkgs$status == "FAILED"
  ignored <- ignored | (ignore_unavail & failed)

  # ignore packages with the wrong OS type
  if (config$get("goal") == "install") {
    os <- os_type()
    bad <- which(!is.na(pkgs$os_type) & pkgs$os_type != os)
    if (length(bad) > 0) ignored[bad] <- TRUE
  }

  soft_deps <- tolower(pkg_dep_types_soft())

  ## 4. Package dependencies must be satisfied
  depconds <- function(wh) {
    if (pkgs$status[wh] != "OK") return()
    deps <- pkgs$deps[[wh]]
    deptypes <- pkgs$dep_types[[wh]]
    deps <- deps[deps$ref != "R", ]
    deps <- deps[!deps$ref %in% base, ]
    deps <- deps[tolower(deps$type) %in% tolower(deptypes), ]
    if (!linkingto && pkgs$platform[wh] != "source") {
      deps <- deps[tolower(deps$type) != "linkingto", ]
    }
    for (i in seq_len(nrow(deps))) {
      depref <- deps$ref[i]
      depver <- deps$version[i]
      depop <- deps$op[i]
      deppkg <- deps$package[i]
      deptyp <- tolower(deps$type[i])

      # candidates
      res <- pkgs[match(depref, pkgs$ref), ]
      cand <- which(pkgs$package == deppkg)

      # if all candidates are ignored and the package is a soft
      # dependency, then nothing to do
      if (all(ignored[cand]) && deptyp %in% soft_deps) next

      # good candidates
      good_cand <- Filter(
        x = cand,
        function(c) {
          candver <- pkgs$version[c]
          pkgs$status[[c]] != "FAILED" &&
            isTRUE(satisfies_remote(res, pkgs[c, ])) &&
            (depver == "" || version_satisfies(candver, depop, depver))
        }
      )
      bad_cand <- setdiff(cand, good_cand)

      report <- c(
        if (length(good_cand)) {
          gc <- paste(pkgs$ref[good_cand], pkgs$version[good_cand])
          paste0("version ", paste(gc, collapse = ", "))
        },
        if (length(bad_cand)) {
          bc <- paste(pkgs$ref[bad_cand], pkgs$version[bad_cand])
          paste0("but not ", paste(bc, collapse = ", "))
        },
        if (!length(cand)) "but no candidates"
      )
      txt <- sprintf(
        "%s depends on %s: %s",
        pkgs$ref[wh],
        depref,
        collapse(report, sep = ", ")
      )
      note <- list(
        wh = wh,
        ref = depref,
        cand = cand,
        good_cand = good_cand,
        txt = txt,
        depop = depop,
        depver = depver
      )

      lp <<- pkgplan_i_lp_add_cond(
        lp,
        c(wh, good_cand),
        "<=",
        rhs = 0,
        coef = c(1, rep(-1, length(good_cand))),
        type = "dependency",
        note = note
      )
    }
  }
  lapply(setdiff(seq_len(num_candidates), ruled_out), depconds)

  lp
}

#' @export

print.pkgplan_lp_problem <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

format_cond <- function(x, cond) {
  if (cond$type == "dependency") {
    paste0(cond$note$txt)
  } else if (cond$type == "satisfy-refs") {
    ref <- x$pkgs$ref[cond$note]
    cand <- x$pkgs$ref[cond$vars]
    sprintf("`%s` is not satisfied by `%s`", ref, cand)
  } else if (cond$type == "ok-resolution") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("`%s` resolution failed", ref)
  } else if (cond$type == "source-required") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("a source package was required for `%s` by the user", ref)
  } else if (cond$type == "ignored-by-user") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("`%s` explicitly ignored by user", ref)
  } else if (cond$type == "matching-platform") {
    ref <- x$pkgs$ref[cond$vars]
    plat <- x$pkgs$platform[cond$vars]
    sprintf("Platform `%s` does not match for `%s`", plat, ref)
  } else if (cond$type == "old-rversion") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("`%s` needs a newer R version: %s", ref, cond$note)
  } else if (cond$type == "new-rversion") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("`%s` needs an older R version: %s", ref, cond$node)
  } else if (cond$type == "different-rversion") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("`%s` needs a different R version: %s", ref, cond$note)
  } else if (cond$type == "direct-update") {
    package <- x$pkgs$package[cond$vars]
    sprintf("`%s` is direct, needs latest version", package)
  } else if (cond$type == "choose-latest") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("`%s` has a newer version of the same platform", ref)
  } else if (cond$type == "prefer-installed") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("installed is preferred for `%s`", ref)
  } else if (cond$type == "prefer-binary") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("binary is preferred for `%s`", ref)
  } else if (cond$type == "prefer-new-binary") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("newer binary is preferred for `%s`", ref)
  } else if (cond$type == "source-requested") {
    ref <- x$pkgs$ref[cond$vars]
    sprintf("source package is requested for `%s`", ref)
  } else if (cond$type == "exactly-once") {
    ref <- na.omit(x$pkgs$package[cond$vars])[1]
    sprintf("select %s exactly once", ref)
  } else if (cond$type == "at-most-once") {
    ref <- na.omit(x$pkgs$package[cond$vars])[1]
    sprintf("select %s at most once", ref)
  } else {
    "Unknown constraint"
  }
}

#' @export

format.pkgplan_lp_problem <- function(x, ...) {
  result <- character()
  push <- function(...) result <<- c(result, ...)

  push("<pkgplan_lp_problem>")
  push(sprintf("+ refs (%s):", x$num_candidates))
  pn <- sort(x$pkgs$ref)
  push(paste0("  - ", x$pkgs$ref))

  if (length(x$conds)) {
    push(sprintf("+ constraints (%s):", length(x$conds)))
    conds <- drop_nulls(lapply(x$conds, format_cond, x = x))
    push(paste0("  - ", conds))
  } else {
    push("+ no constraints")
  }

  result
}

pkgplan__solve_lp_problem <- function(self, private, problem) {
  res <- pkgplan_i_solve_lp_problem(problem)
  res
}

pkgplan_i_solve_lp_problem <- function(problem) {
  "!DEBUG solving LP problem"
  condmat <- matrix(0, nrow = length(problem$conds), ncol = problem$total)
  for (i in seq_along(problem$conds)) {
    cond <- problem$conds[[i]]
    condmat[i, cond$vars] <- cond$coef
  }

  dir <- vcapply(problem$conds, "[[", "op")
  rhs <- vapply(problem$conds, "[[", "rhs", FUN.VALUE = double(1))
  lpSolve::lp(
    "min",
    problem$obj,
    condmat,
    dir,
    rhs,
    int.vec = seq_len(problem$total)
  )
}

pkgplan_get_solution <- function(self, private) {
  if (is.null(private$solution)) {
    throw(pkg_error(
      "You need to call the {.code $solve()} method first."
    ))
  }
  private$solution$result
}

#' Highlight version number changes
#'
#' @param old Character vector, old versions. `NA` for new installs.
#' @param new Character vector, the new versions to highlight.
#' @return Character vector, like `new`, but the change highlighted
#'
#' @noRd

highlight_version <- function(old, new) {
  if (length(old) != length(new)) {
    throw(pkg_error(
      "Lengtgs of `old` and `new` must match",
      i = msg_internal_error()
    ))
  }
  if (length(new) == 0) return(new)

  wch <- !is.na(old) & old != new

  oldv <- strsplit(old[wch], "(?=[.-])", perl = TRUE)
  newv <- strsplit(new[wch], "(?=[.-])", perl = TRUE)
  new[wch] <- as.character(mapply(oldv, newv, FUN = function(o, n) {
    length(o) <- length(n) <- max(length(o), length(n))
    idx <- which(is.na(o) | is.na(n) | (o != n & o != "." & o != "-"))[1]
    n <- na.omit(n)
    paste0(
      if (idx > 1) paste(n[1:(idx - 1)], collapse = ""),
      if (idx <= length(n))
        cli::style_bold(paste(n[idx:length(n)]), collapse = "")
    )
  }))

  new
}

#' Highlight package list
#'
#' @param sol Solution data, data frame, with at least these columns:
#' `type`, `package`, `old_version`, `version`, `lib_status`,
#' `cache_status`, `platform`, `needscompilation`. Just what
#' `$get_solution()$data` returns, basically.
#' @return Character vector of highlighted list. All strings will have the
#' same (printed) length. Packages that do not involve installation will
#' have `NA` in the result.
#'
#' @noRd

highlight_package_list <- function(sol) {
  arrow <- cli::symbol$arrow_right

  ins <- sol$type != "installed" & sol$type != "deps"
  sol <- sol[ins, ]

  pkg <- ansi_align_width(cli::col_blue(sol$package))
  old <- ansi_align_width(ifelse(is.na(sol$old_version), "", sol$old_version))
  arr <- ansi_align_width(ifelse(is.na(sol$old_version), "", arrow))
  new <- ansi_align_width(highlight_version(sol$old_version, sol$version))

  bld <- sol$lib_status %in% c("new", "update") & sol$platform == "source"
  cmp <- sol$lib_status %in%
    c("new", "update") &
    !is.na(sol$needscompilation) &
    sol$needscompilation
  dnl <- !is.na(sol$cache_status) & sol$cache_status == "miss"

  gh <- sol$type == "github"
  hash <- character(nrow(sol))
  hash[gh] <- vcapply(sol$metadata[gh], function(x) x["RemoteSha"])

  sysreqs <- highlight_sysreqs(sol$sysreqs_packages)

  ann <- paste0(
    ifelse(
      bld,
      if (has_emoji()) emo_builder(sum(ins)) else emoji("builder"),
      ""
    ),
    ifelse(cmp, emoji("wrench"), ""),
    ifelse(dnl, emoji("dl"), ""),
    ifelse(
      dnl & !is.na(sol$filesize),
      paste0(" ", format_file_size(sol$filesize)),
      ""
    ),
    ifelse(gh, paste0(" (GitHub: ", substr(hash, 1, 7), ")"), ""),
    sysreqs
  )

  lns <- paste0(pkg, " ", old, " ", arr, " ", new, " ", ann)

  ret <- rep(NA_character_, length(ins))
  ret[ins] <- lns

  key <- paste0(
    c(
      if (any(bld)) paste(emoji("builder"), "build"),
      if (any(cmp)) paste(emoji("wrench"), "compile"),
      if (any(dnl)) paste(emoji("dl"), "download")
    ),
    collapse = " | "
  )

  attr(ret, "key") <- if (key == "") "" else paste("[", key, "]")
  ret
}

highlight_sysreqs <- function(sysreqs) {
  if (is.null(sysreqs)) return("")
  vcapply(sysreqs, function(p) {
    if (length(p) == 0) return("")
    tick <- paste0(cli::col_green(cli::symbol$tick), " ")
    cross <- paste0(cli::col_red(cli::symbol$cross), " ")
    pkgs <- unlist(lapply(p, function(x) {
      if (length(x$packages) != 0) {
        if ("packages_missing" %in% names(x)) {
          paste0(
            ifelse(x$packages %in% x$packages_missing, cross, tick),
            x$packages
          )
        } else {
          x$packages
        }
      } else {
        paste0(x$sysreq, " (installer)")
      }
    }))
    if (length(pkgs) == 0) return("") # nocov
    paste0(
      cli::col_silver(" + "),
      paste(cli::col_cyan(pkgs), collapse = ", ")
    )
  })
}

pkgplan_show_solution <- function(self, private, key = FALSE) {
  self$stop_for_solve_error()
  sol <- self$get_solution()$data
  sol <- sol[order(sol$package), ]

  hl <- highlight_package_list(sol)
  hl2 <- na.omit(hl)

  if (length(hl2)) {
    hl2 <- paste0(cli::col_silver("+ "), hl2)
    if (key && attr(hl, "key") != "") hl2 <- c(hl2, " ", attr(hl, "key"))
    out <- paste(hl2, collapse = "\n")
    cli::cli_verbatim(hl2)
  }

  invisible(self$get_solution())
}

categorize_sysreqs <- function(rpkgs) {
  rpkgs <- rpkgs[vlapply(rpkgs$sysreqs_packages, function(x) length(x) > 0), ]

  inst <- structure(list(), names = character())
  miss <- structure(list(), names = character())
  upd <- structure(list(), names = character())

  for (i in seq_along(rpkgs$sysreqs_packages)) {
    elt <- rpkgs$sysreqs_packages[[i]]
    pkg <- rpkgs$package[i]
    for (j in seq_along(elt)) {
      if ("packages_missing" %in% names(elt[[j]])) {
        # we know exactly what is missing
        miss1 <- elt[[j]][["packages_missing"]]
        inst1 <- setdiff(elt[[j]][["packages"]], miss)
        upd1 <- character()
      } else {
        # we don't know what is missing
        upd1 <- elt[[j]][["packages"]]
        inst1 <- miss1 <- character()
      }

      for (p in miss1) miss[[p]] <- c(miss[[p]], pkg)
      for (p in inst1) inst[[p]] <- c(inst[[p]], pkg)
      for (p in upd1) upd[[p]] <- c(upd[[p]], pkg)
    }
  }

  list(inst = inst, miss = miss, upd = upd)
}

pkgplan_get_sysreqs <- function(self, private) {
  categorize_sysreqs(self$get_solution()[["data"]])
}

pkgplan_show_sysreqs <- function(self, private) {
  rpkgs <- self$get_solution()[["data"]]
  if (is.null(rpkgs[["sysreqs_packages"]])) return(invisible())
  cats <- categorize_sysreqs(rpkgs)
  inst <- cats$inst
  miss <- cats$miss
  upd <- cats$upd

  col1 <- col2 <- character()
  if (length(miss)) {
    miss <- miss[order(tolower(names(miss)))]
    col1 <- paste0(cli::col_silver("+ "), cli::col_cyan(names(miss)))
    col2 <- paste0(
      cli::col_silver("- "),
      vcapply(miss, function(x) paste(cli::col_blue(x), collapse = ", "))
    )
  }

  if (length(upd)) {
    upd <- upd[order(tolower(names(upd)))]
    col1 <- c(col1, paste0(cli::col_silver("* "), cli::col_cyan(names(upd))))
    col2 <- c(
      col2,
      paste0(
        cli::col_silver("- "),
        vcapply(upd, function(x) paste(cli::col_blue(x), collapse = ", "))
      )
    )
  }

  col1 <- ansi_align_width(col1)
  col2 <- ansi_align_width(col2)
  out <- paste0(col1, "  ", col2)
  if (length(out)) cli::cli_verbatim(paste(out, collapse = "\n"))

  invisible(cats)
}

pkgplan_install_plan <- function(self, private, downloads) {
  "!DEBUG creating install plan"
  sol <- if (downloads) {
    self$get_solution_download()
  } else {
    self$stop_for_solve_error()
    self$get_solution()$data
  }
  if (inherits(sol, "pkgplan_solve_error")) return(sol)

  # If it is coming from an install plan, then there is no 'deps' column,
  # but the dependencies column is already set.
  has_deps <- "deps" %in% names(sol)
  if (has_deps) {
    linkingto <- private$config$get("include_linkingto")
    if ("dep_types" %in% names(sol)) {
      selected_deps <- lapply(sol$dep_types, intersect, pkg_dep_types_hard())
    } else {
      selected_deps <- list(pkg_dep_types_hard())
    }
    deps <- lapply(
      seq_len(nrow(sol)),
      function(i) {
        x <- sol$deps[[i]]
        if (!linkingto && sol$platform[[i]] != "source") {
          x <- x[tolower(x$type) != "linkingto", ]
        }

        mydeps <- if (length(selected_deps) > 1) {
          selected_deps[[i]]
        } else {
          selected_deps[[1]]
        }
        pkgs <- x$package[tolower(x$type) %in% tolower(mydeps)]
        pkgs <- intersect(pkgs, sol$package)
        # Yes, some packages Suggest themselves, apparently
        setdiff(pkgs, sol$package[[i]])
      }
    )
    deps <- lapply(deps, setdiff, y = c("R", base_packages()))
  }

  installed <- ifelse(
    sol$type == "installed",
    file.path(private$config$get("library"), sol$package),
    NA_character_
  )

  res <- self$get_resolution()
  direct_packages <- res$package[res$direct]
  direct <- sol$direct |
    (sol$type == "installed" & sol$package %in% direct_packages)

  binary <- sol$platform != "source"
  if (downloads) {
    had <- paste("Had", current_r_platform())
    binary <- binary | sol$download_status == had
  }
  vignettes <- !binary &
    !sol$type %in% c("cran", "bioc", "standard") &
    private$config$get("build-vignettes")

  sol$library <- private$config$get("library")
  sol$binary <- binary
  sol$direct <- direct
  if (has_deps) sol$dependencies <- I(deps)
  sol$installed <- installed
  sol$vignettes <- vignettes

  # If we are on X64 Windows and we prefer x64, then only compile
  # for x64
  nomulti <- sol$platform %in%
    c("*", "source") &
    sol$type != "installed" &
    "x86_64-w64-mingw32" %in% private$config$get("platforms") &
    private$config$get("windows-archs") == "prefer-x64"
  sol$install_args <- ifelse(nomulti, "--no-multiarch", "")

  if (downloads) {
    tree <- file.exists(sol$fulltarget_tree)
    sol$packaged <- !tree
    sol$file <- ifelse(tree, sol$fulltarget_tree, sol$fulltarget)
  }

  sol
}

pkgplan_export_install_plan <- function(self, private, plan_file, version) {
  pkgs <- pkgplan_install_plan(self, private, downloads = FALSE)
  cols <- unique(c(
    "ref",
    "package",
    "version",
    "type",
    "direct",
    "binary",
    "dependencies",
    "vignettes",
    "needscompilation",
    "metadata",
    "sources",
    "target",
    "platform",
    "rversion",
    "built",
    "directpkg",
    "license",
    "sha256",
    "filesize",
    "dep_types",
    "params",
    "install_args",
    "repotype"
  ))

  packages <- pkgs[, cols]
  if ("sysreqs" %in% names(pkgs)) packages[["sysreqs"]] <- pkgs[["sysreqs"]]

  # drop missing system packages from sysreqs, we don't want these in
  # the lock file
  if ("sysreqs_packages" %in% names(pkgs)) {
    spkgs <- pkgs[["sysreqs_packages"]]
    for (i in seq_along(spkgs)) {
      elt <- spkgs[[i]]
      for (j in seq_along(elt)) {
        elt[[j]]$sysreq <- tojson$unbox(elt[[j]]$sysreq)
        elt[[j]]$packages_missing <- NULL
      }
      if (!is.null(elt)) spkgs[[i]] <- elt
    }
    packages[["sysreqs_packages"]] <- spkgs
  }

  packages$params <- lapply(
    packages$params,
    function(x) lapply(as.list(x), tojson$unbox)
  )

  plan <- list(
    lockfile_version = tojson$unbox(version),
    os = tojson$unbox(utils::sessionInfo()$running),
    r_version = tojson$unbox(R.Version()$version.string),
    platform = tojson$unbox(R.Version()$platform),
    packages = packages
  )

  if (private$config$get("sysreqs")) {
    sysreqs <- sysreqs2_scripts(
      self$get_solution()$data$sysreqs_packages,
      private$config$get("sysreqs_platform")
    )
    sysreqs$os <- tojson$unbox(sysreqs$os)
    sysreqs$distribution <- tojson$unbox(sysreqs$distribution)
    sysreqs$version <- tojson$unbox(sysreqs$version)
    sysreqs$url <- tojson$unbox(sysreqs$url)
    sysreqs$total <- NULL
    plan$sysreqs <- sysreqs
  }

  txt <- as_json_lite_plan(plan)
  writeLines(txt, plan_file)
}

as_json_lite_plan <- function(liteplan) {
  tolist1 <- function(x) lapply(x, function(v) lapply(as.list(v), tojson$unbox))
  liteplan$packages$metadata <- tolist1(liteplan$packages$metadata)
  json <- tojson$write_str(liteplan, opts = list(pretty = TRUE))
  json
}

calculate_lib_status <- function(sol, res) {
  ## Possible values at the moment:
  ## - virtual: not really a package
  ## - new: newly installed
  ## - current: up to date, not installed
  ## - update: will be updated
  ## - no-update: could update, but won't

  sres <- res[res$package %in% sol$package, c("package", "version", "type")]

  ## Check if it is not new
  lib_ver <- vcapply(sol$package, function(p) {
    c(
      sres$version[sres$package == p & sres$type == "installed"],
      NA_character_
    )[1]
  })

  ## If not new, and not "installed" type, that means update
  status <- ifelse(
    sol$type == "deps",
    "virtual",
    ifelse(
      is.na(lib_ver),
      "new",
      ifelse(sol$type == "installed", "current", "update")
    )
  )

  # Return NA for empty sets
  mmax <- function(...) c(suppressWarnings(max(...)), NA)[1]

  ## Check for no-update
  new_version <- vcapply(seq_along(sol$package), function(i) {
    p <- sol$package[i]
    v <- if (is.na(sol$version[i])) NA_character_ else
      package_version(sol$version[i])
    g <- sres$package == p & !is.na(sres$version)
    mmax(sres$version[g][v < sres$version[g]])
  })
  status[status == "current" & !is.na(new_version)] <- "no-update"

  data_frame(
    lib_status = status,
    old_version = lib_ver,
    new_version = new_version
  )
}

## TODO: non-CRAN packages? E.g. GH based on sha.

calculate_cache_status <- function(soldata, cache) {
  toinst <- soldata$sha256[soldata$type != "installed"]
  nocache <- vlapply(soldata$params, is_true_param, "nocache")
  cached <- cache$package$find(sha256 = toinst)
  ifelse(
    soldata$type == "installed",
    NA_character_,
    ifelse(soldata$sha256 %in% cached$sha256 & !nocache, "hit", "miss")
  )
}

describe_solution_error <- function(pkgs, solution) {
  assert_that(
    !is.null(pkgs),
    !is.null(solution),
    solution$solution$objval >= solve_dummy_obj - 1L
  )

  num <- nrow(pkgs)
  if (!num) {
    throw(pkg_error(
      "No solution errors to describe",
      i = msg_internal_error()
    ))
  }
  sol <- solution$solution$solution
  sol_pkg <- sol[1:num]
  sol_dum <- sol[(num + 1):solution$problem$total]

  ## For each candidate, we work out if it _could_ be installed, and if
  ## not, why not. Possible cases:
  ## 1. it is in the install plan, so it can be installed, YES
  ## 2. it failed resolution, so NO
  ## 3. it needs a newer R version, so NO
  ## 4. it does not satisfy a direct ref for the same package, so NO
  ## 5. it conflicts with another to-be-installed candidate of the
  ##    same package, so NO
  ## 6. one of its (downstream) dependencies cannot be installed, so NO
  ## 7. otherwise YES

  FAILS <- c(
    "failed-res",
    "satisfy-direct",
    "conflict",
    "dep-failed",
    "old-rversion",
    "new-rvresion",
    "different-rversion",
    "matching-platform",
    "ignored-by-user",
    "binary-preferred",
    "source-required",
    "installed-preferred"
  )

  state <- rep("maybe-good", num)
  note <- replicate(num, NULL)
  downstream <- replicate(num, character(), simplify = FALSE)

  ## If a candidate of a package is OK, then we should not report errors
  ## about other candidates.
  ok_pkgs <- unique(pkgs$package[sol_pkg == 1])
  state[pkgs$package %in% ok_pkgs] <- "installed"

  cnd <- solution$problem$conds
  typ <- vcapply(cnd, "[[", "type")
  var <- lapply(cnd, "[[", "vars")

  ## Ignored by user
  ign_vars <- unlist(var[typ == "ignored-by-user"])
  ign_vars <- intersect(ign_vars, which(state == "maybe-good"))
  state[ign_vars] <- "ignored-by-user"

  ## Source required
  ign_vars <- unlist(var[typ == "source-required"])
  ign_vars <- intersect(ign_vars, which(state == "maybe-good"))
  state[ign_vars] <- "source-required"

  ## Ruled out in favor of an installed package
  ins_vars <- unlist(var[typ == "prefer-installed"])
  ins_vars <- intersect(ins_vars, which(state == "maybe-good"))
  state[ins_vars] <- "installed-preferred"

  ## Ruled out in favor of a binary package
  bin_vars <- unlist(var[typ %in% c("prefer-binary", "prefer-new-binary")])
  bin_vars <- intersect(bin_vars, which(state == "maybe-good"))
  state[bin_vars] <- "binary-preferred"

  ## Candidates that failed resolution
  fres_vars <- unlist(var[typ == "ok-resolution"])
  fres_vars <- intersect(fres_vars, which(state == "maybe-good"))
  state[fres_vars] <- "failed-res"
  for (fv in fres_vars) {
    if (length(e <- pkgs$error[[fv]])) {
      note[[fv]] <- c(note[[fv]], conditionMessage(e))
    }
  }

  ## Candidates that need a newer/older/different R version
  for (w in which(
    typ %in% c("old-rversion", "new-rversion", "different-rversion")
  )) {
    sv <- var[[w]]
    if (state[sv] != "maybe-good") next
    needs <- cnd[[w]]$note
    state[sv] <- typ[[w]]
    note[[sv]] <- c(note[[sv]], sprintf("Needs R %s", needs))
  }

  ## Candidates with platform mismatch
  for (w in which(typ == "matching-platform")) {
    sv <- var[[w]]
    if (state[sv] != "maybe-good") next
    state[sv] <- "matching-platform"
    note[[sv]] <- c(note[[sv]], "Platform mismatch")
  }

  ## Candidates that conflict with a direct package
  for (w in which(typ == "satisfy-refs")) {
    sv <- var[[w]]
    down <- pkgs$ref[sv]
    up <- pkgs$ref[cnd[[w]]$note]
    state[sv] <- "satisfy-direct"
    note[[sv]] <- c(note[[sv]], sprintf("Conflicts with %s", up))
  }

  ## Find "conflict". These are candidates that are not installed,
  ## and have an "at-most-once" constraint with another package that will
  ## be installed. So we just go over these constraints.
  for (c in cnd[typ == "at-most-once"]) {
    is_in <- sol_pkg[c$vars] != 0
    if (any(is_in)) {
      state[c$vars[!is_in]] <- "conflict"
      package <- pkgs$package[c$vars[1]]
      inst <- pkgs$ref[c$vars[is_in]]
      vv <- c$vars[!is_in]
      for (v in vv) {
        note[[v]] <- c(
          note[[v]],
          sprintf("%s conflicts with %s, to be installed", pkgs$ref[v], inst)
        )
      }
    }
  }

  ## Find "dep-failed". This is the trickiest. First, if there are no
  ## condidates at all
  type_dep <- typ == "dependency"
  dep_up <- viapply(cnd[type_dep], function(x) x$vars[1])
  dep_cands <- lapply(cnd[type_dep], function(x) x$vars[-1])
  no_cands <- which(
    !viapply(dep_cands, length) &
      state[dep_up] == "maybe-good"
  )
  for (x in no_cands) {
    pkg <- cnd[type_dep][[x]]$note$ref
    state[dep_up[x]] <- "dep-failed"
    note[[dep_up[x]]] <-
      c(note[[dep_up[x]]], sprintf("Can't install dependency %s", pkg))
    depop <- cnd[type_dep][[x]]$note$depop %||% ""
    depver <- cnd[type_dep][[x]]$note$depver %||% ""
    if (nzchar(depver)) {
      note[[dep_up[x]]] <- paste0(
        note[[dep_up[x]]],
        " (",
        depop,
        " ",
        depver,
        ")"
      )
    }
    downstream[[dep_up[x]]] <- c(downstream[[dep_up[x]]], pkg)
  }

  ## Then we start with the already known
  ## NO answers, and see if they rule out upstream packages
  new <- which(state %in% FAILS)
  while (length(new)) {
    dep_cands <- lapply(dep_cands, setdiff, new)
    which_new <- which(
      !viapply(dep_cands, length) & state[dep_up] == "maybe-good"
    )
    for (x in which_new) {
      pkg <- cnd[type_dep][[x]]$note$ref
      state[dep_up[x]] <- "dep-failed"
      note[[dep_up[x]]] <- c(
        note[[dep_up[x]]],
        sprintf("Can't install dependency %s", pkg)
      )
      depop <- cnd[type_dep][[x]]$note$depop %||% ""
      depver <- cnd[type_dep][[x]]$note$depver %||% ""
      if (nzchar(depver)) {
        note[[dep_up[x]]] <- paste0(
          note[[dep_up[x]]],
          " (",
          depop,
          " ",
          depver,
          ")"
        )
      }
      downstream[[dep_up[x]]] <- c(downstream[[dep_up[x]]], pkg)
    }
    new <- dep_up[which_new]
  }

  ## The rest is good
  state[state == "maybe-good"] <- "could-be"

  wh <- state %in% FAILS | (pkgs$direct & sol_pkg == 0)
  fails <- pkgs[wh, ]
  fails$failure_type <- state[wh]
  fails$failure_message <- note[wh]
  fails$failure_down <- downstream[wh]
  class(fails) <- unique(c("pkg_solution_failures", class(fails)))

  fails
}

#' @export

print.pkg_solution_failures <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.pkg_solution_failures <- function(x, ...) {
  fails <- x
  if (!nrow(fails)) return()

  done <- rep(FALSE, nrow(x))
  res <- character()

  do <- function(i) {
    if (done[i]) return()
    if (
      fails$failure_type[i] %in% c("installed-preferred", "binary-preferred")
    ) {
      return()
    }
    done[i] <<- TRUE
    msgs <- unique(fails$failure_message[[i]])

    fail <- paste0("* ", cli::style_bold(fails$ref[i]), ":")
    if (length(msgs) == 0) {
      fail <- paste0(fail, " ", "dependency conflict")
    } else if (length(msgs) == 1) {
      fail <- paste0(fail, " ", msgs)
    } else if (length(msgs) > 1) {
      fail <- paste0(fail, "\n", paste0("  * ", msgs, collapse = "\n"))
    }

    res <<- c(res, fail)
    down <- which(
      fails$package %in%
        fails$failure_down[[i]] |
        fails$ref %in% fails$failure_down[[i]]
    )
    lapply(down, do)
  }

  direct_refs <- which(fails$direct)
  lapply(direct_refs, do)

  unique(res)
}

#' @export

`[.pkg_solution_result` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pkg_solution_result")
  NextMethod("[")
}

#' @export

format.pkg_solution_result <- function(x, ...) {
  ok <- is.null(x$failures) || nrow(x$failures) == 0
  refs <- sort(unique(x$problem$pkgs$ref[x$problem$pkgs$direct]))
  nc <- length(x$problem$conds)
  cnst <- unlist(lapply(x$problem$conds, format_cond, x = x$problem))
  solrefs <- sort(x$data$ref)
  c(
    "<pkg_solution>",
    paste0("+ result: ", if (ok) "OK" else "FAILED"),
    "+ refs:",
    paste0("  - ", refs),
    if (nc == 0) "+ no constraints",
    if (nc > 0) paste0("+ constraints (", length(cnst), "):"),
    if (nc > 0) paste0("  - ", utils::head(cnst, 10)),
    if (nc > 10) "  ...",
    if (ok) c("+ solution:", paste0("  - ", solrefs)),
    if (!ok) c("x failures:", format(x$failures))
  )
}

#' @export

print.pkg_solution_result <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
