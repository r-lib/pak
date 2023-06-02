
#' Simplified manual. Start here!
#'
#' @name Get started with pak
#' @rdname get-started
#' @description
#' You don't need to read long manual pages for a simple task.
#' This manual page collects the most common pak use cases.
#'
#' ```{r child = "man/chunks/tldr.Rmd"}
#' ```
NULL

#' Frequently Asked Questions
#'
#' @name FAQ
#' @rdname faq
#' @description
#' Please take a look at this list before asking questions.
#'
#' ```{r child = "man/chunks/FAQ.Rmd"}
#' ```
NULL

#' A list of the most important pak features
#'
#' @name Great pak features
#' @rdname features
#' @description
#' A list of the most important pak features.
#'
#' ```{r child = "man/chunks/features.Rmd"}
#' ```
NULL

#' Install packages from CRAN, Bioconductor, GitHub, URLs, etc.
#'
#' @description
#' Install packages from CRAN, Bioconductor, GitHub, URLs, etc.
#' Learn how to tell pak which packages to install, and where those packages
#' can be found.
#'
#' If you want a quick overview of package sources, see
#' "[Get started with pak]".
#'
#' @details
#' \eval{include_docs("pkgdepends", "docs/pkg-refs.rds", top = FALSE)}
#'
#' @name Package sources
#' @rdname pak_package_sources
NULL

#' Find the ideal set of packages and versions to install
#'
#' pak contains a package dependency solver, that makes sure that the
#' package source and version requirements of all packages are satisfied,
#' before starting an installation. For CRAN and BioC packages this is
#' usually automatic, because these repositories are generally in a
#' consistent state. If packages depend on other other package sources,
#' however, this is not the case.
#'
#' Here is an example of a conflict detected:
#' ```
#' > pak::pkg_install(c("r-lib/pkgcache@conflict", "r-lib/cli@message"))
#' Error: Cannot install packages:
#'   * Cannot install `r-lib/pkgcache@conflict`.
#'     - Cannot install dependency r-lib/cli@main
#'   * Cannot install `r-lib/cli@main`.
#' - Conflicts r-lib/cli@message
#' ```
#'
#' `r-lib/pkgcache@conflict` depends on the main branch of `r-lib/cli`,
#' whereas, we explicitly requested the `message` branch. Since it cannot
#' install both versions into a single library, pak quits.
#'
#' When pak considers a package for installation, and the package is given
#' with its name only, (e.g. as a dependency of another package), then
#' the package may have _any_ package source. This is necessary, because
#' one R package library may contain only at most one version of a package
#' with a given name.
#'
#' pak's behavior is best explained via an example.
#' Assume that you are installing a local package (see below), e.g.
#' `local::.`, and the local package depends on `pkgA` and `user/pkgB`,
#' the latter being a package from GitHub (see below), and that `pkgA`
#' also depends on `pkgB`. Now pak must install `pkgB` _and_ `user/pkgB`.
#' In this case pak interprets `pkgB` as a package from any package source,
#' instead of a standard package, so installing `user/pkgB` satisfies both
#' requirements.
#'
#' Note that that `cran::pkgB` and `user/pkgB` requirements result a
#' conflict that pak cannot resolve. This is because the first one _must_
#' be a CRAN package, and the second one _must_ be a GitHub package, and
#' two different packages with the same cannot be installed into an R
#' package library.
#'
#' @name The dependency solver
#' @rdname pak_solver
NULL

#' All about installing pak.
#'
#' @name Installing pak
#' @rdname install
#' @description
#' Read this if the default installation methods do not work for you or
#' if you want the release candidate or development version.
#'
#' ```{r child = "man/chunks/install.Rmd"}
#' ```
NULL

#' Various types of R package dependencies
#'
#' @details
#' \eval{include_docs("pkgdepends", "docs/deps.rds")}
#'
#' @name Package dependency types
#' @rdname package-dependency-types
NULL

#' Environment variables and options that modify the default behavior
#'
#' @name pak configuration
#' @rdname pak-config
#' @aliases pak-config
#'
#' @description
#' pak behavior can be finetuned with environment variables and
#' options (as in [base::options()]).
#'
#' @details
#' # R options affecting pak's behavior
#'
#' ## `Ncpus`
#'
#' Set to the desired number of worker processes for package installation.
#' If not set, then pak will use the number of logical processors in the
#' machine.
#'
#' ## `repos`
#'
#' The CRAN-like repositories to use. See [base::options()] for details.
#'
#' # pak configuration
#'
#' Configuration entries (unless noted otherwise on this manual page)
#' have a corresponding environment variable, and a corresponding option.
#'
#' The environment variable is always uppercase and uses underscores as the
#' word separator. It always has the `PKG_` prefix.
#'
#' The option is typically lowercase, use it uses underscores as the word
#' separator, but it always has the `pkg.` prefix (notice the dot!).
#'
#' Some examples:
#'
#' | Config entry name  | Env var name      |  Option name      |
#' |:-------------------|:------------------|:------------------|
#' | platforms          | `PKG_PLATFORMS`   | `pkg.platforms`   |
#' | cran_mirror        | `PKG_CRAN_MIRROR` | `pkg.cran_mirror` |
#'
#' ## pak configuration entries
#'
#' \eval{doc_config()}
#'
#' ## Notes
#'
#' From version 0.4.0 pak copies the `PKG_*` environment variables and
#' the `pkg.*` options to the pak subprocess, where they are actually
#' used, so you don't need to restart R or reaload pak after a
#' configuration change.
NULL

doc_config <- function() {
  lib <- private_lib_dir()
  rds <- file.path(lib, "pkgdepends", "docs", "pak-config-docs.rds")
  if (!file.exists(rds)) {
    return("Cannot look up documentation in pkgdepends. :(")
  }

  rd <- readRDS(rds)
  paste0(
    "\\itemize{",
    paste(map_named(rd, function(name, entry) {
      env <- toupper(chartr(".", "_", paste0("pkg_", name)))
      opt <- paste0("pkg.", name)
      paste0(
        "\\item \\sQuote{", name, "}: ",
        "(Env var: \\code{", env, "}, ",
        "option: \\code{", opt, "}.) ",
        entry
      )
    }), collapse = "\n"),
    "}"
  )
}

include_docs <- function(pkg, file, top = FALSE) {
  lib <- private_lib_dir()
  rds <- file.path(lib, pkg, file)
  if (!file.exists(rds)) {
    if (top) {
      return("\\section{Error}{Cannot load pkgdepends docs.}")
    } else {
      return("Error: cannot load pkgdepends docs.")
    }
  }

  rd <- readRDS(rds)
  # We cannot insert top level docs currently, because of a base R bug that
  # was fixed in R 4.0. See also the comments in tools/dynamic-help.R.
  rd <- gsub("\\section", "\\subsection", fixed = TRUE, rd)

  # Work around a weird bug. Apparently, if this is generated from a macro,
  # then R adds a `</p>` right after the inserted `<div>`. So we offset that with
  # a starting `<p>`. This must not be empty, hence the non-breaking space.
  rd <- gsub(
    "\\if{html}{\\out{<div class=\"sourceCode\">}}",
    "\\if{html}{\\out{<div class=\"sourceCode\"><p>&nbsp; }}",
    rd,
    fixed = TRUE
  )
  rd <- gsub(
    "\\if{html}{\\out{<div class=\"sourceCode r\">}}",
    "\\if{html}{\\out{<div class=\"sourceCode r\"><p>&nbsp;}}",
    rd,
    fixed = TRUE
  )

  rd
}

pak_or_pkgdepends <- function() "pak"

man_config_link <- function(txt) {
  paste0("\\link[=pak-config]{", txt, "}")
}
