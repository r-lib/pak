
#' Simplified manual. Start here!
#'
#' @name tldr
#' @description
#' You don't need to read long manual pages for a simple task.
#' This manual page collects the most common pak use cases.
#'
#' ```{r child = "man/chunks/tldr.Rmd"}
#' ```
NULL

#' Frequently Asked Questions
#'
#' @name faq
#' @description
#' Please look at this before asking questions.
#'
#' ```{r child = "man/chunks/FAQ.Rmd"}
#' ```
NULL

#' Awesome pak features
#'
#' @name features
#' @description
#' Features that make pak special.
#'
#' ```{r child = "man/chunks/features.Rmd"}
#' ```
NULL

#' All about installing pak.
#'
#' @name install
#' @description
#' Read this if the default installation methods do not work for you or
#' if you want the RC or development version.
#'
#' ```{r child = "man/chunks/install.Rmd"}
#' ```
NULL

#' pak configuration
#'
#' @name config
#' @description
#' Environment variables and options that modify the default pak behavior.
#'
#' ```{r child = "man/chunks/config.Rmd"}
#' ```
NULL

#' pak configuration
#'
#' @name pak-config
#' @aliases pak_config
#'
#' @description
#' pak behavior can be finetuned with environment variables and
#' options (as in [base::options()]).
#'
#' @details
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
#' \eval{pak:::doc_config()}
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

include_docs <- function(pkg, file) {
  lib <- private_lib_dir()
  rds <- file.path(lib, pkg, file)
  if (!file.exists(rds)) {
    return("Cannot look up documentation in pkgdepends. :(")
  }

  rd <- readRDS(rds)
  rd
}
