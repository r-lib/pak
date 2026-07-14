roxy_to_rd <- function(text) {
  roxy_text <- paste0(
    "#' @name foo\n",
    "#' @title title\n",
    "#' @details\n",
    "#' ",
    gsub("\n", "\n#' ", text),
    "\n",
    "#' @md\n",
    "NULL\n"
  )
  # We change the current package's name to make sure that we create
  # cross-references that are fully qualified.
  pkg <- asNamespace("roxygen2")$roxy_meta_get("current_package")
  asNamespace("roxygen2")$roxy_meta_set("current_package", "no-package")
  on.exit(
    asNamespace("roxygen2")$roxy_meta_set("current_package", pkg),
    add = TRUE
  )
  # Suppress the warning caused by the changed package name
  out <- suppressWarnings(asNamespace("roxygen2")$roc_proc_text(
    asNamespace("roxygen2")$rd_roclet(),
    roxy_text
  ))

  det <- out$foo$get_rd("details")
  sec <- out$foo$get_rd("section")
  if (det == "NULL" && sec == "NULL") {
    cli::cli_alert_warning("No content in {.fn roxy_to_rd}.")
    ""
  } else if (det == "NULL") {
    sec
  } else if (sec == "NULL") {
    if (startsWith(det, "\\details")) {
      det <- sub("^[\\]details[{]\n?", "", det)
      det <- sub("\n?[}]$", "", det)
    }
    det
  } else {
    cli::cli_alert_warning("\\details{{}} content ignored in {.fn roxy_to_rd}.")
    sec
  }
}

generate_config_docs <- function() {
  documented <- Filter(function(x) !is.null(x$docs), pkgdepends_config)
  forwarded <- Filter(function(x) isTRUE(x$forwarded), documented)
  config <- Filter(function(x) !isTRUE(x$forwarded), documented)

  # for the dynamic help in pak. Forwarded entries are included too, but
  # since they do not follow the `pkg.` / `PKG_` naming convention, we
  # attach their actual option and environment variable names as attributes,
  # so that pak can pick them up instead of deriving them from the name.
  pakcfg <- Filter(function(x) x$pak %||% TRUE, documented)
  rd <- map_named(pakcfg, function(name, entry) {
    out <- roxy_to_rd(entry[["docs_pak"]] %||% entry[["docs"]])
    if (isTRUE(entry$forwarded)) {
      attr(out, "option") <- entry$option %||% paste0("pkg.", name)
      attr(out, "envvar") <- entry$envvar %||%
        toupper(chartr(".", "_", paste0("pkg_", name)))
    }
    out
  })
  outp <- paste(utils::capture.output(print(rd)), collapse = "\n")
  mdfile <- "tools/doc/pak-config-docs.md"
  oldp <- read_char(mdfile)
  if (outp != oldp) {
    outfile <- file.path("inst/docs/pak-config-docs.rds")
    cli::cli_alert_info("Writing {.path {outfile}}")
    saveRDS(rd, outfile, version = 2)
    write_char(outp, mdfile)
  }

  # for roxygen2 in pkgdepends
  items <- map_named(config, function(name, entry) {
    paste0("* `", name, "`: ", entry$docs)
  })
  alldocs <- paste(items, collapse = "\n")

  # Forwarded entries are documented in their own section, with their
  # actual option and environment variable names, as they do not follow
  # the `pkg.` / `PKG_` naming convention of the regular entries.
  if (length(forwarded)) {
    fitems <- map_named(forwarded, function(name, entry) {
      option <- entry$option %||% paste0("pkg.", name)
      envvar <- entry$envvar %||% toupper(paste0("PKG_", name))
      paste0(
        "* `", option, "` option, `", envvar,
        "` environment variable: ", entry$docs
      )
    })
    alldocs <- paste0(
      alldocs,
      "\n\n# Forwarded configuration\n\n",
      "The following entries are not used directly, they are ",
      "handled by other packages that perform the corresponding requests. ",
      "They are set via R options and environment variables, but, unlike ",
      "the entries above, they cannot be set via the `config` argument, and ",
      "their option and environment variable names may differ from the ",
      "`pkg.` and `PKG_` naming convention.\n\n",
      paste(fitems, collapse = "\n")
    )
  }

  alldocs
}

doc_share_rmd <- function(rmd, rds, md = NULL) {
  md <- md %||% sub("[.]Rmd$", ".md", rmd)
  if (md == rmd) {
    stop("Docs Rmd file must have extension `.Rmd`")
  }
  withr_with_envvar(c(THIS_IS_PAK = "true"), {
    cmd <- paste0("```{r child=\"", rmd, "\"}\n```\n")
    rd <- roxy_to_rd(cmd)
  })
  oldp <- read_char(md)
  if (oldp != rd) {
    cli::cli_alert_info("Writing {.path {rds}.}")
    saveRDS(rd, rds, version = 2)
    write_char(rd, md)
  }

  return("")
}

read_char <- function(path) {
  bin <- readBin(path, "raw", file.info(path)$size)
  txt <- rawToChar(bin)
  Encoding(txt) <- "UTF-8"
  txt
}

write_char <- function(x, path) {
  writeBin(charToRaw(x), path)
}

man_config_link <- function(txt) {
  paste0("\\link[=pkgdepends-config]{", txt, "}")
}
