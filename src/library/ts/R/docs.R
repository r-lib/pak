dglue <- function(..., .envir = parent.frame()) {
  glue(..., .open = "<<", .close = ">>", .envir = .envir)
}

is_rcmd_check <- function() {
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    return(FALSE)
  }
  Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != "" ||
    Sys.getenv("_R_RD_MACROS_PACKAGE_DIR_", "") != ""
}

doc_has_method <- function(method, package) {
  method <- paste0(method, ".ts_tree_", sub("^ts", "", package))
  length(utils::help(
    topic = (method),
    package = (package),
    help_type = "text"
  )) >
    0
}

doc_seealso <- function(method) {
  ts_package <- get_env("R_TS_PACKAGE")
  if (is.null(ts_package)) {
    psrs <- ts_list_parsers()
    psrs <- psrs[!duplicated(psrs$package), ]
    links <- vapply(
      psrs$package,
      function(pkg) {
        if (doc_has_method(method, pkg)) {
          glue(
            "\\code{{\\link[{pkg}:{method}.{pkg}]{{{method}(<ts_tree_{pkg}>)}}}}",
          )
        } else {
          ""
        }
      },
      ""
    )
    links <- links[links != ""]
    if (length(links) > 0) {
      s <- if (length(links) > 1) "s"
      glue("Method{s} in installed package{s}: {ts_collapse(links)}.")
    } else {
      ""
    }
  } else {
    paste0(
      "The generic of this method in the ts package: ",
      glue("\\code{{\\link[ts:{method}]{{{method}()}}}}"),
      "."
    )
  }
}

doc_insert <- function(key, manpkg = NULL) {
  if (is_rcmd_check()) {
    return("Placeholder.")
  }
  if (!is.null(manpkg)) {
    Sys.setenv("R_TS_PACKAGE" = manpkg)
    on.exit(Sys.unsetenv("R_TS_PACKAGE"), add = TRUE)
  }
  keypcs <- strsplit(key, "::", fixed = TRUE)[[1]]
  if (length(keypcs) == 2) {
    package <- keypcs[1]
    key <- keypcs[2]
  } else {
    package <- "ts"
  }
  lib <- dirname(find.package(package))
  output <- doc_create_chunk(key, lib, package, 1L, "<<contents>>")

  mch <- gregexpr("\\\\eval\\{[^\\}]+\\}", output, perl = TRUE)
  regmatches(output, mch)[[1]] <- lapply(
    regmatches(output, mch)[[1]],
    function(x) {
      x <- sub("^\\\\eval\\{", "", x)
      x <- sub("\\}$", "", x)
      eval(parse(text = x))
    }
  )
  output
}

doc_tabs <- function(key) {
  if (is_rcmd_check()) {
    return("Placeholder.")
  }
  package <- if (nzchar(ev <- Sys.getenv("R_TS_PACKAGE"))) {
    ev
  }

  if (is.null(package)) {
    doc_tabs_all(key)
  } else {
    doc_tabs_one(key, package)
  }
}

doc_tabs_all <- function(key) {
  # list all installed ts packages
  psrs <- ts_list_parsers()
  psrs <- psrs[!duplicated(psrs$package), ]

  tsdocpath <- doc_path("ts")
  t_tab <- read_char(file.path(tsdocpath, "tab.html"))
  t_div <- read_char(file.path(tsdocpath, "tabs.html"))
  t_btn <- read_char(file.path(tsdocpath, "btn.html"))
  output <- buttons <- tabs <- ""

  for (i in seq_len(nrow(psrs))) {
    language <- sub("^ts", "", psrs$package[i])
    tab <- doc_create_chunk(key, psrs$library[i], psrs$package[i], i, t_tab)
    btn <- dglue(
      t_btn,
      .envir = c(psrs[i, ], list(idx = i, language = language))
    )
    if (!is.null(tab) && nzchar(tab)) {
      tabs <- paste0(tabs, tab, "\n")
      buttons <- paste0(buttons, btn, "\n")
    }
  }
  if (tabs != "") {
    output <- dglue(t_div, .envir = list(tabs = tabs, buttons = buttons))
  }

  output
}

doc_tabs_one <- function(key, package) {
  lib <- dirname(find.package(package))
  doc_create_chunk(key, lib, package, 1, "<<contents>>")
}

doc_path <- function(package) {
  pkgdir <- find.package(package)
  docpath <- file.path(pkgdir, "tsdocs")
  if (!file.exists(docpath)) {
    docpath <- file.path(pkgdir, "inst", "tsdocs")
  }

  docpath
}

doc_create_chunk <- function(key, lib, package, idx, template) {
  file <- paste0(key, ".Rd")
  path <- file.path(doc_path(package), file)
  if (!file.exists(path)) {
    return("")
  }
  x <- read_char(path)
  lns <- strsplit(x, "\n", fixed = TRUE)[[1]]
  rulepos <- which(lns == "# ---")
  lns <- lns[(rulepos + 1):length(lns)]
  lang_data <- list(
    idx = idx,
    package = package,
    language = sub("^ts", "", package),
    contents = paste(lns, collapse = "\n")
  )
  dglue(template, .envir = lang_data)
}

doc_extra <- function() {
  if (is_rcmd_check()) {
    return("Placeholder.")
  }
  tsdocpath <- doc_path("ts")
  jspath <- file.path(tsdocpath, "tabs.js")
  js <- read_char(jspath)

  csspath1 <- file.path(doc_path("ts"), "w3.css")
  css1 <- read_char(csspath1)
  css <- ""
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    csspath2 <- file.path(doc_path("ts"), "pkgdown.css")
    css2 <- read_char(csspath2)
    css <- gsub("%", "\\%", paste0(css1, "\n\n", css2), fixed = TRUE)
    css <- paste0("<style>\n", css, "\n</style>\n")
  } else {
    # in Rd we insert CSS with JS, because tidy does not allow <style>
    # tags in <body>
    js <- paste0(
      "styles = `",
      gsub("%", "\\%", css1, fixed = TRUE),
      "`;\n",
      "var styleSheet = document.createElement('style');\n",
      "styleSheet.textContent = styles;\n",
      "document.head.appendChild(styleSheet);\n",
      js
    )
  }

  dglue("\\if{html}{\\out{<script>\n<<js>>\n</script>\n<<css>>}}")
}

doc_style_tab <- function(idx) {
  paste0(
    "padding: 0.01em 16px;",
    "border: 1px solid #ccc!important;",
    if (idx != 1) {
      "display:none"
    }
  )
}

# ------------------------------------------------------------------------------
# Roxygen2 roclet for generating manual stubs

roxy_tag_parse.roxy_tag_ts <- function(x) {
  lns <- strsplit(x$raw, "\n", fixed = TRUE)[[1]]
  lns1p <- strsplit(lns[1], " ", fixed = TRUE)[[1]]
  file <- lns1p[1]
  title <- paste(lns1p[-1], collapse = " ")

  x$raw <- paste(lns[-1], collapse = "\n")

  pkg <- asNamespace("roxygen2")$roxy_meta_get("current_package")
  asNamespace("roxygen2")$roxy_meta_set("current_package", "x")
  on.exit(
    asNamespace("roxygen2")$roxy_meta_set("current_package", pkg),
    add = TRUE
  )

  x <- asNamespace("roxygen2")$tag_markdown_with_sections(x)
  x$val <- list(
    file = file,
    title = title,
    val = x$val
  )
  x
}

roclet_ts <- function() {
  asNamespace("roxygen2")$roclet("ts")
}

roclet_process.roclet_ts <- function(x, blocks, env, base_path) {
  for (block in blocks) {
    tags <- asNamespace("roxygen2")$block_get_tags(block, "ts")

    for (tag in tags) {
      path <- file.path(
        base_path,
        "inst",
        "tsdocs",
        paste0(tag$val$file, ".Rd")
      )
      value <- paste0(
        "#| title: ",
        tag$val$title,
        "\n# ---\n",
        rd_patch_p(tag$val$val)
      )
      write_if_newer(value, path)
    }
  }
  invisible(list())
}

# Rd2HTML adds a </p> before the \preformatted, this is a hach.
# It might create an un-closed <p>, but that's apparently not a problem.
rd_patch_p <- function(rd) {
  gsub(
    '\\out{<div class="sourceCode r">}}\\preformatted',
    '\\out{<div class="sourceCode r"><p style="display:none">&nbsp;}}\\preformatted',
    rd,
    fixed = TRUE
  )
}

write_if_newer <- function(txt, path) {
  bin <- charToRaw(txt)
  nl <- charToRaw("\n")
  if (length(bin) > 0 && bin[[length(bin)]] != nl) {
    bin <- c(bin, nl)
  }
  if (file.exists(path)) {
    old <- readBin(path, what = "raw", n = file.size(path))
    if (identical(old, bin)) {
      return(invisible(FALSE))
    }
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  cli::cli_alert_info("Manual stub at {.file {basename(path)}}.")
  writeBin(bin, path)
  invisible(TRUE)
}

roclet_output.roclet_ts <- function(x, results, base_path, ...) {
  invisible(NULL)
}

ts_roclet_register <- function() {
  registerS3method(
    "roxy_tag_parse",
    "roxy_tag_ts",
    roxy_tag_parse.roxy_tag_ts,
    asNamespace("roxygen2")
  )
  registerS3method(
    "roclet_process",
    "roclet_ts",
    roclet_process.roclet_ts,
    asNamespace("roxygen2")
  )
  registerS3method(
    "roclet_output",
    "roclet_ts",
    roclet_output.roclet_ts,
    asNamespace("roxygen2")
  )
}
