#' Scan R code for dependent packages
#'
#' Scan all R files of a project or directory for packages used within
#' them. It parses R code to find `library(package)`, `package::func()`,
#' and similar calls that imply package dependencies. See details below.
#'
#' # Detected dependencies
#'
#' `scan_deps()` detects package dependencies from these R expressions:
#' * `library()`, `require()`, `loadNamespace()` and `requireNamespace`
#'   calls.
#' * `::` and `:::` operators.
#' * Any of the calls in this list in R code from R markdown or quarto
#'   `R` and `Rscript` (case insensitive) code blocks or inline R code.
#' * A dependency on the methods package is inferred from finding
#'   `setClass()` and/or `setGeneric()` calls.
#' * `xfun::pkg_attach()` and `xfun::pkg_attach2()` calls.
#' * `pacman::p_load()` calls.
#' * `modules::import()` and `modules::module()` calls.
#' * `import::from()`, `import::here()` and `import::into()` calls.
#' * `box::use()` calls.
#' * `targets::tar_option_set(packages = ...)` calls.
#' * Any of the calls in this list in R code from `glue::glue()` strings.
#' * A dependency on the svglite package is inferred from
#'   `ggplot2::ggsave()` calls saving `.svg` files.
#' * Dependencies from `parsnip::set_engine()` calls, the default engine
#'   to package mapping is:
#'   - `"glm"` -> stats,
#'   - `"glmnet"` -> glmnet,
#'   - `"keras"` -> keras,
#'   - `"kknn"` -> kknn,
#'   - `"nnet"` -> nnet,
#'   - `"rpart"` -> rpart,
#'   - `"spark"` -> sparklyr,
#'   - `"stan"` -> rstanarm.
#'   You can override the default mapping by setting the
#'   `renv.parsnip.engines` option to a named list.
#' * A dependency on the xml2 package is inferred from using the
#'   "Junit" reporter (`JunitReporter`) from the testthat package.
#' * A dependency on the ragg package is inferred from setting the default
#'   knitr device (`dev` option) to `"ragg_png"`.
#' * A dependency on the hexbin package is inferred from using
#'   `ggplot2::geom_hex()`.
#' * A custom symbol name to package name mapping can be defined in the
#'   `renv.dependencies.database` option. This must be a named list of
#'   named lists, where the outer names are package names, the inner names
#'   are function or object names, and the values are package names. E.g.
#'   ```
#'   options(renv.dependencies.database = list(
#'     ggplot2 = list(geom_hex = "hexbin"),
#'     testthat = list(JunitReporter = "xml2")
#'   ))
#'   ```
#'
#' # Dependency types
#'
#' `scan_deps()` classifies package dependencies into three groups, based
#' on which files they were found:
#' * Production dependencies: `"prod"`.
#' * Test dependencies: `"test"`.
#' * Development dependencies: `"dev"`.
#'
#' @param path Files and/or directories to scan. Defaults to the current
#'   project, detected by finding the first parent directory of the current
#'   working directory, that contains a file or directory called
#'   `r cli::format_inline("{.or {pkgdepends:::project_root_anchors}}")`.
#'   (Note that this is different from `renv::dependencies()`, which only
#'   scans the current working directory by default!)
#'
#'   If `path` is not `NULL`, then only the specified files and directories
#'   are scanned, the directories recursively. In this case the `root`
#'   argument is used as the project root, to find `.gitignore` and
#'   `.renvignore` files. All entries of `path` must be within the `root`,
#'   the project root.
#' @param root The root directory of the project. It is used to find the
#'   `.gitignore` and `.renvignore` files. By default the same algorithm
#'   is used to detect this as for `path`. If `path` is specified and it is
#'   not within the detected or specified `root`, `scan_path()` throws an
#'   error.
#' @return Data frame with columns:
#'   * `path`: Path to the file in which the dependencies was found.
#'   * `package`: Detected package dependency. Typically a package name,
#'     but it can also be a package reference, e.g. a package from GitHub.
#'   * `type`: Dependency type. It is `"prod"`, `"test"` or `"dev"`. See
#'     'Dependency types' below.
#'   * `code`: The piece of code the dependency was extracted from.
#'   * `start_row`: Start row of the code the dependency was extracted
#'     from.
#'   * `start_column`: Start column of the code the dependency was
#'     extracted from.
#'   * `start_byte`: Start byte of the code the dependency was extracted
#'     from.
#'
#' Note the data frame may contain the same package multiple times, if it
#' was detected multiple times, e.g. multiple `library()` calls load the
#' same package.
#'
#' @export

scan_deps <- function(path = NULL, root = NULL) {
  if (is.null(path)) {
    path <- find_project_root()
    root <- root %||% path
  } else {
    root <- root %||% find_project_root()
  }
  assert_that(
    is_string(root),
    is_character(path)
  )
  if (!file.exists(root)) {
    throw(pkg_error("Project root {.path {root}} does not exist."))
  }
  if (any(bad <- !file.exists(path))) {
    throw(pkg_error("Path{?s} do{?es/} not exist: {.path {path[bad]}}."))
  }
  check_inside_dir(root, path)
  paths <- c(
    dir(path, pattern = scan_deps_pattern(), recursive = TRUE),
    if (root %in% path) {
      dir(root, pattern = scan_deps_pattern_root(), recursive = FALSE)
    }
  )
  full_paths <- normalizePath(file.path(path, paths), winslash = "/")
  deps_list <- lapply(full_paths, scan_path_deps)
  deps <- do.call("rbind", c(list(scan_deps_df()), deps_list))
  # write back the relative paths
  deps$path <- paths[match(deps$path, full_paths)]
  deps$type <- get_dep_type_from_path(deps$path, deps$type)
  class(deps) <- c("pkg_scan_deps", class(deps))
  deps
}

scan_deps_pattern <- function() {
  ptrns <- c(
    "[.]R$",
    "[.]r$",
    "[.]Rmd$",
    "[.]rmd$",
    "[.]rmarkdown",
    "[.]Rmarkdown",
    "[.][Rr]nw",
    "[.]qmd$",
    "[.]Rproj$",
    "^_bookdown[.]yml$",
    "^_quarto[.]yml$",
    NULL
  )
  paste0("(", paste0(collapse = "|", ptrns), ")")
}

scan_deps_pattern_root <- function() {
  ptrns <- c(
    "^DESCRIPTION$",
    "^NAMESPACE$",
    "^_pkgdown.yml$",
    "^renv[.]lock$",
    "^rsconnect$",
    NULL
  )
  paste0("(", paste0(collapse = "|", ptrns), ")")
}

scan_deps_file_type_use_basename <- function() {
  # for these we don't use the extension but the basename
  # to decide which parser to use
  c(
    "DESCRIPTION",
    "NAMESPACE",
    "_bookdown.yml",
    "_pkgdown.yml",
    "_quarto.yml",
    "renv.lock",
    NULL
  )
}

scan_deps_file_type <- function(paths) {
  ext <- tolower(file_ext(paths))
  bsn <- basename(paths)
  ifelse(
    bsn %in% scan_deps_file_type_use_basename() | is.na(ext),
    bsn,
    ext
  )
}

# -------------------------------------------------------------------------

# needs to increase as the deps discovry code changes, otherwise we don't
# apply the new discovery code
deps_cache_version <- 2L

get_deps_cache_path <- function(hash = NULL) {
  root <- file.path(get_user_cache_dir()$root, "deps", deps_cache_version)
  if (is.null(hash)) {
    root
  } else {
    file.path(root, substr(hash, 1, 2), hash)
  }
}

clear_deps_cache <- function() {
  unlink(dirname(get_deps_cache_path()), recursive = TRUE)
}

re_r_dep <- function() {
  db <- renv_dependencies_database()
  fns <- as.character(unlist(lapply(db, names)))
  paste0(collapse = "|", c(
    "library", "require", "loadNamespace",
    "::",
    "setClass", "setGeneric",
    "pkg_attach",
    "p_load",
    "module",
    "import",
    "box::",
    "tar_option_set",
    "glue",
    "ggsave",
    "set_engine",
    "opts_chunk",
    "geom_hex",
    "JunitReporter",
    fns
  ))
}

scan_path_deps <- function(path) {
  code <- if (file.info(path)$isdir) {
    NULL
  } else {
    readBin(path, "raw", file.size(path))
  }

  # check if already known, set path
  should_cache <- scan_path_should_cache(path)
  if (should_cache) {
    hash <- cli::hash_raw_xxhash(code)
    cache <- get_deps_cache_path(hash)
    if (file.exists(cache)) {
      deps <- readRDS(cache)
      if (!is.null(deps) && nrow(deps) > 0) {
        deps$path <- path
        deps$type <- get_dep_type_from_path(deps$path, deps$type)
      }
      return(deps)
    }
  }

  # scan it if it is worth it, based on a quick check
  maybe_has_deps <- scan_deps_file_type(path) != "r" ||
    length(grepRaw(re_r_dep(), code)) > 0
  deps <- if (maybe_has_deps) {
    scan_path_deps_do(code, path)
  }

  # save it to the cache, but anonimize it first. If no deps, save NULL
  if (should_cache) {
    deps_no_path <- deps
    if (!is.null(deps_no_path) && nrow(deps_no_path) > 0) {
      deps_no_path$path <- ""
    }
    dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(deps_no_path, cache)
  }

  deps
}

scan_deps_df <- function(
  path = character(),
  ref = package,
  package = character(),
  version = "*",
  type = get_dep_type_from_path(path),
  code = character(),
  start_row = 1L,
  start_column = 1L,
  start_byte = 1L
) {
  data_frame(
    path = path,
    ref = ref,
    package = package,
    version = version,
    type = type,
    code = code,
    start_row = start_row,
    start_column = start_column,
    start_byte = start_byte
  )
}

scan_path_should_cache <- function(paths) {
  # we don't want to cache the ones that depend on the file
  # name, because caching is content-based.
  ! basename(paths) %in% c(
    "_bookdown.yml",
    "_pkgdown.yml",
    "_quarto.yml",
    "renv.lock",
    "rsconnect",
    NULL
  )
}

scan_path_deps_do <- function(code, path) {
  ext <- scan_deps_file_type(path)
  switch(
    ext,
    ".r" = scan_path_deps_do_r(code, path),
    ".qmd" = ,
    ".rmarkdown" = ,
    ".rmd" = scan_path_deps_do_rmd(code, path),
    ".rnw" = scan_path_deps_do_rnw(code, path),
    ".ipynb" = scan_path_deps_do_ipynb(code, path),
    "DESCRIPTION" = scan_path_deps_do_dsc(code, path),
    "NAMESPACE" = scan_path_deps_do_namespace(code, path),
    "_bookdown.yml" = scan_path_deps_do_bookdown(code, path),
    "_pkgdown.yml" = scan_path_deps_do_pkgdown(code, path),
    "_quarto.yml" = scan_path_deps_do_quarto(code, path),
    "renv.lock" = scan_path_deps_do_renv_lock(code, path),
    "rsconnect" = scan_path_deps_do_rsconnect(code, path),
    ".rproj" = scan_path_deps_do_rproj(code, path),
    stop("Cannot parse ", ext, " file for dependencies, internal error")
  )
}

# -------------------------------------------------------------------------

scan_path_deps_do_r <- function(code, path, ranges = NULL) {
  hits <- code_query(code, q_deps(), ranges = ranges)
  mct <- hits$matched_captures

  # q_library_0 hits are generic ones, only use them if they are not hit
  gen_pat <- hits$patterns$id[hits$patterns$name == "q_library_0"]
  gen_hits <- mct[mct$pattern %in% gen_pat, ]

  # for these patterns we need to work from the function names
  fn_patterns <- "methods"
  fn_pat <- hits$patterns$id[hits$patterns$name %in% fn_patterns]
  fn_hits <- mct[mct$pattern %in% fn_pat, ]

  # junit reporter needs xml2
  jr_patterns <- "junit_reporter"
  jr_pat <- hits$patterns$id[hits$patterns$name %in% jr_patterns]
  jr_hits <- mct[mct$pattern %in% jr_pat, ]

  # knit ragg_png device needs ragg
  ragg_patterns <- "knitr_dev"
  ragg_pat <- hits$patterns$id[hits$patterns$name %in% ragg_patterns]
  ragg_hits <- mct[mct$pattern %in% ragg_pat, ]

  # database that matches symbols to packages
  db_patterns <- "database"
  db_pat <- hits$patterns$id[hits$patterns$name %in% db_patterns]
  db_hits <- mct[mct$pattern %in% db_pat, ]

  pkg_hits <- mct[! mct$pattern %in% c(gen_pat, fn_pat, jr_pat, ragg_pat, db_hits), ]
  rbind(
    if (nrow(pkg_hits) > 0) scan_path_deps_do_pkg_hits(pkg_hits, path),
    if (nrow(fn_hits) > 0) scan_path_deps_do_fn_hits(fn_hits, path),
    if (nrow(gen_hits) > 0) scan_path_deps_do_gen_hits(gen_hits, path),
    if (nrow(jr_hits) > 0) scan_path_deps_do_jr_hits(jr_hits, path),
    if (nrow(ragg_hits) > 0) scan_pat_deps_do_ragg_hits(ragg_hits, path),
    if (nrow(db_hits) > 0) scan_pat_deps_do_db_hits(db_hits, path)
  )
}

scan_path_deps_do_pkg_hits <- function(hits, path) {
  pkg <- hits$code[hits$name == "pkg-name"]
  scan_deps_df(
    path = path,
    package = pkg,
    code = hits$code[hits$name == "dep-code"],
    start_row = hits$start_row[hits$name == "dep-code"],
    start_column = hits$start_column[hits$name == "dep-code"],
    start_byte = hits$start_byte[hits$name == "dep-code"]
  )
}

scan_path_deps_do_fn_hits <- function(hits, path) {
  fn_pkg_map <- c(setClass = "methods", setGeneric = "methods")
  fn_names <- hits$code[hits$name == "fn-name"]
  scan_deps_df(
    path = path,
    package = fn_pkg_map[fn_names],
    code = hits$code[hits$name == "dep-code"],
    start_row = hits$start_row[hits$name == "dep-code"],
    start_column = hits$start_column[hits$name == "dep-code"],
    start_byte = hits$start_byte[hits$name == "dep-code"]
  )
}

scan_path_deps_do_gen_hits <- function(hits, path) {
  code <- hits$code[hits$name == "dep-code"]
  fn <- hits$code[hits$name == "fn-name"]
  ns <- vcapply(
    unique(hits$match),
    function(m) {
      hits[hits$match == m & hits$name == "ns-name", ]$code %|0|%
        NA_character_
    }
  )
  pkgs <- lapply(seq_along(code), function(i) {
    safe_parse_pkg_from_call(ns[i], fn[i], code[i])
  })
  pkgs_count <- lengths(pkgs)
  scan_deps_df(
    path = path,
    package = unlist(pkgs),
    code = rep(code, pkgs_count),
    start_row = rep(hits$start_row[hits$name == "dep-code"], pkgs_count),
    start_column = rep(hits$start_column[hits$name == "dep-code"], pkgs_count),
    start_byte = rep(hits$start_byte[hits$name == "dep-code"], pkgs_count)
  )
}

scan_path_deps_do_jr_hits <- function(hits, path) {
  code <- hits$code[hits$name == "dep-code"]
  scan_deps_df(
    path = path,
    package = "xml2",
    code = code,
    start_row = hits$start_row[hits$name == "dep-code"],
    start_column = hits$start_column[hits$name == "dep-code"],
    start_byte = hits$start_byte[hits$name == "dep-code"]
  )
}

scan_pat_deps_do_ragg_hits <- function(hits, path) {
  wcodes <- which(hits$name == "dep-code")
  for (wc in wcodes) {
    expr <- parse(text = hits$code[wc], keep.source = FALSE)
    matched <- match.call(function(...) { }, expr, expand.dots=FALSE)
    args <- matched[["..."]]
    if ("dev" %in% names(args) && args[["dev"]] == "ragg_png") {
      return(scan_deps_df(
        path = path,
        package = "ragg",
        code = hits$code[wc],
        start_row = hits$start_row[wc],
        start_column = hits$start_column[wc],
        start_byte = hits$start_byte[wc]
      ))
    }
  }
  NULL
}

scan_pat_deps_do_db_hits <- function(hits, path) {
  db <- renv_dependencies_database()
  fns <- unlist(lapply(db, names))
  map <- unlist(unname(db), recursive = FALSE)
  pkgs <- unlist(map[hits$code])
  scan_deps_df(
    path = path,
    package = pkgs,
    code = hits$code,
    start_row = hits$start_row,
    start_column = hits$start_column,
    start_byte = hits$start_byte
  )
}

# nocov start
prot_xfun_pkg_attach <- function(..., install, message) { }
prot_xfun_pkg_attach2 <- function(...) { }
prot_pacman_p_load <- function(
  ..., char, install, update, character.only) {
}
prot_modules_import <- function(
  from, ..., attach = TRUE, where = parent.frame()) {
}
prot_modules_module <- function(
  expr = {}, topEncl = NULL, envir = parent.frame()) {
}
prot_import_from <- function(.from, ..., .character_only = FALSE) { }
prot_import_here <- function(.from, ..., .character_only = FALSE) { }
prot_import_into <- function(
  .into, ..., .from, .library = NULL, .directory = ".", .all = NULL,
  .except = character(), .chdir = TRUE, .character_only = FALSE,
  .S3 = FALSE) {
}
prot_box_use <- function(...) { }
prot_targets_tar_option_set <- function(
  tidy_eval = NULL, packages = NULL, ...) {
}
prot_glue_glue <- function(
  ..., .sep = "", .envir = parent.frame(), .open = "{", .close = "}") {
}
prot_ggplot2_ggsave <- function(filename, ...) { }
prot_parsnip_set_engine <- function(object, engine, ...) { }
prot_r6_r6class <- function(
  classname = NULL, public = list(), private = NULL, active = NULL,
  inherit = NULL, ...) { }
prot_testthat_test_package <- function(package, reporter = NULL, ...) { }
prot_testthat_test_dir <- function(
  path, filter = NULL, reporter = NULL, ...) {
}
prot_testthat_test_file <- function(path, reporter = NULL, ...) { }
# nocov end

safe_parse_pkg_from_call <- function(ns, fn, code) {
  tryCatch(
    parse_pkg_from_call(ns, fn, code),
    error = function(...) NULL
  )
}

parse_pkg_from_call_match <- function(fn, code) {
  expr <- parse(text = code, keep.source = FALSE)
  fun <- switch(fn,
    "library" = base::library,
    "require" = base::require,
    "loadNamespace" = base::loadNamespace,
    "requireNamespace" = base::requireNamespace,
    "pkg_attach" = prot_xfun_pkg_attach,
    "pkg_attach2" = prot_xfun_pkg_attach2,
    "p_load" = prot_pacman_p_load,
    "import" = prot_modules_import,
    "module" = prot_modules_module,
    "from" = prot_import_from,
    "here" = prot_import_here,
    "into" = prot_import_into,
    "use" = prot_box_use,
    "tar_option_set" = prot_targets_tar_option_set,
    "glue" = prot_glue_glue,
    "ggsave" = prot_ggplot2_ggsave,
    "set_engine" = prot_parsnip_set_engine,
    "R6Class" = prot_r6_r6class,
    "test_package" = prot_testthat_test_package,
    "test_dir" = prot_testthat_test_dir,
    "test_file" = prot_testthat_test_file
  )
  match.call(fun, expr, expand.dots = FALSE)
}

parse_pkg_from_call <- function(ns, fn, code) {
  matched <- parse_pkg_from_call_match(fn, code)
  switch(fn,
    "library" = , "require" =
      parse_pkg_from_call_library(ns, fn, matched),
    "loadNamespace" = , "requireNamespace" =
      parse_pkg_from_call_loadnamespace(ns, fn, matched),
    "pkg_attach" = , "pkg_attach2" =
      parse_pkg_from_call_xfun(ns, fn, matched),
    "p_load" =
      parse_pkg_from_call_pacman(ns, fn, matched),
    "import" =
      parse_pkg_from_call_modules_import(ns, fn, matched),
    "module" =
      parse_pkg_from_call_modules_module(ns, fn, matched),
    "from" = , "here" = , "into" =
      parse_pkg_from_call_import(ns, fn, matched),
    "use" =
      parse_pkg_from_call_box(ns, fn, matched),
    "tar_option_set" =
      parse_pkg_from_call_targets(ns, fn, matched),
    "glue" =
      parse_pkg_from_call_glue(ns, fn, matched),
    "ggsave" =
      parse_pkg_from_call_ggplot2(ns, fn, matched),
    "set_engine" =
      parse_pkg_from_call_parsnip(ns, fn, matched),
    "R6Class" =
      parse_pkg_from_call_testthat_r6class(ns, fn, matched),
    "test_package" = , "test_dir" = , "test_file" =
      parse_pkg_from_call_testthat_test(ns, fn, matched)
  )
}

parse_pkg_from_call_library <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "base") return(NULL)
  pkg <- matched[["package"]]
  if (is.character(pkg) && length(pkg) == 1) {
    return(pkg)
  }
  if (is.symbol(pkg) &&
      identical(matched[["character.only"]] %||% FALSE, FALSE)) {
    return(as.character(pkg))
  }
  NULL
}

parse_pkg_from_call_loadnamespace <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "base") return(NULL)
  pkg <- matched[["package"]]
  if (is.character(pkg) && length(pkg) == 1) {
    return(pkg)
  }
  NULL
}

parse_pkg_from_call_xfun <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "xfun") return(NULL)
  pkgs <- unlist(lapply(
    matched[["..."]],
    function(x) if (is.character(x)) x
  ))
  if (length(pkgs) > 0) return(pkgs)
  NULL
}

parse_pkg_from_call_pacman <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "pacman") return(NULL)
  # list of characters and symbols
  pkgs <- as.list(matched[["..."]])

  # character vector or scalar
  char <- matched[["char"]]
  if (length(char) > 0 && char[[1]] == quote(c)) {
    pkgs <- c(pkgs, as.list(char[-1]))
  } else if (is.character(char)) {
    pkgs <- c(pkgs, as.list(char))
  }
  if (matched[["character.only"]] %||% FALSE) {
    pkgs <- pkgs[vlapply(pkgs, function(x) is.character(x))]
  } else {
    pkgs <- pkgs[vlapply(pkgs, function(x) is.symbol(x) || is.character(x))]
  }
  pkgs <- vcapply(pkgs, as.character)
  if (length(pkgs) > 0) return(pkgs)
  NULL
}

parse_pkg_from_call_modules_import <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "modules") return(NULL)
  pkgs <- as.character(matched[["from"]])
  if (length(pkgs) > 0) return(pkgs)
  NULL
}

parse_pkg_from_call_modules_module <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "modules") return(NULL)
  expr <- as.character(matched[["expr"]])
  hits <- code_query(expr, q_module_import())[["matched_captures"]]
  code <- hits$code[hits$name == "dep-code"]
  pkgs <- lapply(seq_along(code), function(i) {
    safe_parse_pkg_from_call(ns, "import", code[i])
  })
  if (length(pkgs) > 0) return(unlist(pkgs))
  NULL
}

parse_pkg_from_call_import <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "import") return(NULL)
  from <- matched[[".from"]]
  if (is.symbol(from)) {
    if (!identical(matched[[".character_only"]], TRUE)) {
      from <- as.character(from)
    }
  }
  if (!is.character(from) || length(from) != 1) {
    return(NULL)
  }

  # '.from' can also be an R script; if it appears to be a path, then ignore it
  # https://github.com/rstudio/renv/issues/1743
  if (grepl("\\.[rR]$", from, perl = TRUE) && grepl("[/\\]", from)) {
    return(NULL)
  }

  from
}

parse_pkg_from_call_box <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "box") return(NULL)
  args <- as.list(matched[["..."]])
  pkgs <- na.omit(vcapply(args, function(arg) {
    if (!is.symbol(arg) && identical(arg[[1]], quote(`/`))) {
      return(NA_character_)
    }
    name <- if (is.symbol(arg) && !identical(arg, quote(expr = ))) {
      as.character(arg)
    } else if (
      identical(arg[[1]], quote(`[`)) &&
        length(arg) > 1L &&
        is.symbol(arg[[2L]])) {
      as.character(arg[[2L]])
    }
    if (is.null(name) || name == "." || name == "..") {
      return(NA_character_)
    }
    name
  }))

  if (length(pkgs) > 0) return(pkgs)
  NULL
}

parse_pkg_from_call_targets <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "targets") return(NULL)
  pkgs <- matched[["packages"]]
  pkgs <- dependencies_eval(pkgs)
  if (is.character(pkgs) && length(pkgs) > 0) {
    return(pkgs)
  }
  NULL
}

# from renv:::renv_dependencies_eval
dependencies_eval <- function(expr) {
  syms <- c("list", "c", "T", "F", "{", "(", "[", "[[", "::",
    ":::", "$", "@", ":", "+", "-", "*", "/", "<", ">", "<=",
    ">=", "==", "!=", "!", "&", "&&", "|", "||")
  vals <- mget(syms, envir = baseenv())
  envir <- list2env(vals, parent = emptyenv())
  eval(expr, envir = envir)
}

parse_pkg_from_call_glue <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "glue") return(NULL)
  args <- as.list(matched[["..."]])
  nm <- names(args) %||% rep.int("", length(args))
  str <- args[!nzchar(nm) & vlapply(args, is.character)]
  code <- character()
  for (s in str) {
    asNamespace("cli")$glue(
      s,
      .open = matched[[".open"]] %||% "{",
      .close = matched[[".close"]] %||% "}",
      .transformer = function(x, envir) { code <<- c(code, x) }
    )
  }

  pkgs <- unlist(lapply(
    code,
    function(x) scan_path_deps_do_r(x, path = "")[["package"]]
  ))
  if (length(pkgs) > 0) {
    return(pkgs)
  }
  NULL
}

parse_pkg_from_call_ggplot2 <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "ggplot2") return(NULL)
  fn <- matched[["filename"]]
  if (!is.character(fn)) {
    return(NULL)
  }
  # check for attempts to save to '.svg', and assume svglite is
  # required in this scenario.
  if (any(endsWith(fn, ".svg"))) {
    return("svglite")
  }
  NULL
}

parse_pkg_from_call_parsnip <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "parsnip") return(NULL)
  engine <- matched[["engine"]]
  if (!is.character(engine) || length(engine) != 1L) {
    return(NULL)
  }

  map <- getOption("renv.parsnip.engines", default = list(
    glm    = "stats",
    glmnet = "glmnet",
    keras  = "keras",
    kknn   = "kknn",
    nnet   = "nnet",
    rpart  = "rpart",
    spark  = "sparklyr",
    stan   = "rstanarm"
  ))

  pkgs <- if (is.function(map)) {
    map(engine)
  } else {
    map[[engine]]
  }

  if (length(pkgs) > 0) {
    return(pkgs)
  }
  NULL
}

parse_pkg_from_call_testthat_r6class <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "R6") return(NULL)
  inherit <- matched[["inherit"]]
  if (identical(inherit, quote(JunitReporter)) ||
      identical(inherit, quote(testthat::JunitReporter))) {
    return("xml2")
  }
  NULL
}

parse_pkg_from_call_testthat_test <- function(ns, fn, matched) {
  if (!is.na(ns) && ns != "testthat") return(NULL)
  reporter <- matched[["reporter"]]
  if (identical(reporter, "Junit") ||
      identical(reporter, "junit") ||
      identical(reporter, quote(JunitReporter)) ||
      identical(reporter, quote(JunitReporter))) {
    return("xml2")
  }
  NULL
}

# -------------------------------------------------------------------------

scan_path_deps_do_rmd <- function(code, path) {
  hits <- code_query(code, language = "markdown", query = q_deps_rmd())
  inl_pat <- hits$patterns$id[hits$patterns$name == "inline"]
  inl_hits <- hits$matched_captures[
    hits$matched_captures$pattern %in% inl_pat, ]
  hdr_pat <- hits$patterns$id[hits$patterns$name == "header"]
  hdr_hits <- hits$matched_captures[
    hits$matched_captures$pattern %in% hdr_pat, ]
  blk_hits <- hits$matched_captures[
    ! hits$matched_captures$pattern %in% c(inl_pat, hdr_pat), ]
  rbind(
    if (nrow(inl_hits)) scan_path_deps_do_inline_hits(code, inl_hits, path),
    if (nrow(blk_hits)) scan_path_deps_do_block_hits(code, blk_hits, path),
    if (nrow(hdr_hits)) scan_path_deps_do_header_hits(code, hdr_hits, path),
    if (nrow(blk_hits)) scan_path_deps_do_rmarkdown(code, blk_hits, path)
  )
}

range_cols <- c(
  "start_row", "start_column", "end_row", "end_column",
  "start_byte", "end_byte"
)

scan_path_deps_do_inline_hits <- function(code, inl_hits, path) {
  wcnd <- which(inl_hits$name == "inline")
  wcnd <- wcnd[grepl("`", inl_hits$code[wcnd], fixed = TRUE)]
  wcnd <- wcnd[grepl(re_r_dep(), inl_hits$code[wcnd])]
  if (length(wcnd) == 0) {
    return(NULL)
  }

  inl_ranges <- inl_hits[wcnd, range_cols]
  r_hits <- code_query(
    code,
    language = "markdown-inline",
    ranges = inl_ranges,
    query = q_deps_rmd_inline()
  )
  cpt <- r_hits$matched_captures
  pre_drop <- nchar(cpt$code[cpt$name == "csd1"])
  post_drop <- nchar(cpt$code[cpt$name == "csd2"])
  r_code <- omit_pre_post(cpt$code[cpt$name == "code"], pre_drop, post_drop)
  wcnd2 <- substr(r_code, 1, 2) == "r " & grepl(re_r_dep(), r_code)
  if (!any(wcnd2)) {
    return(NULL)
  }
  # need to adjust the ranges for the _ASCII_ (!) delimiters
  r_ranges <- cpt[cpt$name == "code", ][wcnd2, range_cols]
  r_ranges$start_byte <- r_ranges$start_byte + pre_drop[wcnd2] + 2L   # 'r '
  r_ranges$start_column <- r_ranges$start_column + pre_drop[wcnd2] + 2L
  r_ranges$end_byte <- r_ranges$end_byte - post_drop[wcnd2]
  scan_path_deps_do_r(code, path = path, ranges = r_ranges)
}

scan_path_deps_do_block_hits <- function(code, blk_hits, path) {
  wcnd <- which(blk_hits$name == "content")
  wcnd <- wcnd[grepl(re_r_dep(), blk_hits$code[wcnd])]
  if (length(wcnd) == 0) {
    return(NULL)
  }

  r_ranges <- blk_hits[wcnd, range_cols]
  scan_path_deps_do_r(code, path = path, ranges = r_ranges)
}

scan_path_deps_do_rmarkdown <- function(code, blk_hits, path) {
  blk_hits <- blk_hits[blk_hits$name == "language", ]
  rchk <- tolower(blk_hits$code) %in% c("r", "rscript")
  if (any(rchk)) {
    # only add the first chunk, adding all R chunks seems too much
    rchk <- which(rchk)[1]
    scan_deps_df(
      path = path,
      package = "rmarkdown",
      code = blk_hits$code[rchk],
      start_row = blk_hits$start_row[rchk],
      start_column = blk_hits$start_column[rchk],
      start_byte = blk_hits$start_byte[rchk]
    )
  }
}

# Crossref: https://github.com/r-lib/pkgdepends/issues/399
# This is pretty difficult, unfortunately, but could not come up with a
# simpler solution.
# * We use tree-sitter to parse and search the YAML, so that we can have
#   coordinates, and also the search is much simpler than when using a YAML
#   parser.
# * The tree-sitter parser cannot scan the actual values of the scalars, so
#   we use a YAML parser for that (libyaml). (No, it is not better to scan
#   them manually, they are quite involved.)
# * Scanning the scalars is a transformation, not just a subsetting, so
#   we lose the correct coordinates for the things (e.g. R code) _within_
#   the values. We still have the coordinates for the values, though.
# * We don't handle references correctly, because the tree-sitter parser
#   does not help with that. For that we'd need to parse the whole YAML
#   with libyaml. Maybe we'll do that in the future.

scan_path_deps_do_header_hits <- function(code, hdr_hits, path) {
  hits <- code_query(
    code,
    language = "yaml",
    query = q_deps_yaml_header(),
    ranges = hdr_hits[, range_cols]
  )

  shiny_pat <- hits$patterns$id[hits$patterns$name == "shiny"]
  shiny_hits <- hits$matched_captures[
    hits$matched_captures$pattern %in% shiny_pat, ]
  pkgstr_pat <- hits$patterns$id[hits$patterns$name == "pkgstring"]
  pkgstr_hits <- hits$matched_captures[
    hits$matched_captures$pattern %in% pkgstr_pat, ]
  bslib_pat <- hits$patterns$id[hits$patterns$name == "bslib"]
  bslib_hits <- hits$matched_captures[
    hits$matched_captures$pattern %in% bslib_pat, ]
  tag_pat <- hits$patterns$id[hits$patterns$name == "tag"]
  tag_hits <- hits$matched_captures[
    hits$matched_captures$pattern %in% tag_pat, ]

  rbind(
    if (nrow(shiny_hits)) {
      scan_path_deps_do_header_shiny_hits(code, shiny_hits, path)
    },
    if (nrow(pkgstr_hits)) {
      scan_path_deps_do_header_pkgstr_hits(code, pkgstr_hits, path)
    },
    if (nrow(bslib_hits)) {
      scan_path_deps_do_header_bslib_hits(code, bslib_hits, path)
    },
    if (nrow(tag_hits)) {
      scan_path_deps_do_header_tag_hits(code, tag_hits, path)
    }
  )
}

scan_path_deps_do_header_shiny_hits <- function(code, hits, path) {
  hits <- hits[hits$name == "value", ]
  vals <- yaml_parse_scalar(hits$code)
  shiny <- vals == "shiny"
  scan_deps_df(
    path = path,
    package = "shiny",
    code = hits$code[shiny],
    start_row = hits$start_row[shiny],
    start_column = hits$start_column[shiny],
    start_byte = hits$start_byte[shiny]
  )
}

scan_path_deps_do_header_pkgstr_hits <- function(code, hits, path) {
  vals <- yaml_parse_scalar(hits$code)
  pkg <- vapply(vals, FUN.VALUE = character(1), function(x) {
    tryCatch({
      expr <- parse(text = x, keep.source = FALSE)[[1]]
      if (length(expr) == 3 && is.call(expr) &&
          (identical(expr[[1]], quote(`::`)) ||
           identical(expr[[1]], quote(`:::`)))) {
        as.character(expr[[2]])
      } else {
        NA_character_
      }
    }, error = function(...) NA_character_)
  })
  if (all(is.na(pkg))) return(NULL)
  hits <- hits[!is.na(pkg), ]
  pkg <- na.omit(pkg)
  scan_deps_df(
    path = path,
    package = pkg,
    code = hits$code,
    start_row = hits$start_row,
    start_column = hits$start_column,
    start_byte = hits$start_byte
  )
}

scan_path_deps_do_header_bslib_hits <- function(code, hits, path) {
  scan_deps_df(
    path = path,
    package = "bslib",
    code = hits$code[hits$name == "code"],
    start_row = hits$start_row[hits$name == "code"],
    start_column = hits$start_column[hits$name == "code"],
    start_byte = hits$start_byte[hits$name == "code"]
  )
}

scan_path_deps_do_header_tag_hits <- function(code, hits, path) {
  hits <- hits[hits[["name"]] == "code", ]
  vals <- yaml_parse_scalar(hits$code)
  res <- lapply(seq_along(vals), function(vi) {
    r1 <- scan_path_deps_do_r(vals[vi], path = path)
    # need to replace the positions with the ones from the YAML file
    # we cannot use ranges here, because the R code is a a transformation
    # of the text in the YAML file, not merely a subset. So we just mark
    # the beginning of the R code in the YAML, instead of the real position
    # of the `library()` etc. calls.
    r1[["start_row"]][] <- hits[vi, "start_row"]
    r1[["start_column"]][] <- hits[vi, "start_column"]
    r1[["start_byte"]][] <- hits[vi, "start_byte"]
    r1
  })
  do.call(rbind, res)
}

yaml_parse_scalar <- function(x) {
  vcapply(x, function(x) .Call(c_yaml_parse_scalar, x), USE.NAMES = FALSE)
}

# -------------------------------------------------------------------------

scan_path_deps_do_dsc <- function(code, path) {
  if (is.raw(code)) code <- rawToChar(code)
  dsc <- desc::desc(text = code)
  deps <- resolve_ref_deps(
    dsc$get_deps(),
    dsc$get("Remotes")[[1]],
    dsc$get(extra_config_fields(dsc$fields()))
  )
  deps <- deps[deps$package != "R", ]
  version <- ifelse(deps$op == "", "*", paste0(deps$op, deps$version))
  scan_deps_df(
    path = path,
    ref = deps$ref,
    package = deps$package,
    version = version,
    type = get_dep_type_from_description_field(deps$type),
    code = deps$ref
  )
}

# -------------------------------------------------------------------------

scan_path_deps_do_namespace <- function(code, path) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mkdirp(file.path(tmp, "pkg"))
  if (is.raw(code)) {
    writeBin(code, file.path(tmp, "pkg", "NAMESPACE"))
  } else {
    writeLines(code, file.path(tmp, "pkg", "NAMESPACE"))
  }
  info <- parseNamespaceFile(package = "pkg", package.lib = tmp)
  pkg <- unique(vcapply(info$imports, "[[", 1))
  scan_deps_df(
    path = path,
    package = pkg,
    type = "prod",
    code = pkg
  )
}

# -------------------------------------------------------------------------

scan_path_deps_do_bookdown <- function(code, path) {
  scan_deps_df(
    path = path,
    package = "bookdown",
    code = NA_character_
  )
}

scan_path_deps_do_pkgdown <- function(code, path) {
  scan_deps_df(
    path = path,
    package = "pkgdown",
    code = NA_character_
  )
}

scan_path_deps_do_quarto <- function(code, path) {
  # renv does not include anything for quarto
  # Do we want a 'dev' dependency for the quarto package?
  # Maybe that's too opinionated?
}

scan_path_deps_do_renv_lock <- function(code, path) {
  scan_deps_df(
    path = path,
    package = "renv",
    code = NA_character_
  )
}

scan_path_deps_do_rsconnect <- function(code, path) {
  scan_deps_df(
    path = path,
    package = "rsconnect",
    code = NA_character_
  )
}

# -------------------------------------------------------------------------

scan_path_deps_do_rproj <- function(code, path) {
  con <- if (is.raw(code)) rawConnection(code) else textConnection(code)
  on.exit(close(con), add = TRUE)
  if ("yes" %in% tolower(read.dcf(con, fields = "PackageUseDevtools"))) {
    scan_deps_df(
      path = path,
      package = c("devtools", "roxygen2"),
      type = "dev",
      code = "PackageUseDevtools: Yes"
    )
  }
}

# -------------------------------------------------------------------------

# knitr::all_patterns$rnw
re_rnw <- list(
  chunk.begin = "^\\s*<<(.*)>>=.*$",
  chunk.end = "^\\s*@\\s*(%+.*|)$",
  inline.code = "\\\\Sexpr\\{([^}]+)\\}",
  inline.comment = "^\\s*%.*",
  ref.chunk = "^\\s*<<(.+)>>\\s*$",
  header.begin = "(^|\n)\\s*\\\\documentclass[^}]+\\}",
  document.begin = "\\s*\\\\begin\\{document\\}"
)

scan_path_deps_do_rnw <- function(code, path) {
  if (is.raw(code)) {
    code <- rawToChar(code)
    Encoding(code) <- "UTF-8"
  }
  code <- unlist(strsplit(code, "\n", fixed = TRUE))
  chunks <- scan_path_deps_do_rnw_chunks(code)

  do.call(
    "rbind",
    lapply(chunks, function(c) scan_path_deps_do_r(c$code, path))
  )
}

# along renv_dependencies_discover_chunks_ignore
scan_path_deps_rnw_chunk_is_ignored <- function(chunk) {
  # renv.ignore = TRUE
  if (is_truthy(chunk$params[["renv.ignore"]])) {
    return(TRUE)
  }

  # engine is not R / Rscript
  engine <- chunk$params[["engine"]] %||% "R"
  if (!is.character(engine) || ! tolower(engine) %in% c("r", "rscript")) {
    return(TRUE)
  }

  # eval = FALSE
  eval <- chunk$params[["eval"]]
  if (!is.null(eval) && !is_truthy(eval)) {
    return(TRUE)
  }

  # skip learnr exercises
  if (is_truthy(chunk$params[["exercise"]])) {
    return(TRUE)
  }

  # skip chunks whose labels end in '-display'
  if (endsWith(chunk$params[["label"]] %||% "", "-display")) {
    return(TRUE)
  }

  FALSE
}

scan_path_deps_do_rnw_del_placeholders <- function(chunk) {
  mch <- gregexpr("<<[^>]+>>", chunk$code)
  repl <- lapply(
    mch,
    function(x) strrep(" ", pmax(0, attr(x, "match.length")))
  )
  regmatches(chunk$code, mch) <- repl
  chunk
}

scan_path_deps_do_rnw_chunks <- function(code) {
  ranges <- scan_path_deps_do_rnw_ranges(code)
  from <- viapply(ranges, "[[", 1L)
  to <- viapply(ranges, "[[", 2L)
  chunks <- .mapply(function(from, to) {
    cheader <- code[from]
    ccode <- code[(from+1):to]
    if (ccode[[length(ccode)]] == "@") {
      ccode <- ccode[-length(ccode)]
    }
    scan_path_deps_do_rnw_parse_chunk(cheader, ccode)
  }, list(from, to), NULL)

  # some chunks are ignored
  ignored <- vlapply(chunks, scan_path_deps_rnw_chunk_is_ignored)
  chunks <- chunks[!ignored]

  # remove reused chunk placeholders
  chunks <- lapply(chunks, scan_path_deps_do_rnw_del_placeholders)

  chunks
}

# along xfun::csv_options
scan_path_deps_do_rnw_parse_chunk_header <- function(header) {
  opts <- sub(re_rnw$chunk.begin, "\\1", header)
  # Note: this is not a "real" eval, because we are in an 'alist'
  tryCatch(
    res <- eval(parse(
      text = paste("alist(", xfun_quote_label(opts), ")"),
      keep.source = FALSE
    )),
    error = function(e) {
      stop("Invalid syntax for chunk options: ", opts, "\n", conditionMessage(e))
    }
  )

  idx <- which(names(res) == "")
  j <- NULL
  for (i in idx) if (identical(res[[i]], alist(, )[[1]])) {
    j <- c(j, i)
  }
  if (length(j)) {
    res[j] <- NULL
  }
  idx <- if (is.null(names(res)) && length(res) == 1L) {
    1L
  } else {
    which(names(res) == "")
  }
  if ((n <- length(idx)) > 1L || (length(res) > 1L && is.null(names(res)))) {
    stop(
      "Invalid chunk options: ", res,
      "\n\nAll options must be of the form 'tag=value' except for the chunk label."
    )
  }
  if (is.null(res$label)) {
    if (n == 0L) {
      res$label <- ""
    } else {
      names(res)[idx] <- "label"
    }
  }
  if (!is.character(res$label)) {
    res$label <- gsub(" ", "", as.character(as.expression(res$label)))
  }
  if (res$label == "") {
    res$label <- NULL
  }
  res
}

xfun_quote_label <- function(x) {
  x <- gsub("^\\s*,?", "", x)
  if (grepl("^\\s*[^'\"](,|\\s*$)", x)) {
    x <- gsub("^\\s*([^'\"])(,|\\s*$)", "'\\1'\\2", x)
  } else if (grepl("^\\s*[^'\"](,|[^=]*(,|\\s*$))", x)) {
    x <- gsub("^\\s*([^'\"][^=]*)(,|\\s*$)", "'\\1'\\2", x)
  }
  x
}

scan_path_deps_do_rnw_parse_chunk <- function(header, code) {
  params <- tryCatch(
    scan_path_deps_do_rnw_parse_chunk_header(header),
    error = function(...) list(a=1)[0]
  )
  list(params = params, code = code)
}

scan_path_deps_do_rnw_ranges <- function(code) {
  beg <- grep(re_rnw$chunk.begin, code)
  end <- c(grep(re_rnw$chunk.end, code), length(code) + 1L)
  lapply(seq_along(beg), function(bx) {
    # for every start we find its end. The end is the next end marker,
    # except when there is another begin marker before
    bmx <- beg[bx]
    emx <- end[end > bmx][1]
    if (bx < length(beg) && beg[bx + 1L] < emx) {
      emx <- beg[bx + 1L] - 1L
    }
    c(bmx, emx)
  })
}

# -------------------------------------------------------------------------

scan_path_deps_do_ipynb <- function(code, path) {
  ipynb <- jsonlite::fromJSON(code, simplifyVector = FALSE)
  if (!identical(ipynb$metadata$kernelspec$language, "R")) {
    return(NULL)
  }
  ir <- if (identical(ipynb$metadata$kernelspec$name, "ir")) {
    scan_deps_df(
      path = path,
      package = "IRkernel",
      code = NA_character_
    )
  }

  deps <- lapply(ipynb$cells, function(cell) {
    if (identical(cell$cell_type, "code")) {
      c1 <- paste(unlist(cell$source), collapse = "")
      scan_path_deps_do_r(c1, path)
    }
  })

  adeps <- drop_nulls(c(list(ir), deps))

  do.call("rbind", adeps)
}
