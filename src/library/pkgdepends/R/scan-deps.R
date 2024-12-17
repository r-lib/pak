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
#' @param path Path to the directory of the project.
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

scan_deps <- function(path = ".") {
  path <- tryCatch(find_project_root(path), error = function(...) path)
  paths <- dir(path, pattern = "[.](R|r|Rmd|rmd)$", recursive = TRUE)
  full_paths <- normalizePath(file.path(path, paths))
  deps_list <- lapply(full_paths, scan_path_deps)
  deps <- do.call("rbind", c(list(scan_path_deps_empty()), deps_list))
  # write back the relative paths
  deps$path <- paths[match(deps$path, full_paths)]
  deps$type <- get_dep_type_from_path(deps$path)
  deps
}

# -------------------------------------------------------------------------

# needs to increase as the deps discovry code changes, otherwise we don't
# apply the new discovery code
deps_cache_version <- 1L

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
  code <- readBin(path, "raw", file.size(path))

  # check if already known, set path
  hash <- cli::hash_raw_xxhash(code)
  cache <- get_deps_cache_path(hash)
  if (file.exists(cache)) {
    deps <- readRDS(cache)
    if (!is.null(deps) && nrow(deps) > 0) {
      deps$path <- path
      deps$type <- get_dep_type_from_path(path)
    }
    return(deps)
  }

  # scan it if it is worth it, based on a quick check
  has_deps <- length(grepRaw(re_r_dep(), code)) > 0
  deps <- if (has_deps) scan_path_deps_do(code, path)

  # save it to the cache, but anonimize it first. If no deps, save NULL
  deps_no_path <- deps
  if (!is.null(deps_no_path) && nrow(deps_no_path) > 0) {
    deps_no_path$path <- ""
    deps_no_path$type <- NA_character_
  }
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  saveRDS(deps_no_path, cache)

  deps
}

scan_path_deps_empty <- function() {
  data_frame(
    path = character(),
    package = character(),
    type = character(),
    code = character(),
    start_row = integer(),
    start_column = integer(),
    start_byte = integer()
  )
}

scan_path_deps_do <- function(code, path) {
  ext <- tolower(file_ext(path))
  switch(
    ext,
    ".r" = scan_path_deps_do_r(code, path),
    ".qmd" = ,
    ".rmd" = scan_path_deps_do_rmd(code, path),
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
  data_frame(
    path = path,
    package = hits$code[hits$name == "pkg-name"],
    type = get_dep_type_from_path(path),
    code = hits$code[hits$name == "dep-code"],
    start_row = hits$start_row[hits$name == "dep-code"],
    start_column = hits$start_column[hits$name == "dep-code"],
    start_byte = hits$start_byte[hits$name == "dep-code"]
  )
}

scan_path_deps_do_fn_hits <- function(hits, path) {
  fn_pkg_map <- c(setClass = "methods", setGeneric = "methods")
  fn_names <- hits$code[hits$name == "fn-name"]
  data_frame(
    path = path,
    package = fn_pkg_map[fn_names],
    type = get_dep_type_from_path(path),
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
  data_frame(
    path = path,
    package = unlist(pkgs),
    type = get_dep_type_from_path(path),
    code = rep(code, pkgs_count),
    start_row = rep(hits$start_row[hits$name == "dep-code"], pkgs_count),
    start_column = rep(hits$start_column[hits$name == "dep-code"], pkgs_count),
    start_byte = rep(hits$start_byte[hits$name == "dep-code"], pkgs_count)
  )
}

scan_path_deps_do_jr_hits <- function(hits, path) {
  code <- hits$code[hits$name == "dep-code"]
  data_frame(
    path = path,
    package = "xml2",
    type = get_dep_type_from_path(path),
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
      return(data_frame(
        path = path,
        package = "ragg",
        type = get_dep_type_from_path(path),
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
  data_frame(
    path = path,
    package = pkgs,
    type = get_dep_type_from_path(path),
    code = hits$code,
    start_row = hits$start_row,
    start_column = hits$start_column,
    start_byte = hits$start_byte
  )
}

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

safe_parse_pkg_from_call <- function(ns, fn, code) {
  tryCatch(
    parse_pkg_from_call(ns, fn, code),
    error = function(...) NULL
  )
}

parse_pkg_from_call <- function(ns, fn, code) {
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
  matched <- match.call(fun, expr, expand.dots = FALSE)
  switch(fn,
    "library" = , "require" =
      parse_pkg_from_call_library(ns, fs, matched),
    "loadNamespace" = , "requireNamespace" =
      parse_pkg_from_call_loadNamespace(ns, fn, matched),
    "pkg_attache" = , "pkg_attach2" =
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

parse_pkg_from_call_loadNamespace <- function(ns, fn, matched) {
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
  if (char[[1]] == quote(c)) {
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
  blk_hits <- hits$matched_captures[
    ! hits$matched_captures$pattern %in% inl_pat, ]
  rbind(
    if (nrow(inl_hits)) scan_path_deps_do_inline_hits(code, inl_hits, path),
    if (nrow(blk_hits)) scan_path_deps_do_block_hits(code, blk_hits, path)
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
