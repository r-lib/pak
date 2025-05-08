copy_package_tree <- function(
  path = ".",
  dest,
  pkgname = desc::desc_get("Package", path)
) {
  if (!file.exists(dest)) mkdirp(dest)

  pkgdir <- file.path(dest, pkgname)
  if (file.exists(pkgdir)) {
    stop(cli::format_error(c(
      "Cannot copy package tree to {.path {dest}}",
      i = "Directory {.path {pkgdir}} already exists, and did not want
          to overwrite."
    )))
  }

  mkdirp(pkgdir)

  paths <- build_files(path, pkgname)

  method <- get_copy_method(path)

  for (i in seq_len(nrow(paths))) {
    # excluded, skip
    if (paths$exclude[i]) next
    if (paths$isdir[i] && paths$trimmed[i]) {
      # trimmed directory, only create directory
      # we do not try to update the mode of the directory, as it is not
      # very important for R packages, and it might fail on some file
      # systems.
      mkdirp(file.path(pkgdir, paths$path[i]))
    } else {
      # not a directory, or a non-trimmed directory, recurse
      src <- paths$realpath[i]
      tgt <- file.path(pkgdir, paths$path[i])
      if (method == "link") {
        file.symlink(src, tgt)
      } else if (paths$isdir[i]) {
        cp(src, dirname(tgt), recursive = TRUE)
      } else {
        cp(src, tgt)
      }
    }
  }

  # TODO: should we return a trimming summary?
  invisible()
}

get_copy_method <- function(path = ".") {
  method <- getOption("pkg.build_copy_method", NA_character_)
  values <- c("none", "copy", "link")
  check_method <- function(method) {
    # no symlinks on Windows
    if (method == "link" && is_windows()) method <- "copy"
    if (method %in% values) return(method)
    stop(cli::format_error(c(
      "Invalid {.code pkg.build_copy_method} value: {.val {method}}.",
      i = "It must be one of {.str {values}}."
    )))
  }
  if (!is_string(method) && !is_na(method)) {
    stop(cli::format_error(c(
      "Invalid {.code pkg.build_copy_method} value.",
      i = "It must be a string, but it is {.type {method}}."
    )))
  }
  if (!is.na(method)) return(check_method(method))

  method <- desc::desc_get("Config/build/copy-method", path)
  if (!is.na(method)) return(check_method(method))

  method <- Sys.getenv("PKG_BUILD_COPY_METHOD", "none")
  check_method(method)
}

build_files <- function(
  path = ".",
  pkgname = desc::desc_get("Package", path)
) {
  path <- normalizePath(path)

  # patterns in .Rbuildignore
  ign_file <- file.path(path, ".Rbuildignore")
  ign <- if (file.exists(ign_file)) {
    readLines(ign_file, warn = FALSE)
  } else {
    character()
  }
  ign <- ign[nzchar(ign)]
  # make it case insensitive, that's how R matches them
  if (length(ign)) ign <- paste0("(?i)", ign)

  ptrn <- c(ign, re_exclude(pkgname))
  ptrn_dir <- re_exclude_dir(pkgname)

  # filter at the top level first, so we don't need to enumerate these
  top <- dir(path, include.dirs = TRUE, all.files = TRUE, no.. = TRUE)

  # now filter top
  realtop <- file.path(path, top)
  topfls <- data.frame(
    stringsAsFactors = FALSE,
    path = top,
    realpath = realtop,
    exclude = logical(length(top)),
    isdir = file.info(realtop)$isdir
  )
  topfls <- exclude(path, topfls, ptrn, ptrn_dir)

  # now create the rest of the files
  sub <- unlist(lapply(
    topfls$path[topfls$isdir & !topfls$exclude],
    function(t) {
      tf <- dir(
        file.path(path, t),
        include.dirs = TRUE,
        all.files = TRUE,
        recursive = TRUE
      )
      tf <- file.path(t, tf)
    }
  ))

  realsub <- file.path(path, sub)
  subfls <- data.frame(
    stringsAsFactors = FALSE,
    path = sub,
    realpath = realsub,
    exclude = logical(length(sub)),
    isdir = file.info(realsub)$isdir
  )
  subfls <- exclude(path, subfls, ptrn, ptrn_dir)

  allfls <- rbind(topfls, subfls)

  # Always keep this, so base R can do its own filtering, just in case
  # it changes compared to ours
  allfls$exclude[allfls$path == ".Rbuildignore"] <- FALSE

  allfls <- exclude_downstream(allfls)

  allfls
}

re_exclude <- function(pkg) {
  c(
    paste0(
      "(?i)", # these are case insensitive
      c(
        "(^|/)\\.DS_Store$", # by macOS finder
        "^\\.RData$", # .RData at /
        "~$",
        "\\.bak$",
        "\\.swp$", # backup files
        "(^|/)\\.#[^/]*$",
        "(^|/)#[^/]*#$", # more backup files (Emacs)

        "^config\\.(cache|log|status)$", # leftover by autoconf
        "(^|/)autom4te\\.cache$",

        "^src/.*\\.d$",
        "^src/Makedeps$", # INSTALL leftover on Windows

        "^inst/doc/Rplots\\.(ps|pdf)$" # Sweave leftover
      )
    ),

    "(^|/)\\._[^/]*$", # macOS resource forks

    paste0(
      # hidden files
      "(^|/)\\.",
      c(
        "Renviron",
        "Rprofile",
        "Rproj.user",
        "Rhistory",
        "Rapp.history",
        "tex",
        "log",
        "aux",
        "pdf",
        "png",
        "backups",
        "cvsignore",
        "cproject",
        "directory",
        "dropbox",
        "exrc",
        "gdb.history",
        "gitattributes",
        "github",
        "gitignore",
        "gitmodules",
        "hgignore",
        "hgtags",
        "htaccess",
        "latex2html-init",
        "project",
        "seed",
        "settings",
        "tm_properties"
      ),
      "$"
    ),

    paste0(
      "(^|/)",
      pkg,
      "_[0-9.-]+",
      "\\.(tar\\.gz|tar|tar\\.bz2|tar\\.xz|tgz|zip)",
      "$"
    )
  )
}

re_exclude_dir <- function(pkg) {
  c(
    "^revdep$", # revdepcheck
    paste0(
      # VC
      "(^|/)",
      c(
        "CVS",
        ".svn",
        ".arch-ids",
        ".bzr",
        ".git",
        ".hg",
        "_darcs",
        ".metadata"
      ),
      "$"
    ),

    "(^|/)[^/]*[Oo]ld$",
    "(^|/)[^/]*\\.Rcheck",

    "^src.*/\\.deps$"
  )
}

exclude <- function(root, paths, patterns, patterns_dir) {
  ex <- logical(nrow(paths))
  for (p in patterns) {
    ex <- ex | grepl(p, paths$path, perl = TRUE)
  }

  # additional regexes for directories, we don't need to check 'ex'
  wdirs <- !ex & paths$isdir

  paths_dirs <- paths$path[wdirs]
  dex <- logical(length(paths_dirs))
  for (p in patterns_dir) {
    dex <- dex | grepl(p, paths_dirs, perl = TRUE)
  }
  ex[wdirs][dex] <- TRUE

  paths$exclude <- ex
  paths
}

exclude_downstream <- function(paths) {
  # We don't actually need this now, but we could use it to optimize,
  # because we could trim only the subsequent elements after a directory.
  paths <- paths[order(paths$path), ]

  # We need to take each excluded directory, and remove all paths in them
  exdirs <- paste0(paths$path[paths$isdir & paths$exclude], "/")
  del <- logical(nrow(paths))
  for (ed in exdirs) {
    del <- del | startsWith(paths$path, ed)
  }
  paths <- paths[!del, ]

  # Now we mark the subdirectories that can be copied as is, i.e. none of
  # their downstream contents are excluded
  indirs <- which(paths$isdir & !paths$exclude)
  trm <- logical(nrow(paths))
  for (id in indirs) {
    trm[id] <- any(
      startsWith(paths$path, paste0(paths$path[id], "/")) & paths$exclude
    )
  }
  paths$trimmed <- trm

  # We can remove the subdirectories of non-trimmed directories as well
  fulldirs <- paste0(paths$path[paths$isdir & !trm], "/")
  del2 <- logical(nrow(paths))
  for (fd in fulldirs) {
    del2 <- del2 | startsWith(paths$path, fd)
  }
  paths <- paths[!del2, ]

  rownames(paths) <- NULL
  paths
}

cp <- local({
  wind <- NULL
  cpargs <- NULL
  function(src, tgt, recursive = FALSE) {
    if (is.null(wind)) wind <<- is_windows()
    if (wind) {
      if (!file.copy(src, tgt, recursive = recursive, copy.date = TRUE)) {
        stop(cli::format_error(c(
          "Could not copy package files.",
          i = "Failed to copy {.path {src}} to {.path {tgt}}."
        )))
      }
    } else {
      if (is.null(cpargs)) cpargs <<- detect_cp_args()
      ret <- processx::run(
        "cp",
        c(cpargs, src, tgt),
        stderr = "2>&1",
        error_on_status = FALSE
      )
      if (ret$status != 0) {
        stop(cli::format_error(c(
          "Could not copy package files.",
          i = "Failed to copy {.path {src}} to {.path {tgt}}.",
          i = "{.code cp} output:",
          " " = verb_for_cli(ret$stdout)
        )))
      }
    }
  }
})

detect_cp_args <- function() {
  # we do this in a tempdir, because it might create a file a called
  # `--preserve=timestamps`
  dir.create(tmp <- tempfile())
  old <- getwd()
  on.exit(
    {
      setwd(old)
      unlink(tmp, recursive = TRUE)
    },
    add = TRUE
  )
  setwd(tmp)
  f1 <- basename(tempfile())
  f2 <- basename(tempfile())
  file.create(f1)
  tryCatch(
    suppressWarnings(processx::run("cp", c("--preserve=timestamps", f1, f2))),
    error = function(e) e
  )
  if (file.exists(f2)) {
    c("-LR", "--preserve=timestamps")
  } else {
    "-pLR"
  }
}
