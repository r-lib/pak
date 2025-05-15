# From remotes
parse_submodules <- function(file) {
  if (grepl("\n", file)) {
    # fix windows line endings
    file <- gsub("\r\n", "\n", file, fixed = TRUE)
    x <- strsplit(file, "\n")[[1]]
  } else {
    x <- readLines(file)
  }

  # https://git-scm.com/docs/git-config#_syntax
  # Subsection names are case sensitive and can contain any characters except
  # newline and the null byte. Doublequote " and backslash can be included by
  # escaping them as \" and \\
  double_quoted_string_with_escapes <- '(?:\\\\.|[^"])*'

  # Otherwise extract section names
  section_names <- re_match(
    x,
    sprintf(
      '^[[:space:]]*\\[submodule "(?<submodule>%s)"\\][[:space:]]*$',
      double_quoted_string_with_escapes
    )
  )$submodule

  # If no sections found return the empty list
  if (all(is.na(section_names))) {
    return(list())
  }

  # Extract name = value
  # The variable names are case-insensitive, allow only alphanumeric characters
  # and -, and must start with an alphabetic character.
  variable_name <- "[[:alpha:]][[:alnum:]\\-]*"
  mapping_values <- re_match(
    x,
    sprintf(
      '^[[:space:]]*(?<name>%s)[[:space:]]*=[[:space:]]*(?<value>.*)[[:space:]]*$',
      variable_name
    )
  )

  values <- cbind(
    submodule = fill(section_names),
    mapping_values[c("name", "value")],
    stringsAsFactors = FALSE
  )
  values <- values[!is.na(mapping_values$.match), ]

  # path and valid url are required
  if (!all(c("path", "url") %in% values$name)) {
    warning(
      "Invalid submodule definition, skipping submodule installation",
      immediate. = TRUE,
      call. = FALSE
    )
    return(list())
  }

  # Roughly equivalent to tidyr::spread(values, name, value)
  res <- stats::reshape(
    values,
    idvar = "submodule",
    timevar = "name",
    v.name = "value",
    direction = "wide"
  )

  # Set the column names, reshape prepends `value.` to path, url and branch
  colnames(res) <- gsub("value[.]", "", colnames(res))

  # path and valid url are required
  if (any(is.na(res$url), is.na(res$path))) {
    warning(
      "Invalid submodule definition, skipping submodule installation",
      immediate. = TRUE,
      call. = FALSE
    )
    return(list())
  }

  # branch is optional
  if (!exists("branch", res)) {
    res$branch <- NA_character_
  }

  # Remove unneeded attribute
  attr(res, "reshapeWide") <- NULL

  # Remove rownames
  rownames(res) <- NULL

  res
}

# Adapted from https://stackoverflow.com/a/9517731/2055486
fill <- function(x) {
  not_missing <- !is.na(x)

  res <- x[not_missing]
  res[cumsum(not_missing)]
}

update_submodule <- function(url, path, branch) {
  synchronize(async_update_submodule(url, path, branch)) # nocov
}

async_update_submodule <- function(url, path, branch) {
  url
  path
  branch
  # if the directory already exists and not empty, we assume that
  # it was already downloaded. We still to update the submodules
  # recursively. This is problematic if a git download is interrupted
  # and then stated again with the same output, but that does not happen
  # during normal operation of pkgdepends, I think. A better solution
  # would be to download the submodule to a temporary directory, and if
  # successful, then move the temporary directory to the correct place.
  if (
    file.exists(path) &&
      length(dir(path, all.files = TRUE, no.. = TRUE)) > 0
  ) {
    # message(path, " exists")
    async_update_git_submodules(path)
  } else {
    if (is.null(branch) || is.na(branch)) branch <- "HEAD"
    # message("getting ", path)
    async_git_download_repo(
      url,
      ref = branch,
      output = path,
      submodules = TRUE
    )
  }
}

update_git_submodules_r <- function(path, subdir) {
  synchronize(async_update_git_submodules_r(path, subdir)) # nocov
}

async_update_git_submodules_r <- function(path, subdir) {
  subdir <- subdir %||% "."
  smfile <- file.path(path, ".gitmodules")
  if (!file.exists(smfile)) return()

  info <- parse_submodules(smfile)
  if (length(info) == 0) return()

  to_ignore <- in_r_build_ignore(
    info$path,
    file.path(path, subdir, ".Rbuildignore")
  )
  info <- info[!to_ignore, ]
  if (nrow(info) == 0) return()

  async_map(seq_len(nrow(info)), function(i) {
    async_update_submodule(
      info$url[i],
      file.path(path, info$path[i]),
      info$branch[i]
    )
  })$then(function() invisible())
}

update_git_submodules <- function(path) {
  synchronize(async_update_git_submodules(path))
}

async_update_git_submodules <- function(path) {
  smfile <- file.path(path, ".gitmodules")
  if (!file.exists(smfile)) return()

  info <- parse_submodules(smfile)
  if (nrow(info) == 0) return()

  async_map(seq_len(nrow(info)), function(i) {
    async_update_submodule(
      info$url[i],
      file.path(path, info$path[i]),
      info$branch[i]
    )
  })$then(function() invisible())
}

r_build_ignore_patterns <- c(
  "^\\.Rbuildignore$",
  "(^|/)\\.DS_Store$",
  "^\\.(RData|Rhistory)$",
  "~$",
  "\\.bak$",
  "\\.swp$",
  "(^|/)\\.#[^/]*$",
  "(^|/)#[^/]*#$",
  "^TITLE$",
  "^data/00Index$",
  "^inst/doc/00Index\\.dcf$",
  "^config\\.(cache|log|status)$",
  "(^|/)autom4te\\.cache$",
  "^src/.*\\.d$",
  "^src/Makedeps$",
  "^src/so_locations$",
  "^inst/doc/Rplots\\.(ps|pdf)$"
)

in_r_build_ignore <- function(paths, ignore_file) {
  ignore <- tryCatch(
    asNamespace("tools")$get_exclude_patterns(),
    error = function(e) r_build_ignore_patterns
  )

  if (file.exists(ignore_file)) {
    ignore <- c(ignore, readLines(ignore_file, warn = FALSE))
  }

  matches_ignores <- function(x) {
    any(vlapply(ignore, grepl, x, perl = TRUE, ignore.case = TRUE))
  }

  # We need to search for the paths as well as directories in the path, so
  # `^foo$` matches `foo/bar`
  should_ignore <- function(path) {
    any(vlapply(c(path, directories(path)), matches_ignores))
  }

  vlapply(paths, should_ignore)
}

directories <- function(paths) {
  dirs <- unique(dirname(paths))
  out <- dirs[dirs != "."]
  while (length(dirs) > 0 && any(dirs != ".")) {
    out <- unique(c(out, dirs[dirs != "."]))
    dirs <- unique(dirname(dirs))
  }
  sort(out)
}
