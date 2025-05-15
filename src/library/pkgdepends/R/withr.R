withr_with_path <- function(
  new,
  code,
  action = c("prefix", "suffix", "replace")
) {
  old <- withr_get_path(path = new, action = action)
  on.exit((function(old) withr_set_path(old, "replace"))(old))
  withr_set_path(path = new, action = action)
  force(code)
}

withr_set_path <- function(path, action = c("prefix", "suffix", "replace")) {
  action <- match.arg(action)
  path <- withr_as_character(path)
  path <- normalizePath(path, mustWork = FALSE)
  old <- withr_get_path()
  path <- withr_merge_new(old, path, action)
  path <- paste(path, collapse = .Platform$path.sep)
  Sys.setenv(PATH = path)
  invisible(old)
}

withr_merge_new <- function(old, new, action, merge_fun = c) {
  action <- match.arg(action, c("replace", "prefix", "suffix"))
  if (action == "suffix") {
    new <- merge_fun(old, new)
  } else if (action == "prefix") {
    new <- merge_fun(new, old)
  }
  new
}

withr_get_path <- function(...) {
  strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
}

withr_local_path <- function(
  new = list(),
  action = c("prefix", "suffix", "replace"),
  .local_envir = parent.frame()
) {
  old <- withr_get_path(path = new, action = action)
  withr_defer(
    (function(old) withr_set_path(old, "replace"))(old),
    frame = .local_envir
  )
  withr_set_path(path = new, action = action)
  invisible(old)
}

# -------------------------------------------------------------------------

withr_defer <- function(expr, frame = parent.frame(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, add = TRUE, after = after), envir = frame)
}

# -------------------------------------------------------------------------

withr_with_libpaths <- function(new, code, action = "replace") {
  old <- withr_get_libpaths(paths = new, action = action)
  on.exit(.libPaths(old))
  withr_set_libpaths(paths = new, action = action)
  force(code)
}

withr_get_libpaths <- function(...) {
  .libPaths()
}

withr_set_libpaths <- function(paths, action = "replace") {
  paths <- withr_as_character(paths)
  paths <- normalizePath(paths, mustWork = TRUE)
  old <- .libPaths()
  paths <- withr_merge_new(old, paths, action)
  .libPaths(paths)
  invisible(old)
}

withr_as_character <- function(x) {
  nms <- names(x)
  res <- as.character(x)
  names(res) <- nms
  res
}

# -------------------------------------------------------------------------

withr_get_envvar <- function(envs, action = "replace") {
  envs <- withr_as_envvars(envs)
  Sys.getenv(names(envs), names = TRUE, unset = NA)
}

withr_set_envvar <- function(envs, action = "replace") {
  envs <- withr_as_envvars(envs)
  stopifnot(is.character(action), length(action) == 1)
  action <- match.arg(action, c("replace", "prefix", "suffix"))
  if (length(envs) == 0) {
    return()
  }
  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)
  both_set <- set & !is.na(old)
  if (any(both_set)) {
    if (action == "prefix") {
      envs[both_set] <- paste(envs[both_set], old[both_set])
    } else if (action == "suffix") {
      envs[both_set] <- paste(old[both_set], envs[both_set])
    }
  }
  if (any(set)) do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set)) Sys.unsetenv(names(envs)[!set])
  invisible(old)
}

withr_with_envvar <- function(new, code, action = "replace") {
  old <- withr_get_envvar(envs = new, action = action)
  on.exit(withr_set_envvar(old))
  withr_set_envvar(envs = new, action = action)
  force(code)
}

withr_as_envvars <- function(envs) {
  if (length(envs) == 0) {
    return(envs)
  }
  stopifnot(withr_is_named(envs))
  envs[withr_vlapply(envs, is.null)] <- NA
  envs <- envs[!duplicated(names(envs), fromLast = TRUE)]
  envs
}

withr_vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}

withr_is_named <- function(x) {
  !is.null(names(x)) && all(names(x) != "")
}

# -------------------------------------------------------------------------
