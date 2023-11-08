
idesc_get_version <- function(self, private) {
  ver <- unname(self$get("Version"))
  if (is.na(ver)) stop("No ", sQuote('Version'), " field found")
  package_version(ver)
}

idesc_set_version <- function(self, private, version) {
  stopifnot(is_package_version(version))
  self$set(Version = as.character(version))
}

idesc_bump_version <- function(self, private, which) {
  stopifnot(is_version_component(which))
  if (is.character(which)) {
    which <- match(which, c("major", "minor", "patch", "dev"))
  }

  ver_str <- self$get_version()
  ver <- get_version_components(ver_str)

  ## Special dev version
  inc <- if (which == 4 && length(ver) < 4) 9000 else 1

  ## Missing components are equivalent to zero
  if (which > length(ver)) ver <- c(ver, rep(0, which - length(ver)))

  ## Bump selected component
  ver[which] <- ver[which] + inc

  ## Zero out everything after
  if (which < length(ver)) ver[(which+1):length(ver)] <- 0

  ## Keep at most three components if they are zero
  if (length(ver) > 3 && all(ver[4:length(ver)] == 0)) {
    ver <- ver[1:3]
  }

  ## Set the new version
  new_ver <- package_version(paste(ver, collapse = "."))
  self$set_version(new_ver)

  ## Give a message
  desc_message(
    "Package version bumped from ", sQuote(ver_str), " to ", sQuote(new_ver)
  )

  invisible(self)
}

## Note that this is not vectorized

get_version_components <- function(x) {
  as.numeric(strsplit(format(x), "[-\\.]")[[1]])
}
