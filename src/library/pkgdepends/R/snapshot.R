snapshot <- function(x, width = Inf, ...) {
  UseMethod("snapshot")
}

snapshot.default <- function(x, width = Inf, ...) {
  if (width == Inf) width <- 10000
  print(x, width = width, ...)
}

#' @export

print.snapshot <- function(x, ...) {
  writeLines(x)
}

#' @export

snapshot.pkg_resolution_result <- function(x, width = Inf, extra = NULL, ...) {
  if (!"pillar" %in% loadedNamespaces()) {
    throw(pkg_error("Needs pillar loaded!"))
  }
  if ("md5sum" %in% colnames(x)) {
    x$md5sum <- sub("^[0-9a-fA-F]{32}$", "<md5>", x$md5sum)
  }
  if ("sha256" %in% colnames(x)) {
    x$sha256 <- sub("^[0-9a-fA-F]{64}$", "<sha256>", x$sha256)
  }
  if ("filesize" %in% colnames(x)) {
    x$filesize[!is.na(x$filesize)] <- 42L
  }

  chr <- format(x, width = width, ...)

  if ("sources" %in% colnames(x) && any(c("all", "sources") %in% extra)) {
    chr <- c(chr, "+ sources:", vcapply(x$sources, paste, collapse = ", "))
  }
  if ("remote" %in% colnames(x) && any(c("all", "remote") %in% extra)) {
    chr <- c(chr, "+ remote:", vcapply(x$remote, format))
  }
  if ("error" %in% colnames(x) && any(c("all", "error") %in% extra)) {
    chr <- c(chr, "+ error:", vcapply(x$error, format_error))
  }
  ## TODO: error
  if ("metadata" %in% colnames(x) && any(c("all", "metadata") %in% extra)) {
    chr <- c(chr, "+ metadata:", vcapply(x$metadata, format.pkg_metadata))
  }
  ## TODO: extra
  if ("dep_types" %in% colnames(x) && any(c("all", "dep_types") %in% extra)) {
    chr <- c(chr, "+ dep_types:", vcapply(x$dep_types, paste, collapse = ", "))
  }
  ## TODO: params

  structure(chr, class = "snapshot")
}

#' @export

format.remote_ref <- function(x, ...) {
  x$params <- paste(vcapply(x$params, format), collapse = ", ")
  paste0(
    "<",
    paste(class(x), collapse = "/"),
    "> ",
    paste0(names(x), ": ", vcapply(x, format), collapse = "; ")
  )
}

format.pkg_metadata <- function(x, ...) {
  if (length(x) == 0) return("-")
  paste0(names(x), ": ", vcapply(x, format), collapse = "; ")
}

format_error <- function(x, ...) {
  if (length(x) == 0) return("-")
  paste0(
    "<",
    paste0(class(x), collapse = "/"),
    "> ",
    conditionMessage(x)
  )
}
