
extra_packages <- function() {
  "tibble"
}

#' @export

pak_install_extra <- function(upgrade = FALSE) {
  extra <- extra_packages()
  pkgs <- paste(extra, collapse = ", ")
  message("i installing extra package: `", pkgs, "`.")
  pak::pkg_install(extra, upgrade = upgrade)
}

load_extra <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (show_extra()) {
      msg <- paste0(
        "! Optional package `", pkg, "` is not available for pak.\n",
        "  Use `pak::pak_install_extra()` to install optional packages.\n",
        "  Use `options(pak.no_extra_messages = TRUE)` to suppress this message.\n"
      )
      once_per_session(message(structure(
        list(message = msg),
        class = c("pak_extra_message", "message", "condition")
      )))
    }
  }
}

show_extra <- function() {
  !isTRUE(getOption("pak.no_extra_messages", FALSE))
}

hash <- function(obj) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  serialize(obj, con <- file(tmp, open = "wb"))
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  close(con)
  tools::md5sum(tmp)[[1]]
}

once_per_session <- local({
  seen <- character()
  function(expr) {
    h <- hash(substitute(expr))
    if (! h %in% seen) {
      seen <<- c(seen, h)
      expr
    }
  }
})

pkg_is_installed <- function(pkg) {
  vlapply(pkg, requireNamespace, quietly = TRUE)
}
