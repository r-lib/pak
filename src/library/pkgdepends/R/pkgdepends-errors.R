pkg_error <- function(
  ...,
  .data = NULL,
  .class = NULL,
  .envir = parent.frame(),
  call. = TRUE
) {
  .hide_from_trace <- TRUE
  cnd <- new_error(
    call. = call.,
    cli::format_error(
      .envir = .envir,
      c(
        ...
      )
    )
  )

  if (length(.data)) cnd[names(.data)] <- .data
  if (length(.class)) class(cnd) <- c(.class, class(cnd))

  cnd
}

pkg_warning <- function(
  ...,
  .data = NULL,
  .class = NULL,
  .envir = parent.frame(),
  call. = TRUE
) {
  .hide_from_trace <- TRUE
  cnd <- new_cond(
    call. = call.,
    cli::format_error(
      .envir = .envir,
      c(
        ...
      )
    )
  )
  class(cnd) <- c(.class, "warning", "condition")

  if (length(.data)) cnd[names(.data)] <- .data
  if (length(.class)) class(cnd) <- c(.class, class(cnd))

  cnd
}

stop <- function(..., call. = TRUE, domain = NA) {
  .hide_from_trace <- TRUE
  args <- list(...)
  if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    throw(
      add_class(args[[1]], c("rlib_error_3_0", "rlib_error"), "end"),
      frame = parent.frame()
    )
  } else {
    throw(new_error(..., call. = call., domain = domain))
  }
}

stopifnot <- function(...) {
  assert_that(..., env = parent.frame())
}

msg_internal_error <- function() {
  "This is an internal error in {pak_or_pkgdepends()}, please report
   an issue at {.url https://github.com/r-lib/{pak_or_pkgdepends()}/issues}."
}

msg_package_sources <- function() {
  if (is_pak()) {
    "See {.help pak::pak_package_sources} for the package sources
     pak supports."
  } else {
    "See {.help pkgdepends::pkg_refs} for supported package sources."
  }
}
