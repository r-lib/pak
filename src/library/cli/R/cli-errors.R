cli_error <- function(
  ...,
  .data = NULL,
  .class = NULL,
  .envir = parent.frame(),
  call. = TRUE
) {
  .hide_from_trace <- TRUE
  cnd <- new_error(
    call. = call.,
    format_error(
      .envir = .envir,
      c(
        ...
      )
    )
  )

  if (length(.data)) cnd[names(.data)] <- .data
  if (length(class)) class(cnd) <- c(.class, class(cnd))

  cnd
}

stop_if_not <- function(
  message,
  ...,
  .envir = parent.frame(),
  call. = sys.call(-1)
) {
  conds <- list(...)
  for (cond in conds) {
    if (!cond) {
      throw(
        new_error(format_error(.envir = .envir, message), call. = call.),
        frame = .envir
      )
    }
  }
}

`%??%` <- function(expr, err) {
  chain_error(expr, err, srcref = utils::getSrcref(sys.call()))
}
