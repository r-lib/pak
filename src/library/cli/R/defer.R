
# NOTE: patched to use `cli_error()`

# nocov start --- compat-defer ---
#
# This drop-in file implements withr::defer(). Please find the most
# recent version in withr's repository.
#
# 2022-03-03
# * Support for `source()` and `knitr::knit()`
# * Handlers are now stored in environments instead of lists to avoid
#   infinite recursion issues.
# * The handler list is now soft-namespaced.


defer <- function(expr, envir = parent.frame(), priority = c("first", "last")) { }

local({

defer <<- defer <- function(expr, envir = parent.frame(), priority = c("first", "last")) {
  priority <- match.arg(priority)
  invisible(
    add_handler(
      envir,
      handler = new_handler(substitute(expr), parent.frame()),
      front = priority == "first"
    )
  )
}

new_handler <- function(expr, envir) {
  hnd <- new.env(FALSE, size = 2)
  hnd[["expr"]] <- expr
  hnd[["envir"]] <- envir
  hnd
}

add_handler <- function(envir,
                        handler,
                        front,
                        frames = as.list(sys.frames()),
                        calls = as.list(sys.calls())) {
  envir <- exit_frame(envir, frames, calls)

  if (front) {
    handlers <- c(list(handler), get_handlers(envir))
  } else {
    handlers <- c(get_handlers(envir), list(handler))
  }

  set_handlers(envir, handlers, frames = frames, calls = calls)
  handler
}

set_handlers <- function(envir, handlers, frames, calls) {
  if (is.null(get_handlers(envir))) {
    # Ensure that list of handlers called when environment "ends"
    setup_handlers(envir)
  }

  attr(envir, "withr_handlers") <- handlers
}

# Evaluate `frames` lazily
setup_handlers <- function(envir,
                           frames = as.list(sys.frames()),
                           calls = as.list(sys.calls())) {
  if (is_top_level_global_env(envir, frames)) {
    # For session scopes we use reg.finalizer()
    if (is_interactive()) {
      message(
        sprintf("Setting global deferred event(s).\n"),
        "i These will be run:\n",
        "  * Automatically, when the R session ends.\n",
        "  * On demand, if you call `withr::deferred_run()`.\n",
        "i Use `withr::deferred_clear()` to clear them without executing."
      )
    }
    reg.finalizer(envir, function(env) deferred_run(env), onexit = TRUE)
  } else {
    # for everything else we use on.exit()

    call <- make_call(execute_handlers, envir)
    # We have to use do.call here instead of eval because of the way on.exit
    # determines its evaluation context
    # (https://stat.ethz.ch/pipermail/r-devel/2013-November/067867.html)

    do.call(base::on.exit, list(call, TRUE), envir = envir)
  }
}

exit_frame <- function(envir,
                       frames = as.list(sys.frames()),
                       calls = as.list(sys.calls())) {
  frame_loc <- frame_loc(envir, frames)
  if (!frame_loc) {
    return(envir)
  }

  if (in_knitr(envir)) {
    out <- knitr_frame(envir, frames, calls, frame_loc)
    if (!is.null(out)) {
      return(out)
    }
  }

  out <- source_frame(envir, frames, calls, frame_loc)
  if (!is.null(out)) {
    return(out)
  }

  envir
}

knitr_frame <- function(envir, frames, calls, frame_loc) {
  knitr_ns <- asNamespace("knitr")

  # This doesn't handle correctly the recursive case (knitr called
  # within a chunk). Handling this would be a little fiddly for an
  # uncommon edge case.
  for (i in seq(1, frame_loc)) {
    if (identical(topenv(frames[[i]]), knitr_ns)) {
      return(frames[[i]])
    }
  }

  NULL
}

source_frame <- function(envir, frames, calls, frame_loc) {
  i <- frame_loc

  if (i < 4) {
    return(NULL)
  }

  is_call <- function(x, fn) {
    is.call(x) && identical(x[[1]], fn)
  }
  calls <- as.list(calls)

  if (!is_call(calls[[i - 3]], quote(source))) {
    return(NULL)
  }
  if (!is_call(calls[[i - 2]], quote(withVisible))) {
    return(NULL)
  }
  if (!is_call(calls[[i - 1]], quote(eval))) {
    return(NULL)
  }
  if (!is_call(calls[[i - 0]], quote(eval))) {
    return(NULL)
  }

  frames[[i - 3]]
}

frame_loc <- function(envir, frames) {
  n <- length(frames)
  if (!n) {
    return(0)
  }

  for (i in seq_along(frames)) {
    if (identical(frames[[n - i + 1]], envir)) {
      return(n - i + 1)
    }
  }

  0
}

in_knitr <- function(envir) {
  knitr_in_progress() && identical(knitr::knit_global(), envir)
}

is_top_level_global_env <- function(envir, frames) {
  if (!identical(envir, globalenv())) {
    return(FALSE)
  }

  # Check if another global environment is on the stack
  !any(vapply(frames, identical, NA, globalenv()))
}

get_handlers <- function(envir) {
  attr(envir, "withr_handlers")
}

execute_handlers <- function(envir) {
  handlers <- get_handlers(envir)
  errors <- list()
  for (handler in handlers) {
    tryCatch(eval(handler$expr, handler$envir),
      error = function(e) {
        errors[[length(errors) + 1]] <<- e
      }
    )
  }
  attr(envir, "withr_handlers") <- NULL

  for (error in errors) {
    stop(error) %??%
      cli_error("Error in a deferred {.code on.exit()} clause")
  }
}

make_call <- function(...) {
  as.call(list(...))
}

# base implementation of rlang::is_interactive()
is_interactive <- function() {
  opt <- getOption("rlang_interactive")
  if (!is.null(opt)) {
    return(opt)
  }
  if (knitr_in_progress()) {
    return(FALSE)
  }
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(FALSE)
  }
  interactive()
}

knitr_in_progress <- function() {
  isTRUE(getOption("knitr.in.progress"))
}

}) # defer() namespace

# nocov end
