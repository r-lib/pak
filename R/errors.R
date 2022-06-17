
# # Standalone file for better error handling ----------------------------
#
# If can allow package dependencies, then you are probably better off
# using rlang's functions for errors.
#
# The canonical location of this file is in the processx package:
# https://github.com/r-lib/processx/blob/main/R/errors.R
#
# ## Dependencies
# - rstudio-detect.R for better printing in RStudio
#
# ## Features
#
# - Throw conditions and errors with the same API.
# - Automatically captures the right calls and adds them to the conditions.
# - Sets `.Last.error`, so you can easily inspect the errors, even if they
#   were not caught.
# - It only sets `.Last.error` for the errors that are not caught.
# - Hierarchical errors, to allow higher level error messages, that are
#   more meaningful for the users, while also keeping the lower level
#   details in the error object. (So in `.Last.error` as well.)
# - `.Last.error` always includes a stack trace. (The stack trace is
#   common for the whole error hierarchy.) The trace is accessible within
#   the error, e.g. `.Last.error$trace`. The trace of the last error is
#   also at `.Last.error.trace`.
# - Can merge errors and traces across multiple processes.
# - Pretty-print errors and traces, if the cli package is loaded.
# - Automatically hides uninformative parts of the stack trace when
#   printing.
#
# ## API
#
# ```
# new_cond(..., call. = TRUE, srcref = NULL, domain = NA)
# new_error(..., call. = TRUE, srcref = NULL, domain = NA)
# throw(cond, parent = NULL, frame = environment())
# throw_error(cond, parent = NULL, frame = environment())
# chain_error(expr, err, call = sys.call(-1))
# chain_call(.NAME, ...)
# chain_clean_call(.NAME, ...)
# onload_hook()
# add_trace_back(cond, frame = NULL)
# format$advice(x)
# format$call(call)
# format$class(x)
# format$error(x, trace = FALSE, class = FALSE, advice = !trace, ...)
# format$error_heading(x, prefix = NULL)
# format$header_line(x, prefix = NULL)
# format$srcref(call, srcref = NULL)
# format$trace(x, ...)
# ```
#
# ## Roadmap:
# - better printing of anonymous function in the trace
#
# ## NEWS:
#
# ### 1.0.0 -- 2019-06-18
#
# * First release.
#
# ### 1.0.1 -- 2019-06-20
#
# * Add `rlib_error_always_trace` option to always add a trace
#
# ### 1.0.2 -- 2019-06-27
#
# * Internal change: change topenv of the functions to baseenv()
#
# ### 1.1.0 -- 2019-10-26
#
# * Register print methods via onload_hook() function, call from .onLoad()
# * Print the error manually, and the trace in non-interactive sessions
#
# ### 1.1.1 -- 2019-11-10
#
# * Only use `trace` in parent errors if they are `rlib_error`s.
#   Because e.g. `rlang_error`s also have a trace, with a slightly
#   different format.
#
# ### 1.2.0 -- 2019-11-13
#
# * Fix the trace if a non-thrown error is re-thrown.
# * Provide print_this() and print_parents() to make it easier to define
#   custom print methods.
# * Fix annotating our throw() methods with the incorrect `base::`.
#
# ### 1.2.1 -- 2020-01-30
#
# * Update wording of error printout to be less intimidating, avoid jargon
# * Use default printing in interactive mode, so RStudio can detect the
#   error and highlight it.
# * Add the rethrow_call_with_cleanup function, to work with embedded
#   cleancall.
#
# ### 1.2.2 -- 2020-11-19
#
# * Add the `call` argument to `catch_rethrow()` and `rethrow()`, to be
#   able to omit calls.
#
# ### 1.2.3 -- 2021-03-06
#
# * Use cli instead of crayon
#
# ### 1.2.4 -- 2021-04-01
#
# * Allow omitting the call with call. = FALSE in `new_cond()`, etc.
#
# ### 1.3.0 -- 2021-04-19
#
# * Avoid embedding calls in trace with embed = FALSE.
#
# ### 2.0.0 -- 2021-04-19
#
# * Versioned classes and print methods
#
# ### 2.0.1 -- 2021-06-29
#
# * Do not convert error messages to native encoding before printing,
#   to be able to print UTF-8 error messages on Windows.
#
# ### 2.0.2 -- 2021-09-07
#
# * Do not translate error messages, as this converts them to the native
#   encoding. We keep messages in UTF-8 now.
#
# ### 3.0.0 -- 2022-04-19
#
# * Major rewrite, use rlang compatible error objects. New API.
#
# ##3 3.0.1 -- 2022-06-17
#
# * Remove the `rlang_error` and `rlang_trace` classes, because our new
#   deparsed `call` column in the trace is not compatible with rlang.

err <- local({

  # -- dependencies -----------------------------------------------------
  rstudio_detect <- rstudio$detect

  # -- condition constructors -------------------------------------------

  #' Create a new condition
  #'
  #' @noRd
  #' @param ... Parts of the error message, they will be converted to
  #'   character and then concatenated, like in [stop()].
  #' @param call. A call object to include in the condition, or `TRUE`
  #'   or `NULL`, meaning that [throw()] should add a call object
  #'   automatically. If `FALSE`, then no call is added.
  #' @param srcref Alternative source reference object to use instead of
  #'   the one of `call.`.
  #' @param domain Translation domain, see [stop()]. We set this to
  #'   `NA` by default, which means that no translation occurs. This
  #'   has the benefit that the error message is not re-encoded into
  #'   the native locale.
  #' @return Condition object. Currently a list, but you should not rely
  #'   on that.

  new_cond <- function(..., call. = TRUE, srcref = NULL, domain = NA) {
    message <- .makeMessage(..., domain = domain)
    structure(
      list(message = message, call = call., srcref = srcref),
      class = c("condition"))
  }

  #' Create a new error condition
  #'
  #' It also adds the `rlib_error` class.
  #'
  #' @noRd
  #' @param ... Passed to [new_cond()].
  #' @param call. Passed to [new_cond()].
  #' @param srcref Passed tp [new_cond()].
  #' @param domain Passed to [new_cond()].
  #' @return Error condition object with classes `rlib_error`, `error`
  #'   and `condition`.

  new_error <- function(..., call. = TRUE, srcref = NULL, domain = NA) {
    cond <- new_cond(..., call. = call., domain = domain, srcref = srcref)
    class(cond) <- c("rlib_error_3_0", "rlib_error", "error", "condition")
    cond
  }

  # -- throwing conditions ----------------------------------------------

  #' Throw a condition
  #'
  #' If the condition is an error, it will also call [stop()], after
  #' signalling the condition first. This means that if the condition is
  #' caught by an exiting handler, then [stop()] is not called.
  #'
  #' @noRd
  #' @param cond Condition object to throw. If it is an error condition,
  #'   then it calls [stop()].
  #' @param parent Parent condition.
  #' @param frame The throwing context. Can be used to hide frames from
  #'   the backtrace.

  throw <- throw_error <- function(cond, parent = NULL, frame = environment()) {
    if (!inherits(cond, "condition")) {
      cond <- new_error(cond)
    }
    if (!is.null(parent) && !inherits(parent, "condition")) {
      throw(new_error("Parent condition must be a condition object"))
    }

    if (isTRUE(cond$call)) {
      cond$call <- sys.call(-1) %||% sys.call()
    } else if (identical(cond$call, FALSE)) {
      cond$call <- NULL
    }

    cond <- process_call(cond)

    if (!is.null(parent)) {
      cond$parent <- process_call(parent)
    }

    # We can set an option to always add the trace to the thrown
    # conditions. This is useful for example in context that always catch
    # errors, e.g. in testthat tests or knitr. This options is usually not
    # set and we signal the condition here
    always_trace <- isTRUE(getOption("rlib_error_always_trace"))
    .hide_from_trace <- 1L
    # .error_frame <- cond
    if (!always_trace) signalCondition(cond)

    if (is.null(cond$`_pid`)) cond$`_pid` <- Sys.getpid()
    if (is.null(cond$`_timestamp`)) cond$`_timestamp` <- Sys.time()

    # If we get here that means that the condition was not caught by
    # an exiting handler. That means that we need to create a trace.
    # If there is a hand-constructed trace already in the error object,
    # then we'll just leave it there.
    if (is.null(cond$trace)) cond <- add_trace_back(cond, frame = frame)

    # Set up environment to store .Last.error, it will be just before
    # baseenv(), so it is almost as if it was in baseenv() itself, like
    # .Last.value. We save the print methods here as well, and then they
    # will be found automatically.
    if (! "org:r-lib" %in% search()) {
      do.call("attach", list(new.env(), pos = length(search()),
                             name = "org:r-lib"))
    }
    env <- as.environment("org:r-lib")
    env$.Last.error <- cond
    env$.Last.error.trace <- cond$trace

    # If we always wanted a trace, then we signal the condition here
    if (always_trace) signalCondition(cond)

    # If this is not an error, then we'll just return here. This allows
    # throwing interrupt conditions for example, with the same UI.
    if (! inherits(cond, "error")) return(invisible())
    .hide_from_trace <- NULL

    # Top-level handler, this is intended for testing only for now,
    # and its design might change.
    if (!is.null(th <- getOption("rlib_error_handler")) &&
        is.function(th)) {
      return(th(cond))
    }

    # In non-interactive mode, we print the error + the traceback
    # manually, to make sure that it won't be truncated by R's error
    # message length limit.
    out <- format(
      cond,
      trace = !is_interactive(),
      class = FALSE,
      full = !is_interactive()
    )
    writeLines(out, con = default_output())

    # Dropping the classes and adding "duplicate_condition" is a workaround
    # for the case when we have non-exiting handlers on throw()-n
    # conditions. These would get the condition twice, because stop()
    # will also signal it. If we drop the classes, then only handlers
    # on "condition" objects (i.e. all conditions) get duplicate signals.
    # This is probably quite rare, but for this rare case they can also
    # recognize the duplicates from the "duplicate_condition" extra class.
    class(cond) <- c("duplicate_condition", "condition")

    # Turn off the regular error printing to avoid printing
    # the error twice.
    opts <- options(show.error.messages = FALSE)
    on.exit(options(opts), add = TRUE)

    stop(cond)
  }

  # -- rethrow with parent -----------------------------------------------

  #' Re-throw an error with a better error message
  #'
  #' Evaluate `expr` and if it errors, then throw a new error `err`,
  #' with the original error set as its parent.
  #'
  #' @noRd
  #' @param expr Expression to evaluate.
  #' @param err Error object or message to use for the child error.
  #' @param call Call to use in the re-thrown error. See [throw()].

  chain_error <- function(expr, err, call = sys.call(-1)) {
    .hide_from_trace <- 1
    force(call)
    srcref <- utils::getSrcref(sys.call())
    withCallingHandlers({
      expr
    }, error = function(e) {
      .hide_from_trace <- 0:1
      e$srcref <- srcref
      if (!inherits(err, "condition")) {
        err <- new_error(err, call. = call)
      }
      throw_error(err, parent = e)
    })
  }

  # -- rethrowing conditions from C code ---------------------------------

  #' Version of .Call that throw()s errors
  #'
  #' It re-throws error from compiled code. If the error had class
  #' `simpleError`, like all errors, thrown via `error()` in C do, it also
  #' adds the `c_error` class.
  #'
  #' @noRd
  #' @param .NAME Compiled function to call, see [.Call()].
  #' @param ... Function arguments, see [.Call()].
  #' @return Result of the call.

  chain_call <- function(.NAME, ...) {
    .hide_from_trace <- 1:3 # withCallingHandlers + do.call + .handleSimpleError (?)
    call <- sys.call()
    call1 <- sys.call(-1)
    srcref <- utils::getSrcref(call)
    withCallingHandlers(
      do.call(".Call", list(.NAME, ...)),
      error = function(e) {
        .hide_from_trace <- 0:1
        e$srcref <- srcref
        e$call <- call
        name <- native_name(.NAME)
        err <- new_error("Native call to `", name, "` failed", call. = call1)
        cerror <- if (inherits(e, "simpleError")) "c_error"
        class(err) <- c(cerror, "rlib_error_3_0", "rlib_error", "error", "condition")
        throw_error(err, parent = e)
      }
    )
  }

  package_env <- topenv()

  #' Version of entrace_call that supports cleancall
  #'
  #' This function is the same as [entrace_call()], except that it
  #' uses cleancall's [.Call()] wrapper, to enable resource cleanup.
  #' See https://github.com/r-lib/cleancall#readme for more about
  #' resource cleanup.
  #'
  #' @noRd
  #' @param .NAME Compiled function to call, see [.Call()].
  #' @param ... Function arguments, see [.Call()].
  #' @return Result of the call.

  chain_clean_call <- function(.NAME, ...) {
    .hide_from_trace <- 1:3
    call <- sys.call()
    call1 <- sys.call(-1)
    srcref <- utils::getSrcref(call)
    withCallingHandlers(
      package_env$call_with_cleanup(.NAME, ...),
      error = function(e) {
        .hide_from_trace <- 0:1
        e$srcref <- srcref
        e$call <- call
        name <- native_name(.NAME)
        err <- new_error("Native call to `", name, "` failed", call. = call1)
        cerror <- if (inherits(e, "simpleError")) "c_error"
        class(err) <- c(cerror, "rlib_error_3_0", "rlib_error", "error", "condition")
        throw_error(err, parent = e)
      }
    )
  }

  # -- create traceback -------------------------------------------------

  #' Create a traceback
  #'
  #' [throw()] calls this function automatically if an error is not caught,
  #' so there is currently not much use to call it directly.
  #'
  #' @param cond Condition to add the trace to
  #' @param frame Use this context to hide some frames from the traceback.
  #'
  #' @return A condition object, with the trace added.

  add_trace_back <- function(cond, frame = NULL) {

    idx <- seq_len(sys.parent(1L))
    frames <- sys.frames()[idx]

    # TODO: remove embedded objects from calls
    calls <- as.list(sys.calls()[idx])
    parents <- sys.parents()[idx]
    namespaces <- unlist(lapply(
      seq_along(frames),
      function(i) {
        env_label(topenvx(environment(sys.function(i))))
      }
    ))
    pids <- rep(cond$`_pid` %||% Sys.getpid(), length(calls))

    mch <- match(format(frame), sapply(frames, format))
    if (is.na(mch)) {
      visibles <- TRUE
    } else {
      visibles <- c(rep(TRUE, mch), rep(FALSE, length(frames) - mch))
    }

    scopes <- vapply(idx, FUN.VALUE = character(1), function(i) {
      tryCatch(
        get_call_scope(calls[[i]], namespaces[[i]]),
        error = function(e) ""
      )
    })

    namespaces <- ifelse(scopes %in% c("::", ":::"), namespaces, NA_character_)
    funs <- ifelse(
      is.na(namespaces),
      ifelse(scopes != "", paste0(scopes, " "), ""),
      paste0(namespaces, scopes)
    )
    funs <- paste0(
      funs,
      vapply(calls, function(x) format_name(x[[1]])[1], character(1))
    )
    visibles <- visibles & mark_invisible_frames(funs, frames)

    pcs <- lapply(calls, function(c) process_call(list(call = c)))
    calls <- lapply(pcs, "[[", "call")
    srcrefs <- I(lapply(pcs, "[[", "srcref"))

    cond$trace <- new_trace(
      calls,
      parents,
      visibles = visibles,
      namespaces = namespaces,
      scopes = scopes,
      srcrefs = srcrefs,
      pids
    )

    cond
  }

  mark_invisible_frames <- function(funs, frames) {
    visibles <- rep(TRUE, length(frames))
    hide <- lapply(frames, "[[", ".hide_from_trace")
    w_hide <- unlist(mapply(seq_along(hide), hide, FUN = function(i, w) {
      i + w
    }, SIMPLIFY = FALSE))
    w_hide <- w_hide[w_hide <= length(frames)]
    visibles[w_hide] <- FALSE

    hide_from <- which(funs %in% names(invisible_frames))
    for (start in hide_from) {
      hide_this <- invisible_frames[[ funs[start] ]]
      for (i in seq_along(hide_this)) {
        if (start + i > length(funs)) break
        if (funs[start + i] != hide_this[i]) break
        visibles[start + i] <- FALSE
      }
    }

    visibles
  }

  invisible_frames <- list(
    "base::source" = c("base::withVisible", "base::eval", "base::eval"),
    "base::stop" = "base::.handleSimpleError",
    "cli::cli_abort" = c(
      "rlang::abort",
      "rlang:::signal_abort",
      "base::signalCondition"),
    "rlang::abort" = c("rlang:::signal_abort", "base::signalCondition")
  )

  call_name <- function(x) {
    if (is.call(x)) {
      if (is.symbol(x[[1]])) {
        as.character(x[[1]])
      } else if (x[[1]][[1]] == quote(`::`) || x[[1]][[1]] == quote(`:::`)) {
        as.character(x[[1]][[2]])
      } else {
        NULL
      }
    } else {
      NULL
    }
  }

  get_call_scope <- function(call, ns) {
    if (is.na(ns)) return("global")
    if (!is.call(call)) return("")
    if (is.call(call[[1]]) &&
        (call[[1]][[1]] == quote(`::`) || call[[1]][[1]] == quote(`:::`))) return("")
    if (ns == "base") return("::")
    if (! ns %in% loadedNamespaces()) return("")
    name <- call_name(call)
    nsenv <- asNamespace(ns)$.__NAMESPACE__.
    if (is.null(nsenv)) return("::")
    if (is.null(nsenv$exports)) return(":::")
    if (exists(name, envir = nsenv$exports, inherits = FALSE)) {
      "::"
    } else if (exists(name, envir = asNamespace(ns), inherits = FALSE)) {
      ":::"
    } else {
      "local"
    }
  }

  topenvx <- function(x) {
    topenv(x, matchThisEnv = err_env)
  }

  new_trace <- function (calls, parents, visibles, namespaces, scopes, srcrefs, pids) {
    trace <- data.frame(
      stringsAsFactors = FALSE,
      parent = parents,
      visible = visibles,
      namespace = namespaces,
      scope = scopes,
      srcref = srcrefs,
      pid = pids
    )
    trace$call <- calls

    class(trace) <- c("rlib_trace_3_0", "rlib_trace", "tbl", "data.frame")
    trace
  }

  env_label <- function(env) {
    nm <- env_name(env)
    if (nzchar(nm)) {
      nm
    } else {
      env_address(env)
    }
  }

  env_address <- function(env) {
    class(env) <- "environment"
    sub("^.*(0x[0-9a-f]+)>$", "\\1", format(env), perl = TRUE)
  }

  env_name <- function(env) {
    if (identical(env, err_env)) {
      return(env_name(package_env))
    }
    if (identical(env, globalenv())) {
      return(NA_character_)
    }
    if (identical(env, baseenv())) {
      return("base")
    }
    if (identical(env, emptyenv())) {
      return("empty")
    }
    nm <- environmentName(env)
    if (isNamespace(env)) {
      return(nm)
    }
    nm
  }

  # -- S3 methods -------------------------------------------------------

  format_error <- function(x, trace = FALSE, class = FALSE,
                           advice = !trace, full = trace, header = TRUE,
                           ...) {
    if (has_cli()) {
      format_error_cli(x, trace, class, advice, full, header, ...)
    } else {
      format_error_plain(x, trace, class, advice, full, header, ...)
    }
  }

  print_error <- function(x, trace = TRUE, class = TRUE,
                          advice = !trace,  ...) {
    writeLines(format_error(x, trace, class, advice, ...))
  }

  format_trace <- function(x, ...) {
    if (has_cli()) {
      format_trace_cli(x, ...)
    } else {
      format_trace_plain(x, ...)
    }
  }

  print_trace <- function(x, ...) {
    writeLines(format_trace(x, ...))
  }

  cnd_message <- function(cond) {
    paste(cnd_message_(cond, full = FALSE), collapse = "\n")
  }

  cnd_message_ <- function(cond, full = FALSE) {
    if (has_cli()) {
      cnd_message_cli(cond, full)
    } else {
      cnd_message_plain(cond, full)
    }
  }

  # -- format API -------------------------------------------------------

  format_advice <- function(x) {
    if (has_cli()) {
      format_advice_cli(x)
    } else {
      format_advice_plain(x)
    }
  }

  format_call <- function(call) {
    if (has_cli()) {
      format_call_cli(call)
    } else {
      format_call_plain(call)
    }
  }

  format_class <- function(x) {
    if (has_cli()) {
      format_class_cli(x)
    } else {
      format_class_plain(x)
    }
  }

  format_error_heading <- function(x, prefix = NULL) {
    if (has_cli()) {
      format_error_heading_cli(x, prefix)
    } else {
      format_error_heading_plain(x, prefix)
    }
  }

  format_header_line <- function(x, prefix = NULL) {
    if (has_cli()) {
      format_header_line_cli(x, prefix)
    } else {
      format_header_line_plain(x, prefix)
    }
  }

  format_srcref <- function(call, srcref = NULL) {
    if (has_cli()) {
      format_srcref_cli(call, srcref)
    } else {
      format_srcref_plain(call, srcref)
    }
  }

  # -- condition message with cli ---------------------------------------

  cnd_message_cli <- function(cond, full = FALSE) {
    exp <- paste0(cli::col_yellow("!"), " ")
    add_exp <- is.null(names(cond$message))

    c(
      paste0(if (add_exp) exp, cond$message),
      if (inherits(cond$parent, "condition")) {
        msg <- if (full && inherits(cond$parent, "rlib_error_3_0")) {
          format(cond$parent,
                 trace = FALSE,
                 full = TRUE,
                 class = FALSE,
                 header = FALSE,
                 advice = FALSE
          )
        } else {
          conditionMessage(cond$parent)
        }
        add_exp <- substr(cli::ansi_strip(msg[1]), 1, 1) != "!"
        if (add_exp) msg[1] <- paste0(exp, msg[1])
        c(format_header_line_cli(cond$parent, prefix = "Caused by error"),
          msg
        )
      }
    )
  }

  # -- condition message w/o cli ----------------------------------------

  cnd_message_plain <- function(cond, full = FALSE) {
    exp <- "! "
    add_exp <- is.null(names(cond$message))
    c(
      paste0(if (add_exp) exp, cond$message),
      if (inherits(cond$parent, "condition")) {
        msg <- if (full && inherits(cond$parent, "rlib_error_3_0")) {
          format(cond$parent,
                 trace = FALSE,
                 full = TRUE,
                 class = FALSE,
                 header = FALSE,
                 advice = FALSE
          )
        } else {
          conditionMessage(cond$parent)
        }
        add_exp <- substr(msg[1], 1, 1) != "!"
        if (add_exp) {
          msg[1] <- paste0(exp, msg[1])
        }
        c(format_header_line_plain(cond$parent, prefix = "Caused by error"),
          msg
        )
      }
    )
  }

  # -- printing error with cli ------------------------------------------

  # Error parts:
  # - "Error:" or "Error in " prefix, the latter if the error has a call
  # - the call, possibly syntax highlightedm possibly trimmed (?)
  # - source ref, with link to the file, potentially in a new line in cli
  # - error message, just `conditionMessage()`
  # - advice about .Last.error and/or .Last.error.trace

  format_error_cli <- function(x, trace = TRUE, class = TRUE,
                               advice = !trace, full = trace,
                               header = TRUE, ...) {
    p_class <- if (class) format_class_cli(x)
    p_header <- if (header) format_header_line_cli(x)
    p_msg <- cnd_message_cli(x, full)
    p_advice <- if (advice) format_advice_cli(x) else NULL
    p_trace <- if (trace && !is.null(x$trace)) {
      c("---", "Backtrace:", format_trace_cli(x$trace))
    }

    c(p_class,
      p_header,
      p_msg,
      p_advice,
      p_trace)
  }

  format_header_line_cli <- function(x, prefix = NULL) {
    p_error <- format_error_heading_cli(x, prefix)
    p_call <- format_call_cli(x$call)
    p_srcref <- format_srcref_cli(conditionCall(x), x$srcref)
    paste0(p_error, p_call, p_srcref, if (!is.null(conditionCall(x))) ":")
  }

  format_class_cli <- function(x) {
    cls <- unique(setdiff(class(x), "condition"))
    cls # silence codetools
    cli::format_inline("{.cls {cls}}")
  }

  format_error_heading_cli <- function(x, prefix = NULL) {
    str_error <- if (is.null(prefix)) {
      cli::style_bold(cli::col_yellow("Error"))
    } else {
      cli::style_bold(paste0(prefix))
    }
    if (is.null(conditionCall(x))) {
      paste0(str_error, ": ")
    } else {
      paste0(str_error, " in ")
    }
  }

  format_call_cli <- function(call) {
    if (is.null(call)) {
      NULL
    } else {
      cl <- trimws(format(call))
      if (length(cl) > 1) cl <- paste0(cl[1], " ", cli::symbol$ellipsis)
      cli::format_inline("{.code {cl}}")
    }
  }

  format_srcref_cli <- function(call, srcref = NULL) {
    ref <- get_srcref(call, srcref)
    if (is.null(ref)) return("")

    link <- if (ref$file != "") {
      cli::style_hyperlink(
        cli::format_inline("{basename(ref$file)}:{ref$line}:{ref$col}"),
        paste0("file://", ref$file),
        params = c(line = ref$line, col = ref$col)
      )

    } else {
      paste0("line ", ref$line)
    }

    cli::col_silver(paste0(" at ", link))
  }

  str_advice <- "Type .Last.error to see the more details."

  format_advice_cli <- function(x) {
    cli::col_silver(str_advice)
  }

  format_trace_cli <- function(x, ...) {
    x$num <- seq_len(nrow(x))

    scope <- ifelse(
      is.na(x$namespace),
      ifelse(x$scope != "", paste0(x$scope, " "), ""),
      paste0(x$namespace, x$scope)
    )

    visible <- if ("visible" %in% names(x)) {
      x$visible
    } else {
      rep(TRUE, nrow(x))
    }

    srcref <- if ("srcref" %in% names(x)) {
      vapply(
        seq_len(nrow(x)),
        function(i) format_srcref_cli(x$call[[i]], x$srcref[[i]]),
        character(1)
      )
    } else {
      unname(vapply(x$call, format_srcref_cli, character(1)))
    }

    lines <- paste0(
      cli::col_silver(format(x$num), ". "),
      ifelse (visible, "", "| "),
      scope,
      vapply(x$call, format_trace_call_cli, character(1)),
      srcref
    )

    lines[!visible] <- cli::col_silver(cli::ansi_strip(
      lines[!visible],
      link = FALSE
    ))

    lines
  }

  format_trace_call_cli <- function(call) {
    cl <- trimws(format(call))
    if (length(cl) > 1) { cl <- paste0(cl[1], " ", cli::symbol$ellipsis) }
    fmc <- cli::code_highlight(cl)[1]
    cli::ansi_strtrim(fmc, cli::console_width() - 5)
  }

  # ----------------------------------------------------------------------

  format_error_plain <- function(x, trace = TRUE, class = TRUE,
                                 advice = !trace, full = trace, header = TRUE,
                                 ...) {
    p_class <- if (class) format_class_plain(x)
    p_header <- if (header) format_header_line_plain(x)
    p_msg <- cnd_message_plain(x, full)
    p_advice <- if (advice) format_advice_plain(x) else NULL
    p_trace <- if (trace && !is.null(x$trace)) {
      c("---", "Backtrace:", format_trace_plain(x$trace))
    }

    c(p_class,
      p_header,
      p_msg,
      p_advice,
      p_trace)
  }

  format_trace_plain <- function(x, ...) {
    x$num <- seq_len(nrow(x))

    scope <- ifelse(
      is.na(x$namespace),
      ifelse(x$scope != "", paste0(x$scope, " "), ""),
      paste0(x$namespace, x$scope)
    )

    visible <- if ("visible" %in% names(x)) {
      x$visible
    } else {
      rep(TRUE, nrow(x))
    }

    srcref <- if ("srcref" %in% names(x)) {
      vapply(
        seq_len(nrow(x)),
        function(i) format_srcref_plain(x$call[[i]], x$srcref[[i]]),
        character(1)
      )
    } else {
      unname(vapply(x$call, format_srcref_plain, character(1)))
    }

    lines <- paste0(
      paste0(format(x$num), ". "),
      ifelse (visible, "", "| "),
      scope,
      vapply(x$call, format_trace_call_plain, character(1)),
      srcref
    )

    lines
  }

  format_advice_plain <- function(x, ...) {
    str_advice
  }

  format_header_line_plain <- function(x, prefix = NULL) {
    p_error <- format_error_heading_plain(x, prefix)
    p_call <- format_call_plain(x$call)
    p_srcref <- format_srcref_plain(conditionCall(x), x$srcref)
    paste0(p_error, p_call, p_srcref, if (!is.null(conditionCall(x))) ":")
  }

  format_error_heading_plain <- function(x, prefix = NULL) {
    str_error <- if (is.null(prefix)) "Error" else prefix
    if (is.null(conditionCall(x))) {
      paste0(str_error, ": ")
    } else {
      paste0(str_error, " in ")
    }
  }

  format_class_plain <- function(x) {
    cls <- unique(setdiff(class(x), "condition"))
    paste0("<", paste(cls, collapse = "/"), ">")
  }

  format_call_plain <- function(call) {
    if (is.null(call)) {
      NULL
    } else {
      cl <- trimws(format(call))
      if (length(cl) > 1) cl <- paste0(cl[1], " ...")
      paste0("`", cl, "`")
    }
  }

  format_srcref_plain <- function(call, srcref = NULL) {
    ref <- get_srcref(call, srcref)
    if (is.null(ref)) return("")

    link <- if (ref$file != "") {
      paste0(basename(ref$file), ":", ref$line, ":", ref$col)
    } else {
      paste0("line ", ref$line)
    }

    paste0(" at ", link)
  }

  format_trace_call_plain <- function(call) {
    fmc <- trimws(format(call)[1])
    if (length(fmc) > 1) { fmc <- paste0(fmc[1], " ...") }
    strtrim(fmc, getOption("width") - 5)
  }

  # -- utilities ---------------------------------------------------------

  cli_version <- function() {
    # this loads cli!
    package_version(asNamespace("cli")[[".__NAMESPACE__."]]$spec[["version"]])
  }

  has_cli <- function() {
    "cli" %in% loadedNamespaces() && cli_version() >= "3.3.0"
  }

  `%||%` <- function(l, r) if (is.null(l)) r else l

  bytes <- function(x) {
    nchar(x, type = "bytes")
  }

  process_call <- function(cond) {
    cond[c("call", "srcref")] <- list(
      call = if (is.null(cond$call)) {
        NULL
      } else if (is.character(cond$call)) {
        cond$call
      } else {
        deparse(cond$call, nlines = 2)
      },
      srcref = get_srcref(cond$call, cond$srcref)
    )
    cond
  }

  get_srcref <- function(call, srcref = NULL) {
    ref <- srcref %||% utils::getSrcref(call)
    if (is.null(ref)) return(NULL)
    if (inherits(ref, "processed_srcref")) return(ref)
    file <- utils::getSrcFilename(ref, full.names = TRUE)[1]
    if (is.na(file)) file <- ""
    line <- utils::getSrcLocation(ref) %||% ""
    col <- utils::getSrcLocation(ref, which = "column") %||% ""
    structure(
      list(file = file, line = line, col = col),
      class = "processed_srcref"
    )
  }

  is_interactive <- function() {
    opt <- getOption("rlib_interactive")
    if (isTRUE(opt)) {
      TRUE
    } else if (identical(opt, FALSE)) {
      FALSE
    } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
      FALSE
    } else if (tolower(getOption("rstudio.notebook.executing", "false")) == "true") {
      FALSE
    } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
      FALSE
    } else {
      interactive()
    }
  }

  no_sink <- function() {
    sink.number() == 0 && sink.number("message") == 2
  }

  rstudio_stdout <- function() {
    rstudio <- rstudio_detect()
    rstudio$type %in% c(
      "rstudio_console",
      "rstudio_console_starting",
      "rstudio_build_pane",
      "rstudio_job",
      "rstudio_render_pane"
    )
  }

  default_output <- function() {
    if ((is_interactive() || rstudio_stdout()) && no_sink()) {
      stdout()
    } else {
      stderr()
    }
  }

  onload_hook <- function() {
    reg_env <- Sys.getenv("R_LIB_ERROR_REGISTER_PRINT_METHODS", "TRUE")
    if (tolower(reg_env) != "false") {
      registerS3method("format", "rlib_error_3_0", format_error, baseenv())
      registerS3method("format", "rlib_trace_3_0", format_trace, baseenv())
      registerS3method("print", "rlib_error_3_0", print_error, baseenv())
      registerS3method("print", "rlib_trace_3_0", print_trace, baseenv())
      registerS3method("conditionMessage", "rlib_error_3_0", cnd_message, baseenv())
    }
  }

  native_name <- function(x) {
    if (inherits(x, "NativeSymbolInfo")) {
      x$name
    } else {
      format(x)
    }
  }

  # There is no format() for 'name' in R 3.6.x and before
  format_name <- function(x) {
    if (is.name(x)) {
      as.character(x)
    } else {
      format(x)
    }
  }

  # -- public API --------------------------------------------------------

  err_env <- environment()
  parent.env(err_env) <- baseenv()

  structure(
    list(
      .internal        = err_env,
      new_cond         = new_cond,
      new_error        = new_error,
      throw            = throw,
      throw_error      = throw_error,
      chain_error      = chain_error,
      chain_call       = chain_call,
      chain_clean_call = chain_clean_call,
      add_trace_back   = add_trace_back,
      process_call     = process_call,
      onload_hook      = onload_hook,
      format = list(
        advice        = format_advice,
        call          = format_call,
        class         = format_class,
        error         = format_error,
        error_heading = format_error_heading,
        header_line   = format_header_line,
        srcref        = format_srcref,
        trace         = format_trace
      )
    ),
    class = c("standalone_errors", "standalone"))
})

# These are optional, and feel free to remove them if you prefer to
# call them through the `err` object.

new_cond         <- err$new_cond
new_error        <- err$new_error
throw            <- err$throw
throw_error      <- err$throw_error
chain_error      <- err$chain_error
chain_call       <- err$chain_call
chain_clean_call <- err$chain_clean_call
