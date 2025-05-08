#' Create an async function
#'
#' Create an async function, that returns a deferred value, from a
#' regular function. If `fun` is already an async function, then it does
#' nothing, just returns it.
#'
#' The result function will have the same arguments, with the same default
#' values, and the same environment as the original input function.
#'
#' @param fun Original function.
#' @return Async version of the original function.
#'
#' @noRd
#' @examples
#' f <- function(x) 42
#' af <- async(f)
#' is_async(f)
#' is_async(af)
#' f()
#' synchronise(dx <- af())
#' dx

async <- function(fun) {
  fun <- as.function(fun)
  if (is_async(fun)) return(fun)

  async_fun <- fun
  body(async_fun) <- bquote({
    mget(ls(environment(), all.names = TRUE), environment())
    fun2 <- function() {
      evalq(
        .(body(fun)),
        envir = parent.env(environment())
      )
    }

    deferred$new(
      type = "async",
      action = function(resolve) resolve(fun2())
    )
  })

  # This is needed, otherwise async_fun might not find 'deferred'
  async_env <- new.env(parent = environment(async_fun))
  async_env$deferred <- deferred
  environment(async_fun) <- async_env

  mark_as_async(async_fun)
}

mark_as_async <- function(fun) {
  attr(body(fun), "async")$async <- TRUE

  ## These are not valid any more, anyway
  attr(fun, "srcref") <- NULL
  attr(body(fun), "srcref") <- NULL

  fun
}

#' Checks if a function is async
#'
#' If `fun` is not a function, an error is thrown.
#'
#' Currently, it checks for the `async` attribute, which is set by
#' [async()].
#'
#' @param fun Function.
#' @return Logical scalar, whether `fun` is async.
#'
#' @noRd
#' @examples
#' f <- function(x) 42
#' af <- async(f)
#' is_async(f)
#' is_async(af)
#' f()
#' synchronise(dx <- af())
#' dx

is_async <- function(fun) {
  assert_that(is.function(fun))
  is.list(a <- attr(body(fun), "async")) && identical(a$async, TRUE)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (length 1 logical)")
}

is_action_function <- function(x) {
  is.function(x) && length(formals(x)) %in% 1:2
}

on_failure(is_action_function) <- function(call, env) {
  paste0(deparse(call$x), " is not a function with two arguments")
}

is_time_interval <- function(x) {
  inherits(x, "difftime") ||
    (is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0)
}

on_failure(is_time_interval) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid time interval")
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x
}

on_failure(is_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (non-negative integer)")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " must be a flag (length 1 logical)")
}

#' Retry an asynchronous function with exponential backoff
#'
#' Keeps trying until the function's deferred value resolves without
#' error, or `times` tries have been performed, or `time_limit` seconds
#' have passed since the start of the first try.
#'
#' Note that all unnamed arguments are passed to `task`.
#'
#' @param task An asynchronous function.
#' @param ... Arguments to pass to `task`.
#' @param .args More arguments to pass to `task`.
#' @param times Maximum number of tries.
#' @param time_limit Maximum number of seconds to try.
#' @param custom_backoff If not `NULL` then a callback function to
#'   calculate waiting time, after the `i`the try. `i` is passed as an
#'   argument. If `NULL`, then the default is used, which is a uniform
#'   random number of seconds between 1 and 2^i.
#' @param on_progress Callback function for a progress bar. Retries are
#'   announced here, if not `NULL`. `on_progress` is called with two
#'   arguments. The first is a named list with entries:
#'   * `event`: string that is either `"retry"` or `"givenup"`,
#'   * `tries`: number of tried so far,
#'   * `spent`: number of seconds spent trying so far,
#'   * `error`: the error object for the last failure,
#'   * `retry_in`: number of seconds before the next try.
#'   The second argument is `progress_data`.
#' @param progress_data `async_backoff()` will pass this object to
#'   `on_progress` as the second argument.
#' @return Deferred value for the operation with retries.
#'
#' @family async control flow
#' @noRd
#' @examples
#' \donttest{
#' afun <- function() {
#'   wait_100_ms <- function(i) 0.1
#'   async_backoff(
#'     function() if (runif(1) < 0.8) stop("nope") else "yes!",
#'     times = 5,
#'     custom_backoff = wait_100_ms
#'   )
#' }
#'
#' # There is a slight chance that it fails
#' tryCatch(synchronise(afun()), error = function(e) e)
#' }

async_backoff <- function(
  task,
  ...,
  .args = list(),
  times = Inf,
  time_limit = Inf,
  custom_backoff = NULL,
  on_progress = NULL,
  progress_data = NULL
) {
  task <- async(task)
  args <- c(list(...), .args)
  times <- times
  time_limit <- time_limit
  custom_backoff <- custom_backoff %||% default_backoff
  on_progress <- on_progress
  progress_data <- progress_data

  did <- 0L
  started <- NULL
  limit <- NULL

  self <- deferred$new(
    type = "backoff",
    call = sys.call(),
    action = function(resolve) {
      started <<- Sys.time()
      limit <<- started + time_limit
      do.call(task, args)$then(self)
    },
    parent_reject = function(value, resolve) {
      did <<- did + 1L
      now <- Sys.time()
      if (did < times && now < limit) {
        wait <- custom_backoff(did)
        if (!is.null(on_progress)) {
          on_progress(
            list(
              event = "retry",
              tries = did,
              spent = now - started,
              error = value,
              retry_in = wait
            ),
            progress_data
          )
        }
        delay(wait)$then(function() do.call(task, args))$then(self)
      } else {
        if (!is.null(on_progress)) {
          on_progress(
            list(
              event = "givenup",
              tries = did,
              spent = now - started,
              error = value,
              retry_in = NA_real_
            ),
            progress_data
          )
        }
        stop(value)
      }
    }
  )
}

async_backoff <- mark_as_async(async_backoff)

default_backoff <- function(i) {
  as.integer(stats::runif(1, min = 1, max = 2^i) * 1000) / 1000
}

#' Asynchronous function call, in a worker pool
#'
#' The function will be called on another process, very much like
#' [callr::r()].
#'
#' @param func Function to call. See also the notes at [callr::r()].
#' @param args Arguments to pass to the function. They will be copied
#'   to the worker process.
#' @return Deferred object.
#'
#' @noRd

call_function <- function(func, args = list()) {
  func
  args

  id <- NULL

  deferred$new(
    type = "pool-task",
    call = sys.call(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      id <<- get_default_event_loop()$add_pool_task(
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(func = func, args = args)
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) {
        get_default_event_loop()$cancel(id)
      }
    }
  )
}

call_function <- mark_as_async(call_function)

#' Make a minimal deferred that resolves to the specified value
#'
#' This is sometimes useful to start a deferred chain.
#'
#' Note that the evaluation of `value` is forced when the deferred value
#' is created.
#'
#' @param value The value to resolve to.
#' @return A deferred value.
#'
#' @noRd
#' @examples
#' afun <- async(function() {
#'   async_constant(1/100)$
#'     then(function(x) delay(x))$
#'     then(function(x) print(x))
#' })
#' synchronise(afun())

async_constant <- function(value = NULL) {
  force(value)
  deferred$new(
    type = "constant",
    call = sys.call(),
    function(resolve) resolve(value)
  )
}

async_constant <- mark_as_async(async_constant)

async_env <- new.env(parent = emptyenv())
async_env$loops <- list()

get_default_event_loop <- function() {
  num_loops <- length(async_env$loops)
  if (num_loops == 0) {
    err <- make_error(
      "You can only call async functions from an async context",
      class = "async_synchronization_barrier_error"
    )
    stop(err)
  }

  async_env$loops[[num_loops]]
}

push_event_loop <- function() {
  num_loops <- length(async_env$loops)
  if (num_loops > 0) async_env$loops[[num_loops]]$suspend()
  new_el <- event_loop$new()
  async_env$loops <- c(async_env$loops, list(new_el))
  new_el
}

pop_event_loop <- function() {
  num_loops <- length(async_env$loops)
  async_env$loops[[num_loops]] <- NULL
  if (num_loops > 1) async_env$loops[[num_loops - 1]]$wakeup()
}

#' Async debugging utilities
#'
#' Helper function to help with the non-trivial debugging of async code.
#'
#' Async debugging can be turned on by setting the `async_debug` global
#' option to `TRUE`:
#' ```
#' options(async_debug = TRUE)
#' ```
#' Setting this value to `FALSE` will turn off debugging.
#'
#' If debugging is on, a [synchronise()] call will stop at the beginning
#' of the event loop. No deferred actions of other callbacks have run at
#' this point. [synchronise()] stops by calling [base::browser()]. All the
#' usual [browser()] commands (see its manual) can be used here, plus some
#' extra commands to help async debugging. The extra commands:
#'
#' `async_debug_shortcuts()` adds handy shortcuts to most of the helper
#' functions. E.g. `async_next()` can be invoked as `.an` (without the
#' parens). You only need to run it once per R session. Note that it adds
#' the shortcuts to the global environment.
#'
#' `async_debug_remove_shortcuts()` removes the shortcuts from the global
#' environment.
#'
#' `.an` (or `async_next()`) runs the next iteration of the event loop.
#' Note that it does not return until _something_ happens in the event loop:
#' an action or a parent callback is executed, or HTTP or other I/O is
#' performed. Also note, that a single iteration of the event loop typically
#' runs multiple action, parent or other callbacks. Once the iteration is
#' done, the control is returned to the browser.
#'
#' `.as` (or `async_step()`) is similar to `.an`, but it also starts the
#' debugging of the action or parent callbacks. I.e. another [browser()] is
#' called at the beginning of _all_ callbacks in the next iteration of the
#' event loop.
#'
#' `.asb` (or `async_step_back()`) stops the debugging of the callbacks.
#' It does not actually exdecutes anything from the event loop, so to go
#' back to the main async browser, you also need to execute `c` (continue).
#'
#' `.al` (or `async_list()`) lists all deferred values in the current async
#' phase. (Only the ones that already exist, some may be created in the
#' future.) It returns a data frame with columns:
#'
#' * `id`: The integer id of the deferred value.
#' * `parents`: Integer vector, the parents of the deferred value.
#' * `label`: A character label, that is used by `async_tree()` to nicely
#'    format information about a deferred value.
#' * `call`: The call (language object) that created the deferred value.
#' * `children`: The list of children, an integer vector. A deferred value
#'    can only have one child, unless it is shared.
#' * `type`: The type of the deferred value. This is an arbitrary label,
#'    specified when the deferred value was created.
#' * `running`: Whether the deferred value is already running.
#' * `state`: The state of the deferred value, `"pending"`, `"fulfilled"` or
#'    `"rejected"`. This is typically pending, since resolved deferred
#'    values are removed from the async DAG (in the next event loop
#'    iteration.)
#' * `cancelled`: Whether the deferred value was cancelled.
#' * `shared`: Whether the deferred value is shared.
#' * `filename`: The file name for the source code that created the
#'    deferred value. Only present if this code was parsed with source
#'    references enabled.
#' * `position`: The start file position, in line:column format, as a
#'    string. Only present if this code was parsed with source references
#'    enabled.
#'
#' `.at` (or `async_tree()`) prints the DAG of the deferred values.
#'
#' `async_debug()` can be used to debug the action and/or parent callbacks
#' of the specified deferred value.
#'
#' `async_wait_for()` runs the event loop until the specified deferred
#' value is resolved (i.e. fulfilled or rejected).
#'
#' `.aw` (or `async_where()`) prints a call stack and marks the frame the
#' corresponds to an action or parent callback.
#'
#' @param el Event loop, defaults to the current event loop.
#' @param def Deferred value that is used at the root of the DAG. Defaults
#'   to the deferred value corresponding to the result of the async phase.
#' @param id Integer scalar, the if of the deferred to debug or to wait for.
#' @param action Whether to debug the action callback.
#' @param parent Whether to debug the parent callbacks.
#' @param calls The calls to print, result of `sys.calls()`. Defaults to
#'   the current call stack.
#' @param parents The parent frames in the call stack, result of
#'   `sys.parents()`. Defaults to the current parents.
#' @param frm The async frame to mark. Defaults to the most recent async
#'   frame in the stack.
#'
#' @name async_debug
#' @noRd
NULL

#' @noRd
#' @aliases .an
#' @rdname async_debug

async_next <- function(el = NULL) {
  el <- el %||% find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  ## TODO: some visual indication that something has happened?
  if (!el$run("once")) message("[ASYNC] async phase complete")
}

# nocov start

#' @noRd
#' @aliases .as
#' @rdname async_debug

async_step <- function() {
  el <- find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  ## TODO: some visual indication that something has happened?
  old <- options(async_debug_steps = TRUE)
  on.exit(options(old))
  if (!el$run("once")) {
    message("[ASYNC] async phase complete")
  }
}

#' @noRd
#' @aliases .asb
#' @rdname async_debug

async_step_back <- function() {
  options(async_debug_steps = FALSE)
  message("[ASYNC] step back, you still need to 'c'ontinue")
}

# nocov end

#' @noRd
#' @aliases .al
#' @rdname async_debug

async_list <- function(def = NULL) {
  def <- def %||% find_sync_frame()$res
  if (is.null(def)) stop("No async context")
  info <- list()
  find_parents <- function(def) {
    info <<- c(info, list(get_private(def)$get_info()))
    prn <- get_private(def)$parents
    lapply(prn, find_parents)
  }
  find_parents(def)

  do.call(rbind, info)
}

#' @noRd
#' @aliases .at
#' @rdname async_debug

async_tree <- function(def = NULL) {
  def <- def %||% find_sync_frame()$res
  data <- async_list(def)
  root <- as.character(get_private(def)$id)
  cli::tree(data, root = root)
}

#' @noRd
#' @rdname async_debug

async_debug <- function(id, action = TRUE, parent = TRUE) {
  def <- find_deferred(id)
  if (is.null(def)) stop("Cannot find deferred `", id, "`")
  prv <- get_private(def)

  if (prv$state != "pending") {
    message("[ASYNC] ", id, " already resolved")
    return(invisible())
  }

  what <- character()
  if (action) {
    if (prv$running) {
      message("[ASYNC] ", id, " action already running")
    } else if (is.null(prv$action)) {
      message("[ASYNC] ", id, " has no action")
    } else {
      ## TODO: make a copy? Or should the deferred make a copy?
      debug1(prv$action)
      what <- "action"
    }
  }

  if (parent) {
    ## TODO: make copies?
    debug_all(prv$parent_resolve)
    debug_all(prv$parent_reject)
    what <- c(what, "parent callbacks")
  }

  if (length(what) == 1) {
    message("[ASYNC] ", id, " debugging ", what)
  }
  if (length(what) == 2) {
    message("[ASYNC] ", id, " debugging ", what[1], " and ", what[2])
  }

  invisible(def)
}

#' @noRd
#' @rdname async_debug

async_wait_for <- function(id) {
  el <- find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  def <- find_deferred(id)
  if (is.null(def)) stop("Cannot find deferred `", id, "`")
  priv <- get_private(def)
  while (priv$state == "pending") el$run("once")
  message("[ASYNC] ", id, "  resolved")
}

#' @noRd
#' @aliases .aw
#' @rdname async_debug

async_where <- function(
  calls = sys.calls(),
  parents = sys.parents(),
  frm = get_async_frames()
) {
  afrm <- viapply(frm, "[[", "frame")
  num <- seq_along(calls)

  src <- lapply(calls, get_source_position)

  res <- data.frame(
    stringsAsFactors = FALSE,
    call = I(calls),
    parent = parents,
    filename = vcapply(src, "[[", "filename"),
    position = vcapply(src, "[[", "position"),
    async = num %in% afrm
  )

  res$def_id <- NA_integer_
  res$def_id[afrm] <- viapply(frm, function(x) x$deferred)
  res$def_cb_type <- NA_character_
  res$def_cb_type[afrm] <- vcapply(frm, function(x) x$type)
  res$def_call <- I(list(NULL))
  res$def_call[afrm] <- lapply(frm, "[[", "call")

  def_src <- lapply(res$def_call[afrm], get_source_position)
  res$def_filename <- NA_character_
  res$def_filename[afrm] <- vcapply(def_src, "[[", "filename")
  res$def_position <- NA_character_
  res$def_position[afrm] <- vcapply(def_src, "[[", "position")

  class(res) <- c("async_where", class(res))
  res
}

# nocov start

#' @noRd

print.async_where <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

# nocov end

#' @noRd

format.async_where <- function(x, ...) {
  paste0(
    paste(
      formatC(seq_len(nrow(x)), width = 3),
      vcapply(x$call, expr_name),
      paste0(" ", x$filename, ":", x$position),
      ifelse(
        !x$async,
        "",
        paste0(
          "\n    ",
          x$def_id,
          " ",
          x$def_cb_type,
          " ",
          x$def_call,
          " ",
          x$def_filename,
          ":",
          x$def_position
        )
      ),
      collapse = "\n"
    ),
    "\n"
  )
}

get_async_frames <- function() {
  drop_nulls(lapply(seq_along(sys.frames()), function(i) {
    if (!is.null(data <- sys.frame(i)$`__async_data__`)) {
      list(
        frame = i + data$skip %||% 1L,
        deferred = data[[1]],
        type = data[[2]],
        call = get_private(data[[3]])$mycall
      )
    }
  }))
}

find_sync_frame <- function() {
  for (i in seq_along(sys.frames())) {
    cand <- sys.frame(-i)
    if (isTRUE(cand$`__async_synchronise_frame__`)) return(cand)
  }
}

find_async_data_frame <- function() {
  frames <- sys.frames()
  for (i in seq_along(frames)) {
    cand <- sys.frame(-i)
    if (!is.null(data <- cand$`__async_data__`)) {
      return(list(frame = length(frames) - i + 1L, data = data))
    }
  }
}

find_deferred <- function(id, def = NULL) {
  def <- def %||% find_sync_frame()$res
  if (is.null(def)) stop("No async context")
  search_parents <- function(def) {
    if (get_private(def)$id == id) return(def)
    prn <- get_private(def)$parents
    for (p in lapply(prn, search_parents)) {
      if (!is.null(p)) return(p)
    }
  }
  search_parents(def)
}

# nocov start

debug1 <- function(fun) {
  debugonce(fun)
}

#' @noRd
#' @rdname async_debug

async_debug_shortcuts <- function() {
  as <- function(name, fun) {
    makeActiveBinding(name, fun, .GlobalEnv)
  }
  as(".an", async_next)
  as(".as", async_step)
  as(".asb", async_step_back)
  as(".al", async_list)
  as(".at", async_tree)
  as(".aw", async_where)
}

#' @noRd
#' @rdname async_debug

async_debug_remove_shortcuts <- function() {
  tryCatch(
    rm(list = c(".an", ".as", ".asb", ".al", ".at", ".aw"), envir = .GlobalEnv),
    error = function(x) x
  )
}

# nocov end

debug_all <- function(fun) {
  debug(fun)
}

#' Deferred value
#'
#' @section Usage:
#' ```
#' dx <- deferred$new(action = NULL, on_progress = NULL, on_cancel = NULL,
#'          parents = NULL, parent_resolve = NULL, parent_reject = NULL,
#'          type = NULL)
#' dx$then(on_fulfilled)
#' dx$catch(...)
#' dx$finally(on_finally)
#' dx$cancel(reason = "Cancelled")
#' dx$share()
#' ```
#'
#' @section Arguments:
#' * `action`: Function to call when the deferred value starts running.
#'      it needs to have at least two arguments: `resolve` and `reject`,
#'      and the third `progress` argument is optional. See details below.
#' * `on_progress`: A function to call to report progress. See details
#'      below.
#' * `on_cancel`: A function to call when the deferred is cancelled. See
#'      details below.
#' * `parents`: A list of deferred values that will be the parents of the
#'      deferred value being created. If some of them are already owned,
#'      an error is thrown.
#' * `parent_resolve`: A function to call when a parent is resolved.
#'      See details below.
#' * `parent_reject`: A function to call when a parent throws an error.
#'      See details below.
#' * `type`: A label that can be used to indicate the type of the deferred
#'      value to create. This might be useful for debugging, but otherwise
#'      it is not used.
#' * `on_fulfilled`: Function to call when the parent deferred is resolved.
#'      Essentially this is the `parent_resolve` function of the `then()`
#'      deferred.
#' * `...` Error handlers, as in `tryCatch()`, see details below.
#' * `on_finally`: Function to call, after the deferred value is resolved
#'      or after it has thrown an error. It will be called without arguments.
#' * `reason` Error message or error object that will be used to cancel the
#'      deferred.
#'
#' @section Deferred values:
#'
#' Asynchronous computation is represented by deferred values.
#' A deferred value is an [R6](https://github.com/wch/R6) object.
#'
#' ```
#' deferred$new(action = NULL, on_progress = NULL, on_cancel = NULL,
#'    parents = NULL, parent_resolve = NULL, parent_reject = NULL,
#'    type = NULL)
#' ```
#'
#' Creates a new deferred value. `action` is a function that is called
#' once the deferred value is _started_ (i.e. _not_ when `dx` is created).
#' It must have one or two arguments: `resolve`, or `resolve` and `progress`
#' It should call `resolve` when it is done, with the final value of the
#' deferred as the argument. (See examples below.) If it has two arguments,
#' then the second one is a callback function for creating progress bars.
#' The deferred value may report its progress through this function.
#' See details in the _Progress bars_ section below.
#'
#' `action` is called when the evaluation of the deferred value is started.
#' Only deferred values that are needed to calculate the value of the
#' async phase, are evaluated. (See also _Lazy Evaluation_ below.)
#'
#' Note that `action` is optional, for some deferred values, no action is
#' takes when they are started. (These typically depend on their parent
#' nodes.)
#'
#' `on_cancel` is a function that is called without arguments when a
#' deferred value is cancelled. This includes explicit cancellation by
#' calling its `$cancel()` method, or auto-cancellation (see below).
#'
#' `parents` is a list of deferred values that need to be computed before
#' the current deferred value. When a parent deferred is resolved, the
#' `parent_resolve` function is called. When a parent referred throws an
#' error, the parent_reject` function is called.
#'
#' `parent_resolve` is a function with (up to) two arguments:
#' `value` and `resolve`. It will be called with the value of the
#' parent, the `resolve` callback of the deferred.
#' `parent_resolve` can resolve the deferred by calling the supplied `resolve`
#' callback, or it can keep waiting on other parents and/or external
#' computation. It may throw an error to fail the deferred.
#'
#' `parent_resolve` allows some shorthands as well:
#' * `NULL`: the deferred is resolved with the value of the parent.
#' * A function with no arguments: this function is called, and the deferred
#'   resolves to its return value.
#' * A function with one argument: this function is called with the value
#'   of the parent as the argument, and the deferred is resolved to its
#'   return value.
#' * A function with arguments `value` and `resolve`. This function is
#'   called with the value of the parent, and the resolve callback of the
#'   deferred.
#'
#' `parent_reject` is a function with (up to) two arguments:
#' `value`, `resolve`. It will be called with the error object
#' thrown by the parent.
#'
#' `parent_resolve` can resolve the deferred by calling the supplied
#' `resolve` callback, or it can keep waiting on other parents and/or
#' external computation. It may throw an error to fail the deferred. It may
#' also re-throw the error received from the parent, if it does not wish
#' to handle it.
#'
#' `parent_reject` also accepts some shorthands as well:
#' * `NULL`: the deferred throws the same error as the parent.
#' * A function with no arguments: this function is called, and the deferred
#'   resolves to its return value.
#' * A function with one argument: this function is called with the value
#'   of the parent as the argument, and the deferred is resolved to its
#'   return value.
#' * A function with arguments `value` and `resolve`. This function is
#'   called with the value of the parent, and the resolve callback of the
#'   deferred.

#' * A list of named error handlers, corresponding to the error handlers
#'   of `$catch()` (and `tryCatch()`). If these error handlers handle the
#'   parent's error, the deferred is resolved with the result of the
#'   handlers. Otherwise the deferred will be failed with the parent's
#'   error. The error handlers may also throw a new error.
#'
#' @section Error handling:
#'
#' The action function of the deferred, and also the `parent_resolve` and
#' `parent_reject` handlers may throw errors if the deferred cannot be
#' computed. Errors can be handled wit the `$catch()` member function:
#'
#' ```
#' dx$catch(...)
#' ```
#'
#' It takes the same named error handler arguments as `tryCatch()`.
#'
#' Technically, `$catch()` creates a new deferred value, and this new
#' deferred value is resolved to the result of the error handlers. Of the
#' handlers do not handle the error, then the new deferred will fail
#' with the same error.
#'
#' The `$finally()` method can be used to run create finalizer code that
#' runs when a deferred is resolved or when it fails. It can be used to
#' close database connections or other resources:
#'
#' ```
#' dx$finally(on_finally)
#' ```
#'
#' Technically, `$finally()` creates a new deferred, which will resolve
#' or fail the same way as the original one, but before doing that it will
#' call the `on_finally` function with no arguments.
#'
#' @section Builtin async functions:
#'
#' The async package comes with some basic async functions:
#' * [delay()] sets a timer and then resolves to `TRUE`.
#' * [async_constant()] resolves successfully to its argument.
#' * [http_get()] and [http_head()] make HTTP GET and HEAD requests.
#'
#' @section Combining async values:
#'
#' Async computation (just like ordinary sync computation) usually
#' consists of several steps that needs to be performed in the specified
#' order. The `$then()` method specifies that a step of computation needs
#' to be performed after the deferred value is known:
#'
#' ```
#' dx$then(on_fulfilled)
#' ```
#'
#' `on_fulfilled` is a function with zero or one formal arguments.
#' It will be called once the result of the deferred is known, with its
#' result. (The result is omitted if it has no arguments).
#'
#' `$then()` creates another deferred value, that will resolve to the
#' result of the `on_fulfilled` callback. Should this callback return
#' with a deferred value, then `$then()` the deferred value will be a
#' child of this newly creted deferred, and only resolve after that.
#'
#' See also [when_all()], [when_some()] and [when_any()], which can combine
#' multiple deferred values into one.
#'
#' You cannot call `$then()` (or [when_any()], [when_all()], etc. on the
#' same deferred value multiple times, unless it is a shared deferred
#' value. See _Ownership_ below.
#'
#' The [async_reflect()], [async_retry()], [async_sequence()],
#' [async_timeout()], [async_until()] and [async_whilst()] functions are
#' helpers for more complex async control flow.
#'
#' @section Ownership:
#'
#' async follows a strong ownership model. Each deferred value must be
#' owned by exactly one other deferred value  (unless they are shared, see
#' below).
#'
#' After a `dx2 <- dx$then()` call, the `dx` deferred is _owned_ by the
#' newly created deferred value. (The same applied to [when_any()], etc.)
#' This means that it is not possible to call `$then()` on the same
#' deferred value multiple times. The deferred value that is synchronized
#' by calling [synchronise()] on it, is owned by [synchronise()], see
#' _Synchronization_ below.
#'
#' The deferred values of an async phase form a directed graph, which we
#' call the async DAG (directed, acyclic graph). Usually (when no deferred
#' is shared, see below), this DAG is a rooted tree, the root of the tree
#' is the synchronised deferred, the final result of the async phase.
#'
#' @section Shared Deferred Values:
#'
#' In the rare cases when the strong ownership model is too restrictive,
#' a deferred value can be marked as _shared_:
#'
#' ```
#' dx$share()
#' ```
#'
#' This has the following implications:
#' * A shared deferred value can have multiple children (owners) in the
#'   async DAG.
#' * A shared deferred value is started after its first child is started.
#' * A shared deferred value is not auto-cancelled when all of its children
#'   are finished. (Because it might have more children in the future.)
#' * A shared deferred value is still auto-cancelled at the end of the
#'   event loop.
#'
#' Use shared deferred values sparingly, only when they are really needed,
#' as they forbid auto-cancellation, so deferred values will hold on to
#' resources longer, until the async phase is finished.
#'
#' @section Synchronization:
#'
#' async allows embedding asynchronous computation in synchronous code.
#' The execution of such a program has a sync phase and async phases. When the
#' program starts, it is in the sync phase. In the sync phase you cannot
#' create deferred values. (But you can still define (async) functions, that
#' will create deferred values when called.)
#'
#' To enter into an async phase, call [synchronise()] on an expression that
#' evaluates to a deferred value. The async phase will last until this
#' deferred value is computed or an error is thrown (and the error reaches
#' [synchronise()]).
#'
#' [synchronise()] creates an event loop, which manages the computation of
#' the deferred values in this particular async phase.
#'
#' Async phases can be embedded into each other. I.e. a program may call
#' [synchronise()] while in the async phase. The outer async phase's event
#' loop then stops until the inner async phase terminates. Deferred values
#' cannot be passed through a `synchronise()` barrier, to anoter (sync or
#' async phase). Should this happen, an error is reported on the first
#' operation on the leaked deferred value.
#'
#' In a typical application, a function is implemented asynchronously, and
#' then used synchronously by the interactive user, or another piece of
#' synchronous code, via [synchronise()] calls. The following example makes
#' three HTTP requests in parallel:
#'
#' ```
#' http_status3 <- function() {
#'   http_status <- function(url) {
#'     http_get(url)$then(function(response) response$status_code)
#'   }
#'   r1 <- http_status("https://eu.httpbin.org/status/403")
#'   r2 <- http_status("https://eu.httpbin.org/status/404")
#'   r3 <- http_status("https://eu.httpbin.org/status/200")
#'   when_all(r1, r2, r3)
#' }
#' synchronise(http_status3())
#' ```
#'
#' This async function can also be used asychronously, as a parent of
#' another deferred value, in an async phase.
#'
#' @section Lazy evaluation:
#'
#' async does not evaluate deferred values that are not part of the async
#' DAG of the async phase. These are clearly not needed to compute the
#' result of the async phase, so it would be a waste of resources working on
#' them. (It is also unclear how their errors should be handled.)
#'
#' In the following example, `d1` and `d2` are created, but they are not
#' part of the async DAG, so they are never evaluated.
#'
#' ```
#' do <- function() {
#'   d1 <- delay(1/100)$then(function() print("d1"))
#'   d2 <- d1$then(function() print("d2"))
#'   d3 <- delay(1/100)$then(function() print("d3"))
#'   d4 <- d3$then(function() print("d4"))
#'   d4
#' }
#' invisible(synchronise(do()))
#' ```
#'
#' @section Cancellation:
#'
#' The computation of a deferred can be cancelled when it is not needed
#' any more:
#'
#' ```
#' dx$cancel(reason = "Cancelled")
#' ```
#'
#' This will _fail_ the children of the deferred, unless they have been
#' completed already. It will also auto-cancel the parent DAG of the
#' deferred, unless they are shared deferreds, see the next Section.
#'
#' @section Auto-cancellation:
#'
#' In an async phase, it might happen that parts of the async DAG are not
#' needed for the final result any more. E.g. if a parent of a `when_all()`
#' node throws an error, then the other parents don't have to be computed.
#' In this case the event loop of the phase automatically cancels these
#' deferred values. Similarly, if a single parent of a [when_any()] node is
#' resolved, the other parents can be cancelled.
#'
#' In general, if a node of the async DAG is resolved, the whole directed
#' DAG, rooted at that node, can be cancelled (except for nodes that were
#' already resolved and nodes that have already failed).
#'
#' Auto-cancellation is very convenient, as you can be sure that resources
#' are free as soon as they are not needed. Some practical examples:
#'
#' * Making HTTP requests to many mirror web sites, to check their response
#'   time. As soon as the first reply is in, the rest of the HTTP requests
#'   are cancelled.
#' * In multi-process computation, as soon as one process fails, the rest are
#'   automatically cancelled. (Unless the failure is handled, of course.)
#'
#' async also has another type of cancellation, when [synchronise()] is
#' interrupted externally, either by the user or some system error. In this
#' case all processes and resources that were created in the event loop,
#' are cancelled and freed.
#'
#' Shared deferred values (see `$share()`) are not auto-cancelled when their
#' children are resolved or errored, but they are always cancelled at the
#' end of the async phase.
#'
#' @section Progress bars:
#'
#' A deferred value may report on its progress, if its action has a progress
#' callback. The progress callback is called with a list that describes
#' and event. We suggest that it always has an `event` entry, which is a
#' simple string. The rest of the list entries can be defined as needed,
#' but typically there will be a counter counting ticks, or a ratio
#' describing what part of the computation is already. See [http_get()]
#' for an async function that reports progress.
#'
#' @section Collections helper functions:
#'
#' async provides some utilities that make it easier to deal with
#' collections of deferred values:
#'
#' The current iterators:
#' * [async_map()] applies an async function to all elements of a vector or
#'   list (collection).
#' * [async_detect()] finds an element of a collection that passed an async
#'   truth test.
#' * [async_every()] checks if every element of a collection satisfies an
#'   async predicate. [async_some()] checks if any element does that.
#' * [async_filter()] keeps elements that pass an async truth test.
#'
#' @section Control flow helper functions:
#'
#' Control flow with deferred values can be challenging. Some helpers:
#' * [async_reflect()] creates an async function that always succeeds.
#'   This is useful if you want to apply it to a collection, and don't
#'   want to stop at the first error.
#' * [async_retry()] tries an async function a number of times.
#'   [async_retryable()] turns a regular function into a retryable one.
#' * [async_sequence()] chains two async functions. Calling their sequence
#'   is equivalent calling '$then()` on them, but [async_sequence()] is
#'   easier to use programmatically.
#' * [async_until()] and [async_whilst()] let you call an async function
#'   repeatedly, until or while a (syncronous) condition holds.
#' * [async_timeout()] runs an async function with a timeout.
#'
#' @section Examples:
#' Please see the README and the vignettes for examples.
#' @name deferred
#' @noRd
NULL

#' @importFrom R6 R6Class
#' @noRd

deferred <- R6Class(
  "deferred",
  public = list(
    initialize = function(
      action = NULL,
      on_progress = NULL,
      on_cancel = NULL,
      parents = NULL,
      parent_resolve = NULL,
      parent_reject = NULL,
      type = NULL,
      call = sys.call(-1),
      event_emitter = NULL
    )
      async_def_init(
        self,
        private,
        action,
        on_progress,
        on_cancel,
        parents,
        parent_resolve,
        parent_reject,
        type,
        call,
        event_emitter
      ),
    then = function(on_fulfilled) def_then(self, private, on_fulfilled),
    catch = function(...) def_catch(self, private, ...),
    finally = function(on_finally) def_finally(self, private, on_finally),
    cancel = function(reason = "Cancelled") def_cancel(self, private, reason),
    share = function() {
      private$shared <<- TRUE
      invisible(self)
    },

    event_emitter = NULL
  ),

  private = list(
    action = NULL,
    running = FALSE,
    id = NULL,
    type = NULL,
    state = c("pending", "fulfilled", "rejected")[1],
    event_loop = NULL,
    value = NULL,
    children = list(),
    progress_callback = NULL,
    cancel_callback = NULL,
    cancelled = FALSE,
    dead_end = FALSE,
    parents = NULL,
    parent_resolve = NULL,
    parent_reject = NULL,
    shared = FALSE,
    mycall = NULL,

    run_action = function() def__run_action(self, private),

    null = function() def__null(self, private),

    resolve = function(value) def__resolve(self, private, value),
    reject = function(reason) def__reject(self, private, reason),
    progress = function(data) def__progress(self, private, data),

    make_error_object = function(err)
      def__make_error_object(self, private, err),

    maybe_cancel_parents = function(reason)
      def__maybe_cancel_parents(self, private, reason),
    add_as_parent = function(child) def__add_as_parent(self, private, child),
    update_parent = function(old, new)
      def__update_parent(self, private, old, new),

    get_info = function() def__get_info(self, private)
  )
)

async_def_init <- function(
  self,
  private,
  action,
  on_progress,
  on_cancel,
  parents,
  parent_resolve,
  parent_reject,
  type,
  call,
  event_emitter
) {
  private$type <- type
  private$id <- get_id()
  private$event_loop <- get_default_event_loop()
  private$parents <- parents
  private$action <- action
  private$mycall <- call
  self$event_emitter <- event_emitter

  "!DEBUG NEW `private$id` (`type`)"

  assert_that(is.null(on_progress) || is.function(on_progress))
  private$progress_callback <- on_progress
  assert_that(is.null(on_cancel) || is.function(on_cancel))
  private$cancel_callback <- on_cancel

  ## Handle the parents

  private$parent_resolve <- def__make_parent_resolve(parent_resolve)
  private$parent_reject <- def__make_parent_reject(parent_reject)

  for (prt in parents) {
    prt_pvt <- get_private(prt)
    prt_pvt$add_as_parent(self)
  }

  invisible(self)
}

def__run_action <- function(self, private) {
  if (private$running) return()
  action <- private$action
  private$running <- TRUE
  private$action <- NULL
  "!DEBUG ACTION `private$type` `private$id`"

  if (!is.null(action)) {
    if (!is.function(action)) {
      action <- as.function(action)
      formals(action) <- alist(resolve = NULL, progress = NULL)
    }
    assert_that(is_action_function(action))

    action_args <- names(formals(action))
    args <- list(private$resolve)
    if (!is.na(pr_arg <- match("progress", action_args))) {
      args$progress <- private$progress
    }

    private$event_loop$add_next_tick(
      function() {
        if (isTRUE(getOption("async_debug_steps", FALSE))) debug1(action)
        `__async_data__` <- list(private$id, "action", self, skip = 2L)
        do.call(action, args)
      },
      function(err, res) if (!is.null(err)) private$reject(err)
    )
  }

  ## If some parents are done, we want them to notify us.
  ## We also start the ones that are not running yet
  for (prt in private$parents) {
    prt_priv <- get_private(prt)
    if (prt_priv$state != "pending") {
      def__call_then(
        if (prt_priv$state == "fulfilled") "parent_resolve" else
          "parent_reject",
        self,
        prt_priv$value
      )
    }
    prt_priv$run_action()
  }
}

def_then <- function(self, private, on_fulfilled = NULL, on_rejected = NULL) {
  force(self)
  force(private)

  if (!identical(private$event_loop, get_default_event_loop())) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error"
    )
    stop(err)
  }

  if (!is_deferred(on_fulfilled)) {
    parent_resolve <- def__make_parent_resolve(on_fulfilled)
    parent_reject <- def__make_parent_reject(on_rejected)

    deferred$new(
      parents = list(self),
      type = paste0("then-", private$id),
      parent_resolve = parent_resolve,
      parent_reject = parent_reject,
      call = sys.call(-1)
    )
  } else {
    private$add_as_parent(on_fulfilled)
    child_private <- get_private(on_fulfilled)
    child_private$parents <- c(child_private$parents, self)
    self
  }
}

def_catch <- function(self, private, ...) {
  def_then(self, private, on_rejected = list(...))
}

def_finally <- function(self, private, on_finally) {
  force(on_finally)
  def_then(
    self,
    private,
    on_fulfilled = function(value) {
      on_finally()
      value
    },
    on_rejected = function(reason) {
      on_finally()
      stop(reason)
    }
  )
}

def_cancel <- function(self, private, reason) {
  if (private$state != "pending") return()
  cancel_cond <- structure(
    list(message = reason %||% "Deferred computation cancelled", call = NULL),
    class = c("async_cancelled", "error", "condition")
  )
  private$reject(cancel_cond)
  invisible(self)
}

def__null <- function(self, private) {
  self$.__enclos_env__$private$dead_end <- TRUE
  invisible(self)
}

def__resolve <- function(self, private, value) {
  if (private$cancelled) return()
  if (private$state != "pending") return()

  if (is_deferred(value)) {
    private$parent_resolve <- def__make_parent_resolve(NULL)
    private$parent_reject <- def__make_parent_reject(NULL)

    # we need this in case self was shared and had multiple children
    val_pvt <- get_private(value)
    val_pvt$id <- private$id
    val_pvt$shared <- private$shared
    val_pvt$dead_end <- private$dead_end # This should not happen, though

    for (child in private$children) {
      ch_pvt <- get_private(child)
      ch_pvt$update_parent(self, value)
    }

    val_pvt$run_action()
  } else {
    if (!private$dead_end && !length(private$children) && !private$shared) {
      ## This cannot happen currently
      "!DEBUG ??? DEAD END `private$id`" # nocov
      warning("Computation going nowhere...") # nocov
    }

    "!DEBUG +++ RESOLVE `private$id`"
    private$state <- "fulfilled"
    private$value <- value
    for (child in private$children) {
      def__call_then("parent_resolve", child, value)
    }
    private$maybe_cancel_parents(private$value)
    private$parents <- NULL
  }
}

#' Create an error object for a rejected deferred computation
#'
#' * Make sure that the error is an error object.
#' * Make sure that the error has the correct classes.
#'
#' @param self self
#' @param private private self
#' @return error object
#'
#' @keywords internal

def__make_error_object <- function(self, private, err) {
  class(err) <- unique(c("async_rejected", class(err)))
  err
}

def__make_parent_resolve <- function(fun) {
  if (is.null(fun)) {
    function(value, resolve) resolve(value)
  } else if (!is.function(fun)) {
    fun <- as.function(fun)
    function(value, resolve) resolve(fun(value))
  } else if (num_args(fun) == 0) {
    function(value, resolve) resolve(fun())
  } else if (num_args(fun) == 1) {
    function(value, resolve) resolve(fun(value))
  } else if (identical(names(formals(fun)), c("value", "resolve"))) {
    fun
  } else {
    stop("Invalid parent_resolve callback")
  }
}

def__make_parent_reject <- function(fun) {
  if (is.null(fun)) {
    function(value, resolve) stop(value)
  } else if (is.list(fun)) {
    def__make_parent_reject_catch(fun)
  } else if (!is.function(fun)) {
    fun <- as.function(fun)
    function(value, resolve) resolve(fun(value))
  } else if (num_args(fun) == 0) {
    function(value, resolve) resolve(fun())
  } else if (num_args(fun) == 1) {
    function(value, resolve) resolve(fun(value))
  } else if (identical(names(formals(fun)), c("value", "resolve"))) {
    fun
  } else {
    stop("Invalid parent_reject callback")
  }
}

def__make_parent_reject_catch <- function(handlers) {
  handlers <- lapply(handlers, as.function)
  function(value, resolve) {
    ok <- FALSE
    ret <- tryCatch(
      {
        quo <- as.call(c(list(quote(tryCatch), quote(stop(value))), handlers))
        ret <- eval(quo)
        ok <- TRUE
        ret
      },
      error = function(x) x
    )

    if (ok) resolve(ret) else stop(ret)
  }
}

def__reject <- function(self, private, reason) {
  if (private$cancelled) return()
  if (private$state != "pending") return()

  ## 'reason' cannot be a deferred here

  "!DEBUG !!! REJECT `private$id`"
  private$state <- "rejected"
  private$value <- private$make_error_object(reason)
  if (inherits(private$value, "async_cancelled")) {
    private$cancelled <- TRUE
  }
  if (!is.null(private$cancel_callback)) {
    private$cancel_callback(conditionMessage(private$value))
  }
  for (child in private$children) {
    def__call_then("parent_reject", child, private$value)
  }
  private$maybe_cancel_parents(private$value)
  private$parents <- NULL
}

def__maybe_cancel_parents <- function(self, private, reason) {
  for (parent in private$parents) {
    if (is.null(parent)) next

    parent_priv <- get_private(parent)
    if (parent_priv$state != "pending") next
    if (parent_priv$shared) next
    parent$cancel(reason)
  }
}

def__call_then <- function(which, x, value) {
  force(value)
  private <- get_private(x)
  if (!private$running) return()
  if (private$state != "pending") return()

  cb <- private[[which]]
  private$event_loop$add_next_tick(
    function() {
      if (isTRUE(getOption("async_debug_steps", FALSE))) {
        debug1(private[[which]]) # nocov
      }
      `__async_data__` <- list(private$id, "parent", x)
      private[[which]](value, private$resolve)
    },
    function(err, res) if (!is.null(err)) private$reject(err)
  )
}

def__add_as_parent <- function(self, private, child) {
  "!DEBUG EDGE [`private$id` -> `get_private(child)$id`]"

  if (!identical(private$event_loop, get_private(child)$event_loop)) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error"
    )
    stop(err)
  }
  if (length(private$children) && !private$shared) {
    stop("Deferred value is already owned")
  }

  private$children <- c(private$children, child)

  if (get_private(child)$running) private$run_action()
  if (private$state == "pending") {
    ## Nothing to do
  } else if (private$state == "fulfilled") {
    def__call_then("parent_resolve", child, private$value)
  } else {
    def__call_then("parent_reject", child, private$value)
  }
}

def__update_parent <- function(self, private, old, new) {
  for (i in seq_along(private$parents)) {
    if (identical(private$parents[[i]], old)) {
      private$parents[[i]] <- new
    }
  }

  new_pvt <- get_private(new)
  new_pvt$add_as_parent(self)
}

def__progress <- function(self, private, data) {
  if (private$state != "pending") return()
  if (is.null(private$progress_callback)) return()
  private$progress_callback(data)
}

def__get_info <- function(self, private) {
  res <- data.frame(
    stringsAsFactors = FALSE,
    id = private$id,
    parents = I(list(viapply(private$parents, function(x) get_private(x)$id))),
    label = as.character(private$id),
    call = I(list(private$mycall)),
    children = I(list(viapply(
      private$children,
      function(x) get_private(x)$id
    ))),
    type = private$type %||% "unknown",
    running = private$running,
    state = private$state,
    cancelled = private$cancelled,
    shared = private$shared
  )
  src <- get_source_position(private$mycall)
  res$filename <- src$filename
  res$position <- src$position
  res$label <- paste0(
    res$id,
    " ",
    if (private$state == "fulfilled") paste0(cli::symbol$tick, " "),
    if (private$state == "rejected") paste0(cli::symbol$cross, "  "),
    deparse(private$mycall)[1],
    " @ ",
    res$filename,
    ":",
    res$position
  )

  res
}

#' Is object a deferred value?
#'
#' @param x object
#' @return Whether it is a deferred value.
#'
#' @noRd
#' @examples
#' is_deferred(1:10)
#' afun <- function() {
#'   print(is_deferred(dx <- delay(1/100)))
#'   dx
#' }
#' synchronise(afun())

is_deferred <- function(x) {
  inherits(x, "deferred")
}

#' Delay async computation for the specified time
#'
#' Since R is single-threaded, the deferred value might be resolved (much)
#' later than the specified time period.
#'
#' @param delay Time interval in seconds, the amount of time to delay
#'   to delay the execution. It can be a fraction of a second.
#' @return A deferred object.
#'
#' @noRd
#' @examples
#' \donttest{
#' ## Two HEAD requests with 1/2 sec delay between them
#' resp <- list()
#' afun <- async(function() {
#'   http_head("https://eu.httpbin.org?q=2")$
#'     then(function(value) resp[[1]] <<- value$status_code)$
#'     then(function(...) delay(1/2))$
#'     then(function(...) http_head("https://eu.httpbin.org?q=2"))$
#'     then(function(value) resp[[2]] <<- value$status_code)
#' })
#' synchronise(afun())
#' resp
#' }

delay <- function(delay) {
  force(delay)
  id <- NULL
  deferred$new(
    type = "delay",
    call = sys.call(),
    action = function(resolve) {
      assert_that(is_time_interval(delay))
      force(resolve)
      id <<- get_default_event_loop()$add_delayed(
        delay,
        function() TRUE,
        function(err, res) resolve(TRUE)
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

delay <- mark_as_async(delay)

#' Find the value of a match, asynchronously
#'
#' All predicates are running in parallel, and the returned match
#' is not guaranteed to be the first one.
#'
#' @param .x A list or atomic vector.
#' @param .p An asynchronous predicate function.
#' @param ... Additional arguments to the predicate function.
#' @param .limit Number of elements to process simulateneously.
#'   If it is 1, then the predicate is applied sequentially.
#' @return A deferred value for the result.
#'
#' @family async iterators
#' @noRd
#' @examples
#' \donttest{
#' synchronise(async_detect(
#'   c("https://eu.httpbin.org/status/404", "https://eu.httpbin.org",
#'     "https://eu.httpbin.org/status/403"),
#'   async_sequence(http_head, function(x) x$status_code == 200)
#' ))
#' }

async_detect <- function(.x, .p, ..., .limit = Inf) {
  if (.limit < length(.x)) {
    async_detect_limit(.x, .p, ..., .limit = .limit)
  } else {
    async_detect_nolimit(.x, .p, ...)
  }
}

async_detect <- mark_as_async(async_detect)

async_detect_nolimit <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE

  self <- deferred$new(
    type = "async_detect",
    call = sys.call(),
    action = function(resolve) {
      lapply(seq_along(defs), function(idx) {
        defs[[idx]]$then(function(val) if (isTRUE(val)) idx)$then(self)
      })
      if (nx == 0) resolve(NULL)
    },
    parent_resolve = function(value, resolve) {
      if (!done && !is.null(value)) {
        done <<- TRUE
        resolve(.x[[value]])
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(NULL)
      }
    }
  )
}

async_detect_limit <- function(.x, .p, ..., .limit = .limit) {
  len <- length(.x)
  nx <- len
  .p <- async(.p)
  args <- list(...)

  done <- FALSE
  nextone <- .limit + 1L
  firsts <- lapply(.x[seq_len(.limit)], .p, ...)

  self <- deferred$new(
    type = "async_detect (limit)",
    call = sys.call(),
    action = function(resolve) {
      lapply(seq_along(firsts), function(idx) {
        firsts[[idx]]$then(function(val) if (isTRUE(val)) idx)$then(self)
      })
      if (nx == 0) resolve(NULL)
    },
    parent_resolve = function(value, resolve) {
      if (!done && !is.null(value)) {
        done <<- TRUE
        resolve(.x[[value]])
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) {
          resolve(NULL)
        } else if (nextone <= len) {
          idx <- nextone
          dx <- .p(.x[[nextone]], ...)
          dx$then(function(val) if (isTRUE(val)) idx)$then(self)
          nextone <<- nextone + 1L
        }
      }
    }
  )

  self
}

#' @importFrom R6 R6Class

event_loop <- R6Class(
  "event_loop",
  public = list(
    initialize = function() el_init(self, private),

    add_http = function(
      handle,
      callback,
      file = NULL,
      progress = NULL,
      data = NULL
    ) el_add_http(self, private, handle, callback, file, progress, data),
    http_setopt = function(total_con = NULL, host_con = NULL, multiplex = NULL)
      el_http_setopt(self, private, total_con, host_con, multiplex),

    add_process = function(conns, callback, data)
      el_add_process(self, private, conns, callback, data),
    add_r_process = function(conns, callback, data)
      el_add_r_process(self, private, conns, callback, data),
    add_pool_task = function(callback, data)
      el_add_pool_task(self, private, callback, data),
    add_delayed = function(delay, func, callback, rep = FALSE)
      el_add_delayed(self, private, delay, func, callback, rep),
    add_next_tick = function(func, callback, data = NULL)
      el_add_next_tick(self, private, func, callback, data),

    cancel = function(id) el_cancel(self, private, id),
    cancel_all = function() el_cancel_all(self, private),

    run = function(mode = c("default", "nowait", "once"))
      el_run(self, private, mode = match.arg(mode)),

    suspend = function() el_suspend(self, private),
    wakeup = function() el_wakeup(self, private)
  ),

  private = list(
    create_task = function(callback, ..., id = NULL, type = "foobar")
      el__create_task(self, private, callback, ..., id = id, type = type),
    ensure_pool = function() el__ensure_pool(self, private),
    get_poll_timeout = function() el__get_poll_timeout(self, private),
    run_pending = function() el__run_pending(self, private),
    run_timers = function() el__run_timers(self, private),
    is_alive = function() el__is_alive(self, private),
    update_time = function() el__update_time(self, private),
    io_poll = function(timeout) el__io_poll(self, private, timeout),
    update_curl_data = function() el__update_curl_data(self, private),

    id = NULL,
    time = Sys.time(),
    stop_flag = FALSE,
    tasks = list(),
    timers = Sys.time()[numeric()],
    pool = NULL,
    curl_fdset = NULL, # return value of multi_fdset()
    curl_poll = TRUE, # should we poll for curl sockets?
    curl_timer = NULL, # call multi_run() before this
    next_ticks = character(),
    worker_pool = NULL,
    http_opts = NULL
  )
)

el_init <- function(self, private) {
  private$id <- new_event_loop_id()
  invisible(self)
}

el_add_http <- function(self, private, handle, callback, progress, file, data) {
  self
  private
  handle
  callback
  progress
  outfile <- file
  data

  id <- private$create_task(
    callback,
    list(handle = handle, data = data),
    type = "http"
  )
  private$ensure_pool()
  if (!is.null(outfile)) cat("", file = outfile)

  content <- NULL

  curl::multi_add(
    handle = handle,
    pool = private$pool,
    done = function(response) {
      task <- private$tasks[[id]]
      task$data$data$event_emitter$emit("end")
      private$tasks[[id]] <- NULL
      response$content <- do.call(c, as.list(content))
      response$file <- outfile
      task$callback(NULL, response)
    },
    data = function(bytes, ...) {
      task <- private$tasks[[id]]
      task$data$data$event_emitter$emit("data", bytes)
      if (!is.null(outfile)) {
        ## R runs out of connections very quickly, especially because they
        ## are not removed until a gc(). However, calling gc() is
        ## expensive, so we only do it if we have to. This is a temporary
        ## solution until we can use our own connections, that are not
        ## so limited in their numbers.
        con <- tryCatch(
          file(outfile, open = "ab"),
          error = function(e) {
            gc()
            file(outfile, open = "ab")
          } # nocov
        )
        writeBin(bytes, con)
        close(con)
      } else {
        content <<- c(content, list(bytes))
      }
    },
    fail = function(error) {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      error <- make_error(message = error)
      class(error) <- unique(c(
        "async_rejected",
        "async_http_error",
        class(error)
      ))
      task$callback(error, NULL)
    }
  )
  id
}

el_add_process <- function(self, private, conns, callback, data) {
  self
  private
  conns
  callback
  data
  data$conns <- conns
  private$create_task(callback, data, type = "process")
}

el_add_r_process <- function(self, private, conns, callback, data) {
  self
  private
  conns
  callback
  data
  data$conns <- conns
  private$create_task(callback, data, type = "r-process")
}

el_add_pool_task <- function(self, private, callback, data) {
  self
  private
  callback
  data
  id <- private$create_task(callback, data, type = "pool-task")
  if (is.null(async_env$worker_pool)) {
    async_env$worker_pool <- worker_pool$new()
  }
  async_env$worker_pool$add_task(data$func, data$args, id, private$id)
  id
}

el_add_delayed <- function(self, private, delay, func, callback, rep) {
  force(self)
  force(private)
  force(delay)
  force(func)
  force(callback)
  force(rep)
  id <- private$create_task(
    callback,
    data = list(delay = delay, func = func, rep = rep),
    type = "delayed"
  )
  # This has to be real time, because our event loop time might
  # be very much in the past when his is called.
  private$timers[id] <- Sys.time() + as.difftime(delay, units = "secs")
  id
}

el_add_next_tick <- function(self, private, func, callback, data) {
  force(self)
  force(private)
  force(callback)
  force(data)
  data$func <- func
  id <- private$create_task(callback, data = data, type = "nexttick")
  private$next_ticks <- c(private$next_ticks, id)
}

el_cancel <- function(self, private, id) {
  private$next_ticks <- setdiff(private$next_ticks, id)
  private$timers <- private$timers[setdiff(names(private$timers), id)]
  if (id %in% names(private$tasks) && private$tasks[[id]]$type == "http") {
    curl::multi_cancel(private$tasks[[id]]$data$handle)
  } else if (
    id %in%
      names(private$tasks) &&
      private$tasks[[id]]$type %in% c("process", "r-process")
  ) {
    private$tasks[[id]]$data$process$kill()
  } else if (
    id %in% names(private$tasks) && private$tasks[[id]]$type == "pool-task"
  ) {
    async_env$worker_pool$cancel_task(id)
  }
  private$tasks[[id]] <- NULL
  invisible(self)
}

el_cancel_all <- function(self, private) {
  http <- curl::multi_list(pool = private$pool)
  lapply(http, curl::multi_cancel)
  private$next_ticks <- character()
  private$timers <- Sys.time()[numeric()]

  ## Need to cancel pool tasks, these are interrupts for the workers
  types <- vcapply(private$tasks, "[[", "type")
  ids <- vcapply(private$tasks, "[[", "id")
  for (id in ids[types == "pool-task"]) {
    self$cancel(id)
  }

  private$tasks <- list()
  invisible(self)
}

el_run <- function(self, private, mode) {
  ## This is closely modeled after the libuv event loop, on purpose,
  ## because some time we might switch to that.

  alive <- private$is_alive()
  if (!alive) private$update_time()

  while (alive && !private$stop_flag) {
    private$update_time()
    private$update_curl_data()
    private$run_timers()
    ran_pending <- private$run_pending()
    ## private$run_idle()
    ## private$run_prepare()

    timeout <- 0
    if ((mode == "once" && !ran_pending) || mode == "default") {
      timeout <- private$get_poll_timeout()
    }

    private$io_poll(timeout)
    ## private$run_check()
    ## private$run_closing_handles()

    if (mode == "once") {
      ## If io_poll returned without doing anything, that means that
      ## we have some timers that are due, so run those.
      ## At this point we have surely made progress
      private$update_time()
      private$run_timers()
    }

    alive <- private$is_alive()
    if (mode == "once" || mode == "nowait") break
  }

  private$stop_flag <- FALSE

  alive
}

el_suspend <- function(self, private) {
  ## TODO
}

el_wakeup <- function(self, private) {
  ## TODO
}

el__run_pending <- function(self, private) {
  next_ticks <- private$next_ticks
  private$next_ticks <- character()
  for (id in next_ticks) {
    task <- private$tasks[[id]]
    private$tasks[[id]] <- NULL
    call_with_callback(
      task$data$func,
      task$callback,
      info = task$data$error_info
    )
  }

  ## Check for workers from the pool finished before, while another
  ## event loop was active
  finished_pool <- FALSE
  pool <- async_env$worker_pool
  if (!is.null(pool)) {
    done_pool <- pool$list_tasks(event_loop = private$id, status = "done")
    finished_pool <- nrow(done_pool) > 0
    for (tid in done_pool$id) {
      task <- private$tasks[[tid]]
      private$tasks[[tid]] <- NULL
      res <- pool$get_result(tid)
      err <- res$error
      res <- res[c("result", "stdout", "stderr")]
      task$callback(err, res)
    }
  }

  length(next_ticks) > 0 || finished_pool
}

el__io_poll <- function(self, private, timeout) {
  types <- vcapply(private$tasks, "[[", "type")

  ## The things we need to poll, and their types
  ## We put the result here as well
  pollables <- data.frame(
    stringsAsFactors = FALSE,
    id = character(),
    pollable = I(list()),
    type = character(),
    ready = character()
  )

  ## HTTP.
  if (private$curl_poll) {
    curl_pollables <- data.frame(
      stringsAsFactors = FALSE,
      id = "curl",
      pollable = I(list(processx::curl_fds(private$curl_fdset))),
      type = "curl",
      ready = "silent"
    )
    pollables <- rbind(pollables, curl_pollables)
  }

  ## Processes
  proc <- types %in% c("process", "r-process")
  if (sum(proc)) {
    conns <- unlist(
      lapply(
        private$tasks[proc],
        function(t) t$data$conns
      ),
      recursive = FALSE
    )
    proc_pollables <- data.frame(
      stringsAsFactors = FALSE,
      id = names(private$tasks)[proc],
      pollable = I(conns),
      type = types[proc],
      ready = rep("silent", sum(proc))
    )
    pollables <- rbind(pollables, proc_pollables)
  }

  ## Pool
  px_pool <- if (!is.null(async_env$worker_pool)) {
    async_env$worker_pool$get_poll_connections()
  }
  if (length(px_pool)) {
    pool_pollables <- data.frame(
      stringsAsFactors = FALSE,
      id = names(px_pool),
      pollable = I(px_pool),
      type = rep("pool", length(px_pool)),
      ready = rep("silent", length(px_pool))
    )
    pollables <- rbind(pollables, pool_pollables)
  }

  if (!is.null(private$curl_timer) && private$curl_timer <= private$time) {
    curl::multi_run(timeout = 0L, poll = TRUE, pool = private$pool)
    private$curl_timer <- NULL
  }

  if (nrow(pollables)) {
    ## OK, ready to poll
    pollables$ready <- unlist(processx::poll(pollables$pollable, timeout))

    ## Any HTTP?
    if (
      private$curl_poll &&
        pollables$ready[match("curl", pollables$type)] == "event"
    ) {
      curl::multi_run(timeout = 0L, poll = TRUE, pool = private$pool)
    }

    ## Any processes
    proc_ready <- pollables$type %in%
      c("process", "r-process") &
      pollables$ready == "ready"
    for (id in pollables$id[proc_ready]) {
      p <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      ## TODO: this should be async
      p$data$process$wait(1000)
      p$data$process$kill()
      res <- list(
        status = p$data$process$get_exit_status(),
        stdout = read_all(p$data$stdout, p$data$encoding),
        stderr = read_all(p$data$stderr, p$data$encoding),
        timeout = FALSE
      )

      error <- FALSE
      if (p$type == "r-process") {
        res$result <- tryCatch(
          {
            p$data$process$get_result()
          },
          error = function(e) {
            error <<- TRUE
            e
          }
        )
      }

      unlink(c(p$data$stdout, p$data$stderr))

      if (p$data$error_on_status && (error || res$status != 0)) {
        err <- make_error("process exited with non-zero status")
        err$data <- res
        res <- NULL
      } else {
        err <- NULL
      }
      p$callback(err, res)
    }

    ## Worker pool
    pool_ready <- pollables$type == "pool" & pollables$ready == "ready"
    if (sum(pool_ready)) {
      pool <- async_env$worker_pool
      done <- pool$notify_event(
        as.integer(pollables$id[pool_ready]),
        event_loop = private$id
      )
      mine <- intersect(done, names(private$tasks))
      for (tid in mine) {
        task <- private$tasks[[tid]]
        private$tasks[[tid]] <- NULL
        res <- pool$get_result(tid)
        err <- res$error
        res <- res[c("result", "stdout", "stderr")]
        task$callback(err, res)
      }
    }
  } else if (length(private$timers) || !is.null(private$curl_timer)) {
    Sys.sleep(timeout / 1000)
  }
}

el__create_task <- function(self, private, callback, data, ..., id, type) {
  id <- id %||% get_uuid()
  private$tasks[[id]] <- list(
    type = type,
    id = id,
    callback = callback,
    data = data,
    error = NULL,
    result = NULL
  )
  id
}

el__ensure_pool <- function(self, private) {
  getopt <- function(nm) {
    anm <- paste0("async_http_", nm)
    if (!is.null(v <- getOption(anm))) return(v)
    if (!is.na(v <- Sys.getenv(toupper(anm), NA_character_))) return(v)
    NULL
  }
  if (is.null(private$pool)) {
    private$http_opts <- list(
      total_con = getopt("total_con") %||% 100,
      host_con = getopt("host_con") %||% 6,
      multiplex = getopt("multiplex") %||% TRUE
    )
    private$pool <- curl::new_pool(
      total_con = private$http_opts$total_con,
      host_con = private$http_opts$host_con,
      multiplex = private$http_opts$multiplex
    )
  }
}

el_http_setopt <- function(self, private, total_con, host_con, multiplex) {
  private$ensure_pool()
  if (!is.null(total_con)) private$http_opts$total_con <- total_con
  if (!is.null(host_con)) private$http_opts$host_con <- host_con
  if (!is.null(multiplex)) private$http_opts$multiplex <- multiplex
  curl::multi_set(
    pool = private$pool,
    total_con = private$http_opts$total_con,
    host_con = private$http_opts$host_con,
    multiplex = private$http_opts$multiplex
  )
}

el__get_poll_timeout <- function(self, private) {
  t <- if (length(private$next_ticks)) {
    ## TODO: can this happen at all? Probably not, but it does not hurt...
    0 # nocov
  } else {
    max(0, min(Inf, private$timers - private$time))
  }

  if (!is.null(private$curl_timer)) {
    t <- min(t, private$curl_timer - private$time)
  }

  t <- max(t, 0)

  if (is.finite(t)) as.integer(t * 1000) else -1L
}

el__run_timers <- function(self, private) {
  expired <- names(private$timers)[private$timers <= private$time]
  expired <- expired[order(private$timers[expired])]
  for (id in expired) {
    task <- private$tasks[[id]]
    if (private$tasks[[id]]$data$rep) {
      ## If it is repeated, then re-init
      private$timers[id] <-
        private$time + as.difftime(task$data$delay, units = "secs")
    } else {
      ## Otherwise remove
      private$tasks[[id]] <- NULL
      private$timers <- private$timers[setdiff(names(private$timers), id)]
    }
    call_with_callback(task$data$func, task$callback)
  }
}

el__is_alive <- function(self, private) {
  length(private$tasks) > 0 ||
    length(private$timers) > 0 ||
    length(private$next_ticks) > 0
}

el__update_time <- function(self, private) {
  private$time <- Sys.time()
}

el__update_curl_data <- function(self, private) {
  private$curl_fdset <- curl::multi_fdset(private$pool)
  num_fds <- length(unique(unlist(private$curl_fdset[1:3])))
  private$curl_poll <- num_fds > 0
  private$curl_timer <- if ((t <- private$curl_fdset$timeout) != -1) {
    private$time + as.difftime(t / 1000.0, units = "secs")
  }
}

#' Generic Event Emitter
#'
#' This is a generic class that can be used to create event emitters.
#' It is mostly modelled after the 'node.js' `EventEmitter` class
#'
#' @section Usage:
#' ```
#' ee <- event_emitter$new(async = TRUE)
#' ee$listen_on(event, callback)
#' ee$listen_off(event, callback)
#' ee$listen_once(event, callback)
#' ee$emit(event, ...)
#' ee$get_event_names()
#' ee$get_listener_count(event)
#' ee$remove_all_listeners(event)
#' ```
#'
#' @section Arguments:
#' * `async`: Whether to call listeners asynchronously, i.e. in the next
#'     tick of the event loop.
#' * `event`: String, name of the event.
#' * `callback`: Function, listener to call when the event is emitted.
#'     Its arguments must match the arguments passed to the `$emit()`
#'     method. It is possible to add the same callback function multiple
#'     times as a listener. It will be called as many times, as many times
#'     it was added.
#' * `...`: Arguments to pass to the listeners. They can be named or
#'     unnnamed.
#'
#' @section Details:
#'
#' `ee$listen_on()` adds `callback` as a new listener for `event`. It is
#' always added to the end of the listener list. Listeners will be called in
#' the order they were added. It returns a reference to the `event_emitter`
#' object, so calls can be chained.
#'
#' `ee$listen_off()` removes the first instance of `callback` from the
#' listener list of `event`. It uses [base::identical()] to find the
#' listener to remove. If `callback` is not among the listeners, nothing
#' happens. Note that if you call this method from an event handler, that
#' does not affect the already emitted events. It returns a reference to
#' the `event_emitter` object, so calls can be chained.
#'
#' `ee$listen_once` is similar to `ee$listen_on()`, but the callback will
#' be only called for a single event, and then it will be removed.
#' (Technically, the listener is removed before the callback is called.)
#' It returns a reference to the `event_emitter` object, so calls can be
#' chained.
#'
#' `ee$emit()` emits an event. All listeners in its listener list will be
#' called, in the order they were added. The arguments are passed to the
#' listeners, so they have to be compatible with them.
#'
#' `ee$get_event_names()` returns the names of the active events,
#' in a character vector. An event is active if it has at least one
#' listener.
#'
#' `ee$get_listener_count()` returns the number of listeners for an event.
#'
#' `ee$remove_all_listener()`  removes all listeners for an an event.
#'
#' @section Error handling:
#' Errors are handled by special `error` events. If a listener errors,
#' and the event emitter has an active `error` event (i.e. some listeners
#' exist for `error`, then _all_ listeners are called, in the order they
#' were specified. They receive the originally thrown error object as the
#' single argument. The error object has an `event` entry, which contains
#' the event name the failed listener was called on.
#'
#' If the event emitter does not have any listeners for the `error` event,
#' then it throws an error. This error propagates to the next
#' synchronization barrier, i.e. the last `synchronise()` or
#' `run_event_loop()` call, which fails.
#'
#' In an error happen within an `error` listener, then the same happens,
#' the last `synchronise()` or `run_event_loop()` call fails. You can
#' wrap the body of the error listeners in a `tryCatch()` call,
#' if you want to avoid this.
#'
#' @noRd
#' @importFrom R6 R6Class

event_emitter <- R6Class(
  "event_emitter",
  public = list(
    initialize = function(async = TRUE) ee_init(self, private, async),

    listen_on = function(event, callback)
      ee_listen_on(self, private, event, callback),

    listen_off = function(event, callback)
      ee_listen_off(self, private, event, callback),

    listen_once = function(event, callback)
      ee_listen_once(self, private, event, callback),

    emit = function(event, ...) ee_emit(self, private, event, ...),

    get_event_names = function() ee_get_event_names(self, private),

    get_listener_count = function(event)
      ee_get_listener_count(self, private, event),

    remove_all_listeners = function(event)
      ee_remove_all_listeners(self, private, event)
  ),

  private = list(
    lsts = NULL,
    async = NULL,

    cleanup_events = function() ee__cleanup_events(self, private),
    error_callback = function(err, res)
      ee__error_callback(self, private, err, res)
  )
)

ee_init <- function(self, private, async) {
  assert_that(is_flag(async))
  private$lsts <- structure(list(), names = character())
  private$async <- async
  invisible(self)
}

ee_listen_on <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  private$lsts[[event]] <-
    c(private$lsts[[event]], list(list(cb = callback, once = FALSE)))
  invisible(self)
}

ee_listen_off <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  for (idx in seq_along(private$lsts[[event]])) {
    if (identical(private$lsts[[event]][[idx]]$cb, callback)) {
      private$lsts[[event]] <- private$lsts[[event]][-idx]
      break
    }
  }
  invisible(self)
}

ee_listen_once <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  private$lsts[[event]] <-
    c(private$lsts[[event]], list(list(cb = callback, once = TRUE)))
  invisible(self)
}

ee_emit <- function(self, private, event, ...) {
  assert_that(is_string(event))
  list(...)
  tocall <- private$lsts[[event]]
  once <- vlapply(tocall, "[[", "once")
  if (any(once)) private$lsts[[event]] <- tocall[!once]

  ## a for loop is not good here, because it does not create
  ## a closure for lst
  lapply(tocall, function(lst) {
    lst
    if (private$async) {
      get_default_event_loop()$add_next_tick(
        function() lst$cb(...),
        private$error_callback,
        data = list(error_info = list(event = event))
      )
    } else {
      call_with_callback(
        function() lst$cb(...),
        private$error_callback,
        info = list(event = event)
      )
    }
  })

  invisible(self)
}

ee_get_event_names <- function(self, private) {
  private$cleanup_events()
  names(private$lsts)
}

ee_get_listener_count <- function(self, private, event) {
  assert_that(is_string(event))
  length(private$lsts[[event]])
}

ee_remove_all_listeners <- function(self, private, event) {
  assert_that(is_string(event))
  private$lsts[[event]] <- NULL
  invisible(self)
}

ee__cleanup_events <- function(self, private) {
  len <- viapply(private$lsts, length)
  private$lsts <- private$lsts[len > 0]
}

ee__error_callback <- function(self, private, err, res) {
  if (is.null(err)) return()
  tocall <- private$lsts[["error"]]
  once <- vlapply(tocall, "[[", "once")
  if (any(once)) private$lsts[["error"]] <- tocall[!once]

  if (length(tocall)) {
    for (lst in tocall) lst$cb(err)
  } else {
    stop(err)
  }
}

#' Do every or some elements of a list satisfy an asynchronous predicate?
#'
#' @param .x A list or atomic vector.
#' @param .p An asynchronous predicate function.
#' @param ... Additional arguments to the predicate function.
#' @return A deferred value for the result.
#'
#' @family async iterators
#' @noRd
#' @examples
#' # Check if all numbers are odd
#' # Note the use of force() here. Otherwise x will be evaluated later,
#' # and by then its value might change.
#' is_odd <- async(function(x) {
#'   force(x)
#'   delay(1/1000)$then(function() as.logical(x %% 2))
#' })
#' synchronise(async_every(c(1,3,5,7,10,11), is_odd))
#' synchronise(async_every(c(1,3,5,7,11), is_odd))

async_every <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE

  deferred$new(
    type = "async_every",
    call = sys.call(),
    parents = defs,
    action = function(resolve) if (nx == 0) resolve(TRUE),
    parent_resolve = function(value, resolve) {
      if (!done && !isTRUE(value)) {
        done <<- TRUE
        resolve(FALSE)
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(TRUE)
      }
    }
  )
}

async_every <- mark_as_async(async_every)

#' Keep or drop elements using an asyncronous predicate function
#'
#' `async_filter` keep the elements for which `.p` is true. (Tested
#' via `isTRUE()`. `async_reject` is the opposite, it drops them.
#'
#' @param .x A list or atomic vector.
#' @param .p An asynchronous predicate function.
#' @param ... Additional arguments to the predicate function.
#' @return A deferred value for the result.
#'
#' @family async iterators
#' @noRd
#' @examples
#' \donttest{
#' ## Filter out non-working URLs
#' afun <- async(function(urls) {
#'   test_url <- async_sequence(
#'      http_head, function(x) identical(x$status_code, 200L))
#'   async_filter(urls, test_url)
#' })
#' urls <- c("https://eu.httpbin.org/get",
#'           "https://eu.httpbin.org/status/404")
#' synchronise(afun(urls))
#' }

async_filter <- function(.x, .p, ...) {
  when_all(.list = lapply(.x, async(.p), ...))$then(
    function(res) .x[vlapply(res, isTRUE)]
  )
}

async_filter <- mark_as_async(async_filter)

#' @rdname async_filter
#' @noRd

async_reject <- function(.x, .p, ...) {
  when_all(.list = lapply(.x, async(.p), ...))$then(
    function(res) .x[!vlapply(res, isTRUE)]
  )
}

async_reject <- mark_as_async(async_reject)
#' HTTP event emitter for server-sent events
#'
#' Server-sent events are a technique to stream events from a web server
#' to a client, through an open HTTP connection.
#'
#' This class implements an event emitter on an async HTTP query created
#' with [http_get()] and friends, that fires an `"event"` event when the
#' server sends an event. An `"end"` event is emitted when the server
#' closes the connection.
#'
#' An event is a named character vector, the names are the keys of the
#' events.
#'
#' Example using our built-in toy web app:
#' ```r
#' http <- webfakes::new_app_process(async:::sseapp())
#' stream_events <- function() {
#'   query <- http_get(http$url("/sse"))
#'   sse <- sse_events$new(query)
#'   sse$
#'     listen_on("event", function(event) {
#'       writeLines("Got an event:")
#'       print(event)
#'     })$
#'     listen_on("end", function() {
#'       writeLines("Done.")
#'     })
#'   query
#' }
#'
#' response <- synchronise(stream_events())
#' ```
#'
#'
#' @noRd

sse_events <- R6Class(
  "sse_events",
  inherit = event_emitter,
  public = list(
    initialize = function(http_handle) {
      super$initialize(async = FALSE)
      http_handle$event_emitter$listen_on("data", function(bytes) {
        private$data <- c(private$data, bytes)
        private$emit_events()
      })
      http_handle$event_emitter$listen_on("end", function() {
        self$emit("end")
      })
    }
  ),

  private = list(
    data = NULL,
    sep = as.raw(c(0xaL, 0xaL)),
    emit_events = function() {
      evs <- chunk_sse_events(private$data, private$sep)
      private$data <- evs$rest
      for (ev in evs$events) {
        self$emit("event", ev)
      }
    }
  )
)

chunk_sse_events <- function(data, sep = NULL) {
  # skip leading \n
  no <- 0L
  while (no <= length(data) && data[no + 1] == 0x0a) {
    no <- no + 1L
  }
  if (no > 0) {
    data <- data[(no + 1L):length(data)]
  }
  sep <- sep %||% as.raw(c(0xaL, 0xaL))
  mtch <- grepRaw(sep, data, fixed = TRUE, all = TRUE)
  # shortcut for no events
  if (length(mtch) == 0) {
    return(list(events = list(), rest = data))
  }

  events <- vector("list", length(mtch))
  for (p in seq_along(mtch)) {
    from <- if (p == 1) 1L else mtch[p - 1] + 2L
    to <- mtch[p] - 1L
    events[[p]] <- parse_sse_event(data[from:to])
  }
  events <- drop_nulls(events)

  restfrom <- mtch[length(mtch)] + 2L
  rest <- if (restfrom <= length(data)) {
    data[restfrom:length(data)]
  } else {
    raw()
  }
  list(events = events, rest = rest)
}

parse_sse_event <- function(data) {
  txt <- rawToChar(data)
  Encoding(txt) <- "UTF-8"
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- lines[lines != ""]
  if (length(lines) == 0) {
    return(NULL)
  }
  keys <- sub(":.*$", "", lines)
  vals <- sub("^[^:]*:[ ]*", "", lines)
  structure(vals, names = keys)
}

drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

sseapp <- function() {
  app <- webfakes::new_app()
  app$get("/sse", function(req, res) {
    `%||%` <- function(l, r) if (is.null(l)) r else l
    if (is.null(res$locals$sse)) {
      duration <- as.double(req$query$duration %||% 2)
      delay <- as.double(req$query$delay %||% 0)
      numevents <- as.integer(req$query$numevents %||% 5)
      pause <- max(duration / numevents, 0.01)
      res$locals$sse <- list(
        sent = 0,
        numevents = numevents,
        pause = pause
      )

      res$set_header("cache-control", "no-cache")$set_header(
        "content-type",
        "text/event-stream"
      )$set_header("access-control-allow-origin", "*")$set_header(
        "connection",
        "keep-alive"
      )$set_status(200)

      if (delay > 0) {
        return(res$delay(delay))
      }
    }

    msg <- paste0(
      "event: ",
      res$locals$sse$sent + 1L,
      "\n",
      "message: live long and prosper\n\n"
    )
    res$locals$sse$sent <- res$locals$sse$sent + 1L
    res$write(msg)

    if (res$locals$sse$sent == res$locals$sse$numevents) {
      res$send("")
    } else {
      res$delay(res$locals$sse$pause)
    }
  })
}

#' Asynchronous HTTP GET request
#'
#' Start an HTTP GET request in the background, and report its completion
#' via a deferred.
#'
#' @section HTTP event emitters:
#' An async HTTP deferred object is also an event emitter, see
#' [event_emitter]. Use `$event_emitter` to access the event emitter API,
#' and call `$event_emitter$listen_on()` etc. to listen on HTTP events,
#' etc.
#'
#' * `"data"` is emitted when we receive data from the server, the data is
#'   passed on to the listeners as a raw vector. Note that zero-length
#'   raw vectors might also happen.
#' * `"end"` is emitted at the end of the HTTP data stream, without
#'   additional arguments (Also on error.)
#'
#' Here is an example, that uses the web server from the webfakes
#' package:
#' ```r
#' http <- webfakes::new_app_process(webfakes::httpbin_app())
#' stream_http <- function() {
#'   query <- http_get(http$url("/drip?duration=3&numbytes=10"))
#'   query$event_emitter$
#'     listen_on("data", function(bytes) {
#'       writeLines(paste("Got", length(bytes), "byte(s):"))
#'       print(bytes)
#'     })$
#'     listen_on("end", function() {
#'       writeLines("Done.")
#'     })
#'   query
#' }
#'
#' response <- synchronise(stream_http())
#' ```
#'
#' @param url URL to connect to.
#' @param headers HTTP headers to send.
#' @param file If not `NULL`, it must be a string, specifying a file.
#'   The body of the response is written to this file.
#' @param options Options to set on the handle. Passed to
#'   [curl::handle_setopt()].
#' @param on_progress Progress handler function. It is only used if the
#'   response body is written to a file. See details below.
#' @return Deferred object.
#'
#' @section Progress bars:
#'
#' `http_get` can report on the progress of the download, via the
#' `on_progress` argument. This is called with a list, with entries:
#' * `url`: the specified url to download
#' * `handle`: the curl handle of the request. This can be queried using
#'   [curl::handle_data()] to get the response status_code, the final
#'   URL (after redirections), timings, etc.
#' * `file`: the `file` argument.
#' * `total`: total bytes of the response. If this is unknown, it is set
#'    to zero.
#' * `current`: already received bytes of the response.
#'
#' @family asyncronous HTTP calls
#' @noRd
#' @examples
#' \donttest{
#' afun <- async(function() {
#'   http_get("https://eu.httpbin.org/status/200")$
#'     then(function(x) x$status_code)
#' })
#' synchronise(afun())
#' }

http_get <- function(
  url,
  headers = character(),
  file = NULL,
  options = list(),
  on_progress = NULL
) {
  url
  headers
  file
  options
  on_progress
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- curl::new_handle(url = url)
      curl::handle_setheaders(handle, .list = headers)

      if (!is.null(on_progress)) {
        options$noprogress <- FALSE
        fun <- options$progressfunction <- function(down, up) {
          on_progress(list(
            url = url,
            handle = handle,
            file = file,
            total = down[[1]],
            current = down[[2]]
          ))
          TRUE
        }
        ## This is a workaround for curl not PROTECT-ing the progress
        ## callback function
        reg.finalizer(handle, function(...) fun, onexit = TRUE)
      }

      curl::handle_setopt(handle, .list = options)
      list(handle = handle, options = options)
    },
    file
  )
}

http_get <- mark_as_async(http_get)

#' Asynchronous HTTP HEAD request
#'
#' An async HTTP deferred object is also an event emitter, see
#' [http_get()] for details, and also [event_emitter].
#'
#' @inheritParams http_get
#' @return Deferred object.
#'
#' @family asyncronous HTTP calls
#' @noRd
#' @examples
#' \donttest{
#' afun <- async(function() {
#'   dx <- http_head("https://eu.httpbin.org/status/200")$
#'     then(function(x) x$status_code)
#' })
#' synchronise(afun())
#'
#' # Check a list of URLs in parallel
#' afun <- function(urls) {
#'   when_all(.list = lapply(urls, http_head))$
#'     then(function(x) lapply(x, "[[", "status_code"))
#' }
#' urls <- c("https://google.com", "https://eu.httpbin.org")
#' synchronise(afun(urls))
#' }

http_head <- function(
  url,
  headers = character(),
  file = NULL,
  options = list(),
  on_progress = NULL
) {
  url
  headers
  file
  options
  on_progress
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- curl::new_handle(url = url)
      curl::handle_setheaders(handle, .list = headers)
      curl::handle_setopt(
        handle,
        customrequest = "HEAD",
        nobody = TRUE,
        .list = options
      )
      list(handle = handle, options = options)
    },
    file
  )
}

http_head <- mark_as_async(http_head)

#' Asynchronous HTTP POST request
#'
#' Start an HTTP POST request in the background, and report its completion
#' via a deferred value.
#'
#' An async HTTP deferred object is also an event emitter, see
#' [http_get()] for details, and also [event_emitter].
#'
#' @inheritParams http_get
#' @param data Data to send. Either a raw vector, or a character string
#'   that will be converted to raw with [base::charToRaw]. At most one of
#'   `data`, `data_file` and `data_form` can be non `NULL`.
#' @param data_file Data file to send. At most one of `data`, `data_file`
#'   and `data_form` can be non `NULL`.
#' @param data_form Form data to send. A name list, where each element
#'   is created with either [curl::form_data()] or [curl::form_file()].
#'   At most one of `data`, `data_file` and `data_form` can be non `NULL`.
#' @param on_progress Progress handler function. It is only used if the
#'   response body is written to a file. See details at [http_get()].
#'
#' @noRd
#' @examples
#' json <- jsonlite::toJSON(list(baz = 100, foo = "bar"))
#'
#' do <- function() {
#'   headers <- c("content-type" = "application/json")
#'   http_post("https://eu.httpbin.org/post", data = json, headers = headers)$
#'     then(http_stop_for_status)$
#'     then(function(x) {
#'       jsonlite::fromJSON(rawToChar(x$content))$json
#'     })
#' }
#'
#' synchronise(do())

http_post <- function(
  url,
  data = NULL,
  data_file = NULL,
  data_form = NULL,
  headers = character(),
  file = NULL,
  options = list(),
  on_progress = NULL
) {
  url
  data
  data_file
  data_form
  headers
  file
  options
  on_progress
  if ((!is.null(data) + !is.null(data_file) + !is.null(data_form)) > 1) {
    stop(
      "At most one of `data`, `data_file` and `data_form` ",
      "can be non `NULL`."
    )
  }
  if (!is.null(data_file)) {
    data <- readBin(data_file, "raw", file.size(data_file))
  }
  if (!is.null(data) && !is.raw(data)) data <- charToRaw(data)
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- curl::new_handle(url = url)
      curl::handle_setheaders(handle, .list = headers)
      curl::handle_setopt(
        handle,
        customrequest = "POST",
        postfieldsize = length(data),
        postfields = data,
        .list = options
      )
      if (!is.null(data_form)) {
        curl::handle_setform(handle, .list = data_form)
      }
      list(handle = handle, options = options)
    },
    file
  )
}

http_post <- mark_as_async(http_post)

http_delete <- function(
  url,
  headers = character(),
  file = NULL,
  options = list()
) {
  url
  headers
  options

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- curl::new_handle(url = url)
      curl::handle_setheaders(handle, .list = headers)
      curl::handle_setopt(handle, customrequest = "DELETE", .list = options)
      list(handle = handle, options = options)
    },
    file
  )
}

http_delete <- mark_as_async(http_delete)

#' @importFrom utils modifyList

get_default_curl_options <- function(options) {
  getopt <- function(nm) {
    if (!is.null(v <- options[[nm]])) return(v)
    anm <- paste0("async_http_", nm)
    if (!is.null(v <- getOption(anm))) return(v)
    if (!is.na(v <- Sys.getenv(toupper(anm), NA_character_))) return(v)
  }
  modifyList(
    options,
    drop_nulls(list(
      timeout = as.integer(getopt("timeout") %||% 0),
      connecttimeout = as.integer(getopt("connecttimeout") %||% 300),
      low_speed_time = as.integer(getopt("low_speed_time") %||% 0),
      low_speed_limit = as.integer(getopt("low_speed_limit") %||% 0),
      cainfo = getopt("cainfo")
    ))
  )
}

http_events <- R6Class(
  "http_events",
  inherit = event_emitter,
  public = list(
    listen_on = function(event, callback) {
      private$check(event)
      super$listen_on(event, callback)
    },
    listen_off = function(event, callback) {
      private$check(event)
      super$listen_off(event, callback)
    }
  ),
  private = list(
    check = function(event) {
      stopifnot(event %in% c("data", "end"))
    }
  )
)

make_deferred_http <- function(cb, file) {
  cb
  file
  id <- NULL
  ee <- http_events$new()
  deferred$new(
    type = "http",
    call = sys.call(),
    action = function(resolve, progress) {
      resolve
      progress
      ## This is a temporary hack until we have proper pollables
      ## Then the deferred will have a "work" callback, which will
      ## be able to throw.
      reject <- environment(resolve)$private$reject
      ho <- cb()
      id <<- get_default_event_loop()$add_http(
        ho$handle,
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        progress,
        file,
        data = c(ho$options, list(event_emitter = ee))
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    },
    event_emitter = ee
  )
}

#' Throw R errors for HTTP errors
#'
#' Status codes below 400 are considered successful, others will trigger
#' errors. Note that this is different from the `httr` package, which
#' considers the 3xx status code errors as well.
#'
#' @param resp HTTP response from [http_get()], [http_head()], etc.
#' @return The HTTP response invisibly, if it is considered successful.
#'   Otherwise an error is thrown.
#'
#' @noRd
#' @examples
#' \donttest{
#' afun <- async(function() {
#'   http_get("https://eu.httpbin.org/status/404")$
#'     then(http_stop_for_status)
#' })
#'
#' tryCatch(synchronise(afun()), error = function(e) e)
#' }

http_stop_for_status <- function(resp) {
  if (!is.integer(resp$status_code)) stop("Not an HTTP response")
  if (resp$status_code < 400) return(invisible(resp))
  stop(http_error(resp))
}

http_error <- function(resp, call = sys.call(-1)) {
  status <- resp$status_code
  reason <- http_status(status)$reason
  message <- sprintf("%s (HTTP %d).", reason, status)
  status_type <- (status %/% 100) * 100
  if (
    length(resp[["content"]]) == 0 &&
      !is.null(resp$file) &&
      file.exists(resp$file)
  ) {
    tryCatch(
      {
        n <- file.info(resp$file, extra_cols = FALSE)$size
        resp$content <- readBin(resp$file, what = raw(), n = n)
      },
      error = identity
    )
  }
  http_class <- paste0("async_http_", unique(c(status, status_type, "error")))
  structure(
    list(message = message, call = call, response = resp),
    class = c(http_class, "error", "condition")
  )
}

http_status <- function(status) {
  status_desc <- http_statuses[as.character(status)]
  if (is.na(status_desc)) {
    stop("Unknown http status code: ", status, call. = FALSE)
  }

  status_types <- c(
    "Information",
    "Success",
    "Redirection",
    "Client error",
    "Server error"
  )
  status_type <- status_types[[status %/% 100]]

  # create the final information message
  message <- paste(status_type, ": (", status, ") ", status_desc, sep = "")

  list(
    category = status_type,
    reason = status_desc,
    message = message
  )
}

http_statuses <- c(
  "100" = "Continue",
  "101" = "Switching Protocols",
  "102" = "Processing (WebDAV; RFC 2518)",
  "200" = "OK",
  "201" = "Created",
  "202" = "Accepted",
  "203" = "Non-Authoritative Information",
  "204" = "No Content",
  "205" = "Reset Content",
  "206" = "Partial Content",
  "207" = "Multi-Status (WebDAV; RFC 4918)",
  "208" = "Already Reported (WebDAV; RFC 5842)",
  "226" = "IM Used (RFC 3229)",
  "300" = "Multiple Choices",
  "301" = "Moved Permanently",
  "302" = "Found",
  "303" = "See Other",
  "304" = "Not Modified",
  "305" = "Use Proxy",
  "306" = "Switch Proxy",
  "307" = "Temporary Redirect",
  "308" = "Permanent Redirect (experimental Internet-Draft)",
  "400" = "Bad Request",
  "401" = "Unauthorized",
  "402" = "Payment Required",
  "403" = "Forbidden",
  "404" = "Not Found",
  "405" = "Method Not Allowed",
  "406" = "Not Acceptable",
  "407" = "Proxy Authentication Required",
  "408" = "Request Timeout",
  "409" = "Conflict",
  "410" = "Gone",
  "411" = "Length Required",
  "412" = "Precondition Failed",
  "413" = "Request Entity Too Large",
  "414" = "Request-URI Too Long",
  "415" = "Unsupported Media Type",
  "416" = "Requested Range Not Satisfiable",
  "417" = "Expectation Failed",
  "418" = "I'm a teapot (RFC 2324)",
  "420" = "Enhance Your Calm (Twitter)",
  "422" = "Unprocessable Entity (WebDAV; RFC 4918)",
  "423" = "Locked (WebDAV; RFC 4918)",
  "424" = "Failed Dependency (WebDAV; RFC 4918)",
  "424" = "Method Failure (WebDAV)",
  "425" = "Unordered Collection (Internet draft)",
  "426" = "Upgrade Required (RFC 2817)",
  "428" = "Precondition Required (RFC 6585)",
  "429" = "Too Many Requests (RFC 6585)",
  "431" = "Request Header Fields Too Large (RFC 6585)",
  "444" = "No Response (Nginx)",
  "449" = "Retry With (Microsoft)",
  "450" = "Blocked by Windows Parental Controls (Microsoft)",
  "451" = "Unavailable For Legal Reasons (Internet draft)",
  "499" = "Client Closed Request (Nginx)",
  "500" = "Internal Server Error",
  "501" = "Not Implemented",
  "502" = "Bad Gateway",
  "503" = "Service Unavailable",
  "504" = "Gateway Timeout",
  "505" = "HTTP Version Not Supported",
  "506" = "Variant Also Negotiates (RFC 2295)",
  "507" = "Insufficient Storage (WebDAV; RFC 4918)",
  "508" = "Loop Detected (WebDAV; RFC 5842)",
  "509" = "Bandwidth Limit Exceeded (Apache bw/limited extension)",
  "510" = "Not Extended (RFC 2774)",
  "511" = "Network Authentication Required (RFC 6585)",
  "598" = "Network read timeout error (Unknown)",
  "599" = "Network connect timeout error (Unknown)"
)

#' Set curl HTTP options in an event loop
#'
#' The event loop must be already running. In other words, you can only
#' call this function from async functions.
#'
#' The default values are set when the first deferred HTTP operation of the
#' event loop is created, and they are taken from the `async_http_total_con`,
#' `async_http_host_con` and `async_http_multiplex` options.
#'
#' @param total_con,host_con,multiplex They are passed to
#'   [curl::multi_set()]. If an argument is `NULL` (the default) then it is
#'   ignored.
#' @noRd
#' @family asyncronous HTTP calls

http_setopt <- function(total_con = NULL, host_con = NULL, multiplex = NULL) {
  get_default_event_loop()$http_setopt(total_con, host_con, multiplex)
  invisible()
}

#' Apply an asynchronous function to each element of a vector
#'
#' @param .x A list or atomic vector.
#' @param .f Asynchronous function to apply.
#' @param ... Additional arguments to `.f`.
#' @param .args More additional arguments to `.f`.
#' @param .limit Number of elements to process simulateneously.
#' @return Deferred value that is resolved after all deferred values
#'   from the application of `.f` are resolved.
#'
#' @family async iterators
#' @noRd
#' @examples
#' synchronise(async_map(
#'   seq(10, 100, by = 10) / 100,
#'   function(wait) delay(wait)$then(function() "OK")
#' ))

async_map <- function(.x, .f, ..., .args = list(), .limit = Inf) {
  if (.limit < length(.x)) {
    async_map_limit(.x, .f, ..., .args = .args, .limit = .limit)
  } else {
    defs <- do.call(lapply, c(list(.x, async(.f), ...), .args))
    when_all(.list = defs)
  }
}

async_map <- mark_as_async(async_map)

async_map_limit <- function(.x, .f, ..., .args = list(), .limit = Inf) {
  len <- length(.x)
  nx <- len
  .f <- async(.f)
  args <- c(list(...), .args)

  nextone <- .limit + 1L
  firsts <- lapply_args(.x[seq_len(.limit)], .f, .args = args)

  result <- structure(
    vector(mode = "list", length = len),
    names = names(.x)
  )

  self <- deferred$new(
    type = "async_map (limit)",
    call = sys.call(),
    action = function(resolve) {
      self
      nx
      firsts
      lapply(seq_along(firsts), function(idx) {
        firsts[[idx]]$then(function(val) list(idx, val))$then(self)
      })
      if (nx == 0) resolve(result)
    },
    parent_resolve = function(value, resolve) {
      self
      nx
      nextone
      result
      .f
      nx <<- nx - 1L
      result[value[[1]]] <<- value[2]
      if (nx == 0) {
        resolve(result)
      } else if (nextone <= len) {
        idx <- nextone
        dx <- do.call(".f", c(list(.x[[nextone]]), args))
        dx$then(function(val) list(idx, val))$then(self)
        nextone <<- nextone + 1L
      }
    }
  )

  self
}

## nocov start

.onLoad <- function(libname, pkgname) {
  if (
    Sys.getenv("DEBUGME") != "" &&
      requireNamespace("debugme", quietly = TRUE)
  ) {
    debugme::debugme()
  }
}

## nocov end

#' Asynchronous external process execution
#'
#' Start an external process in the background, and report its completion
#' via a deferred.
#'
#' @inheritParams processx::run
#' @param error_on_status Whether to reject the referred value if the
#'    program exits with a non-zero status.
#' @return Deferred object.
#'
#' @family asynchronous external processes
#' @noRd
#' @examples
#' \dontrun{
#' afun <- function() {
#'   run_process("ls", "-l")$
#'     then(function(x) strsplit(x$stdout, "\r?\n")[[1]])
#' }
#' synchronise(afun())
#' }

run_process <- function(
  command = NULL,
  args = character(),
  error_on_status = TRUE,
  wd = NULL,
  env = NULL,
  windows_verbatim_args = FALSE,
  windows_hide_window = FALSE,
  encoding = "",
  ...
) {
  command
  args
  error_on_status
  wd
  env
  windows_verbatim_args
  windows_hide_window
  encoding
  list(...)

  id <- NULL

  deferred$new(
    type = "process",
    call = sys.call(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      stdout <- tempfile()
      stderr <- tempfile()
      px <- processx::process$new(
        command,
        args = args,
        stdout = stdout,
        stderr = stderr,
        poll_connection = TRUE,
        env = env,
        cleanup = TRUE,
        cleanup_tree = TRUE,
        wd = wd,
        encoding = encoding,
        ...
      )
      pipe <- px$get_poll_connection()
      id <<- get_default_event_loop()$add_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(
          process = px,
          stdout = stdout,
          stderr = stderr,
          error_on_status = error_on_status,
          encoding = encoding
        )
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

run_process <- mark_as_async(run_process)

#' Asynchronous call to an R function, in a background R process
#'
#' Start a background R process and evaluate a function call in it.
#' It uses [callr::r_process] internally.
#'
#' @inheritParams callr::r_bg
#' @noRd
#'
#' @examples
#' \dontrun{
#' afun <- function() {
#'   run_r_process(function() Sys.getpid())
#' }
#' synchronise(afun())
#' }

run_r_process <- function(
  func,
  args = list(),
  libpath = .libPaths(),
  repos = c(getOption("repos"), c(CRAN = "https://cloud.r-project.org")),
  cmdargs = c("--no-site-file", "--slave", "--no-save", "--no-restore"),
  system_profile = FALSE,
  user_profile = FALSE,
  env = callr::rcmd_safe_env()
) {
  func
  args
  libpath
  repos
  cmdargs
  system_profile
  user_profile
  env

  id <- NULL

  deferred$new(
    type = "r-process",
    call = sys.calls(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      stdout <- tempfile()
      stderr <- tempfile()
      opts <- callr::r_process_options(
        func = func,
        args = args,
        libpath = libpath,
        repos = repos,
        cmdargs = cmdargs,
        system_profile = system_profile,
        user_profile = user_profile,
        env = env,
        stdout = stdout,
        stderr = stderr,
        extra = list(cleanup_tree = TRUE)
      )

      rx <- callr::r_process$new(opts)
      pipe <- rx$get_poll_connection()
      id <<- get_default_event_loop()$add_r_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(
          process = rx,
          stdout = stdout,
          stderr = stderr,
          error_on_status = TRUE,
          encoding = ""
        )
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

run_r_process <- mark_as_async(run_r_process)

#' A deferred value that resolves when the specified number of deferred
#' values resolve, or is rejected when one of them is rejected
#'
#' These functions are similar to [when_some()] and [when_any()], but they
#' do not ignore errors. If a deferred is rejected, then `async_race_some()` and
#' `async_race()` are rejected as well.
#'
#' `async_race()` is a special case of `count = `: it resolves or is rejected
#' as soon as one deferred resolves or is rejected.
#'
#' async has auto-cancellation, so if the required number of deferred values
#' are resolved, or any deferred value is rejected, the rest are cancelled.
#'
#' @param count Number of deferred values that need to resolve.
#' @param ... Deferred values.
#' @param .list More deferred values.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @noRd

async_race_some <- function(count, ..., .list = list()) {
  when_some_internal(count, ..., .list = .list, .race = TRUE)
}

async_race_some <- mark_as_async(async_race_some)

#' @noRd
#' @rdname async_race_some

async_race <- function(..., .list = list()) {
  when_some_internal(1L, ..., .list = .list, .race = TRUE)$then(
    function(x) x[[1]]
  )
}

async_race <- mark_as_async(async_race)

#' Make an asynchronous function that always succeeds
#'
#' This is sometimes useful, if the function is applied to entries in
#' a vector or list.
#'
#' @param task Function to transform.
#' @return Async function returning a deferred value that is never
#'   rejected. Instead its value is a list with entries `error` and
#'   `result`. If the original deferred was resolved, then `error` is
#'   `NULL`. If the original deferred was rejected, then `result` is
#'   `NULL`.
#'
#' @family async control flow
#' @noRd
#' @examples
#'  badfun <- async(function() stop("oh no!"))
#'  safefun <- async_reflect(badfun)
#'  synchronise(when_all(safefun(), "good"))

async_reflect <- function(task) {
  task <- async(task)
  function(...) {
    task(...)$then(function(value) list(error = NULL, result = value))$catch(
      error = function(reason) list(error = reason, result = NULL)
    )
  }
}

async_reflect <- mark_as_async(async_reflect)

#' Replicate an async function a number of times
#'
#' Similar to [base::replicate()], with some differences:
#' * it takes an async function, instead of an expression, and
#' * it always returns a list.
#'
#' @param n Number of replications.
#' @param task Async function to call.
#' @param ... Additional arguments to `task`.
#' @param .limit Number of concurrent async processes to create.
#' @return Resolves to a list of the results of the `n` `task` calls.
#'
#' @noRd
#' @examples
#' \donttest{
#' ## perform an HTTP request three times, and list the reponse times
#' do <- function() {
#'   async_replicate(3,
#'     function() http_get("https://eu.httpbin.org")$then(function(x) x$times))
#' }
#' synchronise(do())
#' }

async_replicate <- function(n, task, ..., .limit = Inf) {
  assert_that(
    is_count(n),
    .limit == Inf || is_count(.limit),
    .limit >= 1L
  )

  force(list(...))
  task <- async(task)

  if (n == 0) {
    async_constant(list())
  } else if (n <= .limit) {
    async_replicate_nolimit(n, task, ...)
  } else {
    async_replicate_limit(n, task, ..., .limit = .limit)
  }
}

async_replicate_nolimit <- function(n, task, ...) {
  defs <- lapply(seq_len(n), function(i) task(...))
  when_all(.list = defs)
}

async_replicate_limit <- function(n, task, ..., .limit = .limit) {
  n
  .limit

  defs <- nextone <- result <- NULL

  self <- deferred$new(
    type = "async_replicate",
    call = sys.call(),
    action = function(resolve) {
      defs <<- lapply(seq_len(n), function(i) task(...))
      result <<- vector(n, mode = "list")
      lapply(seq_len(.limit), function(idx) {
        defs[[idx]]$then(function(val) list(idx, val))$then(self)
      })
      nextone <<- .limit + 1L
    },
    parent_resolve = function(value, resolve) {
      result[value[[1]]] <<- value[2]
      if (nextone > n) {
        resolve(result)
      } else {
        idx <- nextone
        defs[[nextone]]$then(function(val) list(idx, val))$then(self)
        nextone <<- nextone + 1L
      }
    }
  )

  self
}

#' Retry an asynchronous function a number of times
#'
#' Keeps trying until the function's deferred value resolves without
#' error, or `times` tries have been performed.
#'
#' @param task An asynchronous function.
#' @param times Number of tries.
#' @param ... Arguments to pass to `task`.
#' @return Deferred value for the operation with retries.
#'
#' @family async control flow
#' @noRd
#' @examples
#' \donttest{
#' ## Try a download at most 5 times
#' afun <- async(function() {
#'   async_retry(
#'     function() http_get("https://eu.httpbin.org"),
#'     times = 5
#'   )$then(function(x) x$status_code)
#' })
#'
#' synchronise(afun())
#' }

async_retry <- function(task, times, ...) {
  task <- async(task)
  times <- times
  force(list(...))

  self <- deferred$new(
    type = "retry",
    call = sys.call(),
    parents = list(task(...)),
    parent_reject = function(value, resolve) {
      times <<- times - 1L
      if (times > 0) {
        task(...)$then(self)
      } else {
        stop(value)
      }
    }
  )
}

async_retry <- mark_as_async(async_retry)

#' Make an asynchronous funcion retryable
#'
#' @param task An asynchronous function.
#' @param times Number of tries.
#' @return Asynchronous retryable function.
#'
#' @family async control flow
#' @noRd
#' @examples
#' \donttest{
#' ## Create a downloader that retries five times
#' http_get_5 <- async_retryable(http_get, times = 5)
#' ret <- synchronise(
#'   http_get_5("https://eu.httpbin.org/get?q=1")$
#'     then(function(x) rawToChar(x$content))
#' )
#' cat(ret)
#' }

async_retryable <- function(task, times) {
  task <- async(task)
  force(times)
  function(...) {
    async_retry(task, times, ...)
  }
}

#' Compose asynchronous functions
#'
#' This is equivalent to using the `$then()` method of a deferred, but
#' it is easier to use programmatically.
#'
#' @param ... Asynchronous functions to compose.
#' @param .list Mose asynchronous functions to compose.
#' @return Asynchronous function, the composition of all input functions.
#'   They are performed left to right, the ones in `.list` are the last
#'   ones.
#'
#' @family async control flow
#' @noRd
#' @examples
#' \donttest{
#' check_url <- async_sequence(
#'   http_head, function(x) identical(x$status_code, 200L))
#' synchronise(check_url("https://eu.httpbin.org/status/404"))
#' synchronise(check_url("https://eu.httpbin.org/status/200"))
#' }

async_sequence <- function(..., .list = NULL) {
  funcs <- c(list(...), .list)
  if (length(funcs) == 0) stop("Function list empty in `async_sequence`")

  function(...) {
    dx <- async(funcs[[1]])(...)
    for (i in seq_along(funcs)[-1]) dx <- dx$then(funcs[[i]])
    dx
  }
}

async_sequence <- mark_as_async(async_sequence)

#' @noRd
#' @rdname async_every

async_some <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE

  deferred$new(
    type = "async_some",
    call = sys.call(),
    parents = defs,
    action = function(resolve) if (nx == 0) resolve(FALSE),
    parent_resolve = function(value, resolve) {
      if (!done && isTRUE(value)) {
        done <<- TRUE
        resolve(TRUE)
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(FALSE)
      }
    }
  )
}

async_some <- mark_as_async(async_some)

#' Synchronously wrap asynchronous code
#'
#' Evaluate an expression in an async phase. It creates an event loop,
#' then evaluates the supplied expression. If its result is a deferred
#' value, it keeps running the event loop, until the deferred value is
#' resolved, and returns its resolved value.
#'
#' If an error is not handled in the async phase, `synchronise()` will
#' re-throw that error.
#'
#' `synchronise()` cancels all async processes on interrupt or external
#' error.
#'
#' @param expr Async function call expression. If it does not evaluate
#' to a deferred value, then it is just returned.
#'
#' @noRd
#' @examples
#' \donttest{
#' http_status <- function(url, ...) {
#'   http_get(url, ...)$
#'     then(function(x) x$status_code)
#' }
#'
#' synchronise(http_status("https://eu.httpbin.org/status/418"))
#' }

synchronise <- function(expr) {
  new_el <- push_event_loop()
  on.exit(
    {
      new_el$cancel_all()
      pop_event_loop()
    },
    add = TRUE
  )

  ## Mark this frame as a synchronization point, for debugging
  `__async_synchronise_frame__` <- TRUE

  ## This is to allow `expr` to contain `async_list()` etc
  ## calls that look for the top promise. Without this there
  ## is no top promise. This is a temporary top promise that
  ## is never started.
  res <- async_constant(NULL)

  res <- expr

  if (!is_deferred(res)) return(res)

  ## We need an extra final promise that cannot be replaced,
  ## so priv stays the same.
  res <- res$then(function(x) x)

  priv <- get_private(res)
  if (!identical(priv$event_loop, new_el)) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error"
    )
    stop(err)
  }

  priv$null()
  priv$run_action()

  if (isTRUE(getOption("async_debug"))) start_browser()
  while (priv$state == "pending") new_el$run("once")

  if (priv$state == "fulfilled") priv$value else stop(priv$value)
}

start_browser <- function() {
  async_debug_shortcuts()
  on.exit(async_debug_remove_shortcuts(), add = TRUE)
  cat("This is a standard `browser()` call, but you can also use the\n")
  cat("following extra commands:\n")
  cat("- .an / async_next(): next event loop iteration.\n")
  cat(
    "- .as / async_step(): next event loop, debug next action or parent callback.\n"
  )
  cat("- .asb / async_step_back(): stop debugging of callbacks.\n")
  cat("- .al / async_list(): deferred values in the current async phase.\n")
  cat("- .at / async_tree(): DAG of the deferred values.\n")
  cat("- .aw / async_where(): print call stack, mark async callback.\n")
  cat("- async_wait_for(): run until deferred is resolved.\n")
  cat("- async_debug(): debug action and/or parent callbacks of deferred.\n")
  cat("\n")
  browser(skipCalls = 1)
}

#' Run event loop to completion
#'
#' Creates a new event loop, evaluates `expr` in it, and then runs the
#' event loop to completion. It stops when the event loop does not have
#' any tasks.
#'
#' The expression typically creates event loop tasks. It should not create
#' deferred values, though, because those will never be evaluated.
#'
#' Unhandled errors propagate to the `run_event_loop()` call, which fails.
#'
#' In case of an (unhandled) error, all event loop tasks will be cancelled.
#'
#' @param expr Expression to run after creating a new event loop.
#' @return `NULL`, always. If the event loop is to return some value,
#' you can use lexical scoping, see the example below.
#'
#' @noRd
#' @examples
#' counter <- 0L
#' do <- function() {
#'   callback <- function() {
#'     counter <<- counter + 1L
#'     if (runif(1) < 1/10) t$cancel()
#'   }
#'   t <- async_timer$new(1/1000, callback)
#' }
#' run_event_loop(do())
#' counter

run_event_loop <- function(expr) {
  new_el <- push_event_loop()
  on.exit(
    {
      new_el$cancel_all()
      pop_event_loop()
    },
    add = TRUE
  )

  ## Mark this frame as a synchronization point, for debugging
  `__async_synchronise_frame__` <- TRUE

  expr
  new_el$run()

  invisible()
}

distill_error <- function(err) {
  if (is.null(err$aframe)) return(err)
  err$aframe <- list(
    frame = err$aframe$frame,
    deferred = err$aframe$data[[1]],
    type = err$aframe$data[[2]],
    call = get_private(err$aframe$data[[3]])$mycall
  )
  err
}

# nocov start
#' @noRd

print.async_rejected <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

# nocov end

#' @noRd

format.async_rejected <- function(x, ...) {
  x <- distill_error(x)
  src <- get_source_position(x$aframe$call)
  paste0(
    "<async error: ",
    x$message,
    "\n",
    " in *",
    x$aframe$type,
    "* callback of `",
    expr_name(x$aframe$call %||% ""),
    "` at ",
    src$filename,
    ":",
    src$position,
    ">"
  )
}

#' @noRd

summary.async_rejected <- function(object, ...) {
  x <- distill_error(object)
  fmt_out <- format(object, ...)
  stack <- async_where(
    calls = x$calls,
    parents = x$parents,
    frm = list(x$aframe)
  )
  stack_out <- format(stack)
  structure(
    paste0(fmt_out, "\n\n", stack_out),
    class = "async_rejected_summary"
  )
}

# nocov start

#' @noRd

print.async_rejected_summary <- function(x, ...) {
  cat(x)
  invisible(x)
}

# nocov end

#' Asynchronous function call with a timeout
#'
#' If the deferred value is not resolved before the timeout expires,
#' `async_timeout()` throws an `async_timeout` error.
#'
#' @param task Asynchronous function.
#' @param timeout Timeout as a `difftime` object, or number of seconds.
#' @param ... Additional arguments to `task`.
#' @return A deferred value. An `async_timeout` error is thrown if it is
#'   not resolved within the specified timeout.
#'
#' @family async utilities
#' @noRd
#' @examples
#' ## You can catch the error, asynchronously
#' synchronise(
#'   async_timeout(function() delay(1/10)$then(function() "OK"), 1/1000)$
#'     catch(async_timeout = function(e) "Timed out",
#'           error = function(e) "Other error")
#' )
#'
#' ## Or synchronously
#' tryCatch(
#'   synchronise(
#'     async_timeout(function() delay(1/10)$then(function() "OK"), 1/1000)
#'   ),
#'   async_timeout = function(e) "Timed out. :(",
#'   error = function(e) paste("Other error:", e$message)
#' )

async_timeout <- function(task, timeout, ...) {
  task <- async(task)
  force(timeout)
  list(...)
  done <- FALSE

  self <- deferred$new(
    type = "timeout",
    call = sys.call(),
    action = function(resolve) {
      task(...)$then(function(x) list("ok", x))$then(self)
      delay(timeout)$then(function() list("timeout"))$then(self)
    },
    parent_resolve = function(value, resolve) {
      if (!done) {
        done <<- TRUE
        if (value[[1]] == "ok") {
          resolve(value[[2]])
        } else {
          cnd <- structure(
            list(message = "Aync operation timed out"),
            class = c("async_timeout", "error", "condition")
          )
          stop(cnd)
        }
      }
    }
  )
}

async_timeout <- mark_as_async(async_timeout)

#' Repeated timer
#'
#' The supplied callback function will be called by the event loop
#' every `delay` seconds.
#'
#' @section Usage:
#' ```
#' t <- async_timer$new(delay, callback)
#' t$cancel()
#' ```
#'
#' @section Arguments:
#' * `delay`: Time interval in seconds, the amount of time to delay
#'   to delay the execution. It can be a fraction of a second.
#' * `callback`: Callback function without arguments. It will be called
#'   from the event loop every `delay` seconds.
#'
#' @section Details:
#'
#' An `async_timer` is an `[event_emitter]` object with a `timeout` event.
#' It is possible to add multiple listeners to this event, once the timer
#' is created. Note, however, that removing all listeners does not cancel
#' the timer, `timeout` events will be still emitted as usual.
#' For proper cancellation you'll need to call the `cancel()` method.
#'
#' It is only possible to create `async_timer` objects in an asynchronous
#' context, i.e. within a `synchronise()` or `run_event_loop()` call.
#' A `synchronise()` call finishes as soon as its returned deferred value
#' is resolved (or rejected), even if some timers are still active. The
#' active timers will be automatically cancelled in this case.
#'
#' @section Errors:
#' Errors are handled the same way as for generic event emitters. I.e. to
#' catch errors thrown in the `callback` function, you need to add a
#' listener to the `error` event, see the example below.
#'
#' @section Congestion:
#' `async_timer` is _not_ a real-time timer. In particular, if `callback`
#' does not return in time, before the next timer event, then all future
#' timer events will be delayed. Even if `callback` returns promptly, the
#' event loop might be busy with other events, and then the next timer
#' event is not emitted in time. In general there is no guarantee about
#' the timing of the timer events.
#'
#' @importFrom R6 R6Class
#' @noRd
#' @examples
#' ## Call 10 times a second, cancel with 1/10 probability
#' counter <- 0L
#' do <- function() {
#'   cb <- function() {
#'     cat("called\n")
#'     counter <<- counter + 1L
#'     if (runif(1) < 0.1) t$cancel()
#'   }
#'   t <- async_timer$new(1/10, cb)
#' }
#'
#' run_event_loop(do())
#' counter
#'
#' ## Error handling
#' counter <- 0L
#' do <- function() {
#'   cb <- function() {
#'     cat("called\n")
#'     counter <<- counter + 1L
#'     if (counter == 2L) stop("foobar")
#'     if (counter == 3L) t$cancel()
#'   }
#'   t <- async_timer$new(1/10, cb)
#'   handler <- function(err) {
#'     cat("Got error:", sQuote(conditionMessage(err)), ", handled\n")
#'   }
#'   t$listen_on("error", handler)
#' }
#'
#' run_event_loop(do())
#' counter
#'
#' ## Error handling at the synchonization point
#' counter <- 0L
#' do <- function() {
#'   cb <- function() {
#'     cat("called\n")
#'     counter <<- counter + 1L
#'     if (counter == 2L) stop("foobar")
#'     if (counter == 3L) t$cancel()
#'   }
#'   t <- async_timer$new(1/10, cb)
#' }
#'
#' tryCatch(run_event_loop(do()), error = function(x) x)
#' counter

async_timer <- R6Class(
  "async_timer",
  inherit = event_emitter,
  public = list(
    initialize = function(delay, callback)
      async_timer_init(self, private, super, delay, callback),
    cancel = function() async_timer_cancel(self, private)
  ),

  private = list(
    id = NULL
  )
)

async_timer_init <- function(self, private, super, delay, callback) {
  assert_that(
    is_time_interval(delay),
    is.function(callback) && length(formals(callback)) == 0
  )

  ## event emitter
  super$initialize()

  private$id <- get_default_event_loop()$add_delayed(
    delay,
    function() self$emit("timeout"),
    function(err, res) {
      if (!is.null(err)) self$emit("error", err) # nocov
    },
    rep = TRUE
  )

  self$listen_on("timeout", callback)

  invisible(self)
}

async_timer_cancel <- function(self, private) {
  self
  private
  self$remove_all_listeners("timeout")
  get_default_event_loop()$cancel(private$id)
  invisible(self)
}

#' It runs each task in series but stops whenever any of the functions were
#' successful. If one of the tasks were successful, the callback will be
#' passed the result of the successful task. If all tasks fail, the
#' callback will be passed the error and result (if any) of the final
#' attempt.
#' @param ... Deferred values to run in series.
#' @param .list More deferred values to run, `.list` is easier to use
#'   programmatically.
#' @return Resolves to the result of the first successful deferred.
#'   Otherwise throws an error. The error objects of all failed deferreds
#'   will be in the `errors` member of the error object.
#'
#' @family async control flow
#' @noRd
#' @examples
#' do <- function() {
#'   async_try_each(
#'     async(function() stop("doh"))(),
#'     async(function() "cool")(),
#'     async(function() stop("doh2"))(),
#'     async(function() "cool2")()
#'   )
#' }
#' synchronise(do())

async_try_each <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  wh <- nx <- NULL
  errors <- list()

  self <- deferred$new(
    type = "async_try_each",
    call = sys.call(),
    action = function(resolve) {
      nx <<- length(defs)
      if (nx == 0) resolve(NULL)
      wh <<- 1L
      defs[[wh]]$then(self)
    },
    parent_resolve = function(value, resolve) {
      resolve(value)
    },
    parent_reject = function(value, resolve) {
      errors <<- c(errors, list(value))
      if (wh == nx) {
        err <- structure(
          list(errors = errors, message = "async_try_each failed"),
          class = c("async_rejected", "error", "condition")
        )
        stop(err)
      } else {
        wh <<- wh + 1
        defs[[wh]]$then(self)
      }
    }
  )

  self
}

async_try_each <- mark_as_async(async_try_each)

#' Repeatedly call task until it its test function returns `TRUE`
#'
#' @param test Synchronous test function.
#' @param task Asynchronous function to call repeatedly.
#' @param ... Arguments to pass to `task`.
#' @return Deferred value, that is resolved when the iteration is done.
#'
#' @family async control flow
#' @noRd
#' @examples
#' ## Keep calling until it "returns" a number less than < 0.1
#' calls <- 0
#' number <- Inf
#' synchronise(async_until(
#'   function() number < 0.1,
#'   function() {
#'     calls <<- calls + 1
#'     number <<- runif(1)
#'   }
#' ))
#' calls

async_until <- function(test, task, ...) {
  force(test)
  task <- async(task)

  self <- deferred$new(
    type = "async_until",
    call = sys.call(),
    parents = list(task(...)),
    parent_resolve = function(value, resolve) {
      if (test()) {
        resolve(value)
      } else {
        task(...)$then(self)
      }
    }
  )

  self
}

async_until <- mark_as_async(async_until)

`%||%` <- function(l, r) if (is.null(l)) r else l

vlapply <- function(X, FUN, ..., FUN.VALUE = logical(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

viapply <- function(X, FUN, ..., FUN.VALUE = integer(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

vcapply <- function(X, FUN, ..., FUN.VALUE = character(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

make_error <- function(message, class = "simpleError", call = NULL) {
  class <- c(class, "error", "condition")
  structure(
    list(message = as.character(message), call = call),
    class = class
  )
}

num_args <- function(fun) {
  length(formals(fun))
}

get_private <- function(x) {
  x$.__enclos_env__$private
}

#' Call `func` and then call `callback` with the result
#'
#' `callback` will be called with two arguments, the first one will the
#' error object if `func()` threw an error, or `NULL` otherwise. The second
#' argument is `NULL` on error, and the result of `func()` otherwise.
#'
#' @param func Function to call.
#' @param callback Callback to call with the result of `func()`,
#'   or the error thrown.
#' @param info Extra info to add to the error object. Must be a named list.
#'
#' @keywords internal

call_with_callback <- function(func, callback, info = NULL) {
  recerror <- NULL
  result <- NULL
  tryCatch(
    withCallingHandlers(
      result <- func(),
      error = function(e) {
        recerror <<- e
        recerror$aframe <<- recerror$aframe %||% find_async_data_frame()
        recerror$calls <<- recerror$calls %||% sys.calls()
        if (is.null(recerror[["call"]])) recerror[["call"]] <<- sys.call()
        recerror$parents <<- recerror$parents %||% sys.parents()
        recerror[names(info)] <<- info
        handler <- getOption("async.error")
        if (is.function(handler)) handler()
      }
    ),
    error = identity
  )
  callback(recerror, result)
}

get_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

new_event_loop_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

lapply_args <- function(X, FUN, ..., .args = list()) {
  do.call("lapply", c(list(X = X, FUN = FUN), list(...), .args))
}

drop_nulls <- function(x) {
  x[!vlapply(x, is.null)]
}

#' @importFrom utils getSrcDirectory getSrcFilename getSrcLocation

get_source_position <- function(call) {
  list(
    filename = file.path(
      c(getSrcDirectory(call), "?")[1],
      c(getSrcFilename(call), "?")[1]
    ),
    position = paste0(
      getSrcLocation(call, "line", TRUE) %||% "?",
      ":",
      getSrcLocation(call, "column", TRUE) %||% "?"
    )
  )
}

file_size <- function(...) {
  file.info(..., extra_cols = FALSE)$size
}

read_all <- function(filename, encoding) {
  if (is.null(filename)) return(NULL)
  r <- readBin(filename, what = raw(0), n = file_size(filename))
  s <- rawToChar(r)
  Encoding(s) <- encoding
  s
}

crash <- function() {
  get("attach")(structure(list(), class = "UserDefinedDatabase"))
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

expr_name <- function(expr) {
  if (is.null(expr)) {
    return("NULL")
  }

  if (is.symbol(expr)) {
    return(as.character(expr))
  }

  if (is.call(expr)) {
    cl <- as.list(expr)[[1]]
    if (is.symbol(cl)) {
      return(as.character(cl))
    } else {
      return(paste0(format(cl), collapse = ""))
    }
  }

  if (is.atomic(expr) && length(expr) == 1) {
    return(as.character(expr))
  }

  gsub("\n.*$", "...", as.character(expr))
}

get_uuid <- function() {
  async_env$pid <- async_env$pid %||% Sys.getpid()
  async_env$counter <- async_env$counter %||% 0
  async_env$counter <- async_env$counter + 1L
  paste0(async_env$pid, "-", async_env$counter)
}

#' Deferred value for a set of deferred values
#'
#' Create a deferred value that is resolved when all listed deferred values
#' are resolved. Note that the error of an input deferred value
#' triggers the error `when_all` as well.
#'
#' async has auto-cancellation, so if one deferred value errors, the rest
#' of them will be automatically cancelled.
#'
#' @param ... Deferred values.
#' @param .list More deferred values.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @seealso [when_any()], [when_some()]
#' @noRd
#' @examples
#' \donttest{
#' ## Check that the contents of two URLs are the same
#' afun <- async(function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_all(u1, u2)$
#'     then(function(x) identical(x[[1]]$content, x[[2]]$content))
#' })
#' synchronise(afun())
#' }

when_all <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  nx <- 0L

  self <- deferred$new(
    type = "when_all",
    call = sys.call(),
    action = function(resolve) {
      self
      nx
      defs
      lapply(seq_along(defs), function(idx) {
        idx
        if (is_deferred(defs[[idx]])) {
          nx <<- nx + 1L
          defs[[idx]]$then(function(val) list(idx, val))$then(self)
        }
      })
      if (nx == 0) resolve(defs)
    },
    parent_resolve = function(value, resolve) {
      defs[value[[1]]] <<- value[2]
      nx <<- nx - 1L
      if (nx == 0L) resolve(defs)
    }
  )
}

when_all <- mark_as_async(when_all)

#' Resolve a deferred as soon as some deferred from a list resolve
#'
#' `when_some` creates a deferred value that is resolved as soon as the
#' specified number of deferred values resolve.
#'
#' `when_any` is a special case for a single.
#'
#' If the specified number of deferred values cannot be resolved, then
#' `when_any` throws an error.
#'
#' async has auto-cancellation, so if the required number of deferred values
#' are resolved, or too many of them throw error, the rest of the are
#' cancelled.
#'
#' If `when_any` throws an error, then all the underlying error objects
#' are returned in the `errors` member of the error object thrown by
#' `when_any`.
#'
#' @param count Number of deferred values that need to resolve.
#' @param ... Deferred values.
#' @param .list More deferred values.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @seealso [when_all()]
#' @noRd
#' @examples
#' \donttest{
#' ## Use the URL that returns first
#' afun <- function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_any(u1, u2)$then(function(x) x$url)
#' }
#' synchronise(afun())
#' }

when_some <- function(count, ..., .list = list()) {
  when_some_internal(count, ..., .list = .list, .race = FALSE)
}

when_some <- mark_as_async(when_some)

when_some_internal <- function(count, ..., .list, .race) {
  force(count)
  force(.race)
  defs <- c(list(...), .list)
  num_defs <- length(defs)
  num_failed <- 0L
  ifdef <- vlapply(defs, is_deferred)
  resolved <- defs[!ifdef]
  errors <- list()

  cancel_all <- function() lapply(defs[ifdef], function(x) x$cancel())

  deferred$new(
    type = "when_some",
    call = sys.call(),
    parents = defs[ifdef],
    action = function(resolve) {
      if (num_defs < count) {
        stop("Cannot resolve enough deferred values")
      } else if (length(resolved) >= count) {
        resolve(resolved[seq_len(count)])
      }
    },
    parent_resolve = function(value, resolve) {
      resolved <<- c(resolved, list(value))
      if (length(resolved) == count) {
        resolve(resolved)
      }
    },
    parent_reject = function(value, resolve) {
      if (.race) {
        stop(value)
      }
      num_failed <<- num_failed + 1L
      errors <<- c(errors, list(value))
      if (num_failed + count == num_defs + 1L) {
        err <- structure(
          list(errors = errors, message = "when_some / when_any failed"),
          class = c("async_rejected", "error", "condition")
        )
        stop(err)
      }
    }
  )
}

#' @noRd
#' @rdname when_some

when_any <- function(..., .list = list()) {
  when_some(1, ..., .list = .list)$then(function(x) x[[1]])
}

when_any <- mark_as_async(when_any)

#' Repeatedly call task, while test returns true
#'
#' @param test Synchronous test function.
#' @param task Asynchronous function to call repeatedly.
#' @param ... Arguments to pass to `task`.
#' @return Deferred value, that is resolved when the iteration is done.
#'
#' @family async control flow
#' @noRd
#' @examples
#' ## Keep calling while result is bigger than 0.1
#' calls <- 0
#' number <- Inf
#' synchronise(async_whilst(
#'   function() number >= 0.1,
#'   function() {
#'     calls <<- calls + 1
#'     number <<- runif(1)
#'   }
#' ))
#' calls

async_whilst <- function(test, task, ...) {
  force(test)
  task <- async(task)

  self <- deferred$new(
    type = "async_whilst",
    call = sys.call(),
    action = function(resolve) {
      if (!test()) {
        resolve(NULL)
      } else {
        task(...)$then(self)
      }
    },
    parent_resolve = function(value, resolve) {
      if (!test()) {
        resolve(value)
      } else {
        task(...)$then(self)
      }
    }
  )

  self
}

async_whilst <- mark_as_async(async_whilst)

#' Worker pool
#'
#' The worker pool functions are independent of the event loop, to allow
#' independent testing.
#'
#' @family worker pool functions
#' @name worker_pool
#' @noRd
#' @keywords internal
#' @importFrom R6 R6Class
NULL

worker_pool <- R6Class(
  public = list(
    initialize = function() wp_init(self, private),
    add_task = function(func, args, id, event_loop)
      wp_add_task(self, private, func, args, id, event_loop),
    get_fds = function() wp_get_fds(self, private),
    get_pids = function() wp_get_pids(self, private),
    get_poll_connections = function() wp_get_poll_connections(self, private),
    notify_event = function(pids, event_loop)
      wp_notify_event(self, private, pids, event_loop),
    start_workers = function() wp_start_workers(self, private),
    kill_workers = function() wp_kill_workers(self, private),
    cancel_task = function(id) wp_cancel_task(self, private, id),
    cancel_all_tasks = function() wp_cancel_all_tasks(self, private),
    get_result = function(id) wp_get_result(self, private, id),
    list_workers = function() wp_list_workers(self, private),
    list_tasks = function(event_loop = NULL, status = NULL)
      wp_list_tasks(self, private, event_loop, status)
  ),

  private = list(
    workers = list(),
    tasks = list(),

    finalize = function() self$kill_workers(),
    try_start = function() wp__try_start(self, private),
    interrupt_worker = function(pid) wp__interrupt_worker(self, private, pid)
  )
)

wp_init <- function(self, private) {
  self$start_workers()
  invisible(self)
}

wp_start_workers <- function(self, private) {
  num <- worker_pool_size()

  ## See if we need to start more
  if (NROW(private$workers) >= num) return(invisible())

  ## Yeah, start some more
  to_start <- num - NROW(private$workers)
  sess <- lapply(1:to_start, function(x) callr::r_session$new(wait = FALSE))
  fd <- viapply(
    sess,
    function(x) processx::conn_get_fileno(x$get_poll_connection())
  )
  new_workers <- data.frame(
    stringsAsFactors = FALSE,
    session = I(sess),
    task = NA_character_,
    pid = viapply(sess, function(x) x$get_pid()),
    fd = fd,
    event_loop = NA_integer_
  )

  private$workers <- rbind(private$workers, new_workers)
  invisible()
}

wp_add_task <- function(self, private, func, args, id, event_loop) {
  private$tasks <- rbind(
    private$tasks,
    data.frame(
      stringsAsFactors = FALSE,
      event_loop = event_loop,
      id = id,
      func = I(list(func)),
      args = I(list(args)),
      status = "waiting",
      result = I(list(NULL))
    )
  )

  private$try_start()
  invisible()
}

## We only need to poll the sessions that actually do something...

wp_get_fds <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  private$workers$fd[sts %in% c("starting", "busy")]
}

wp_get_pids <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  private$workers$pid[sts %in% c("starting", "busy")]
}

wp_get_poll_connections <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  busy <- sts %in% c("starting", "busy")
  structure(
    lapply(private$workers$session[busy], function(x) x$get_poll_connection()),
    names = private$workers$pid[busy]
  )
}

wp_notify_event <- function(self, private, pids, event_loop) {
  done <- NULL
  dead <- integer()
  which <- match(pids, private$workers$pid)
  for (w in which) {
    msg <- private$workers$session[[w]]$read()
    if (is.null(msg)) next
    if (msg$code == 200 || (msg$code >= 500 && msg$code < 600)) {
      if (msg$code >= 500 && msg$code < 600) dead <- c(dead, w)
      wt <- match(private$workers$task[[w]], private$tasks$id)
      if (is.na(wt)) stop("Internal error, no such task")
      private$tasks$result[[wt]] <- msg
      private$tasks$status[[wt]] <- "done"
      private$workers$task[[w]] <- NA_character_
      done <- c(done, private$tasks$id[[wt]])
    }
  }
  if (length(dead)) {
    private$workers <- private$workers[-dead, ]
    self$start_workers()
  }

  private$try_start()

  done
}

worker_pool_size <- function() {
  getOption("async.worker_pool_size") %||%
    as.integer(Sys.getenv("ASYNC_WORKER_POOL_SIZE", 4))
}

wp_kill_workers <- function(self, private) {
  lapply(private$workers$session, function(x) x$kill())
  private$workers <- NULL
  invisible()
}

wp_cancel_task <- function(self, private, id) {
  wt <- match(id, private$tasks$id)
  if (is.na(wt)) stop("Unknown task")

  if (private$tasks$status[[wt]] == "running") {
    wk <- match(id, private$workers$task)
    if (!is.na(wk)) private$interrupt_worker(private$workers$pid[wk])
  }
  private$tasks <- private$tasks[-wt, ]
  invisible()
}

wp_cancel_all_tasks <- function(self, private) {
  stop("`cancel_all_tasks` method is not implemented yet")
}

wp_get_result <- function(self, private, id) {
  wt <- match(id, private$tasks$id)
  if (is.na(wt)) stop("Unknown task")

  if (private$tasks$status[[wt]] != "done") stop("Task not done yet")
  result <- private$tasks$result[[wt]]
  private$tasks <- private$tasks[-wt, ]
  result
}

wp_list_workers <- function(self, private) {
  private$workers[, setdiff(colnames(private$workers), "session")]
}

wp_list_tasks <- function(self, private, event_loop, status) {
  dont_show <- c("func", "args", "result")
  ret <- private$tasks
  if (!is.null(event_loop)) ret <- ret[ret$event_loop %in% event_loop, ]
  if (!is.null(status)) ret <- ret[ret$status %in% status, ]
  ret[, setdiff(colnames(private$tasks), dont_show)]
}

## Internals -------------------------------------------------------------

#' @importFrom utils head

wp__try_start <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  if (all(sts != "idle")) return()
  can_work <- sts == "idle"

  can_run <- private$tasks$status == "waiting"
  num_start <- min(sum(can_work), sum(can_run))
  will_run <- head(which(can_run), num_start)
  will_work <- head(which(can_work), num_start)

  for (i in seq_along(will_run)) {
    wt <- will_run[[i]]
    ww <- will_work[[i]]
    func <- private$tasks$func[[wt]]
    args <- private$tasks$args[[wt]]
    private$workers$session[[ww]]$call(func, args)
    private$tasks$status[[wt]] <- "running"
    private$workers$task[[ww]] <- private$tasks$id[[wt]]
  }

  invisible()
}

#' Interrupt a worker process
#'
#' We need to make sure that the worker is in a usable state after this.
#'
#' For speed, we try to interrupt with a SIGINT first, and if that does
#' not work, then we kill the worker and start a new one.
#'
#' When we interrupt with a SIGINT a number of things can happen:
#' 1. we successfully interrupted a computation, then
#'    we'll just poll_io(), and read() and we'll get back an
#'    interrupt error.
#' 2. The computation has finished, so we did not interrupt it.
#'    In this case the background R process will apply the interrupt
#'    to the next computation (at least on Unix) so the bg process
#'    needs to run a quick harmless call to absorb the interrupt.
#'    We can use `Sys.sleep()` for this, and `write_input()` directly
#'    for speed and simplicity.
#' 3. The process has crashed already, in this case `interrupt()` will
#'    return `FALSE`. `poll_io()` will return with "ready" immediately,
#'    `read()` will return with an error, and `write_input()` throws
#'    an error.
#' 4. We could not interrupt the process, because it was in a
#'    non-interruptable state. In this case we kill it, and start a
#'    new process. `poll_io()` will not return with "ready" in this case.
#'
#' @param self self
#' @param private private self
#' @param pid pid of process
#' @noRd

wp__interrupt_worker <- function(self, private, pid) {
  ww <- match(pid, private$workers$pid)
  if (is.na(ww)) stop("Unknown task in interrupt_worker() method")

  kill <- FALSE
  sess <- private$workers$session[[ww]]
  int <- sess$interrupt()
  pr <- sess$poll_io(100)["process"]

  if (pr == "ready") {
    msg <- sess$read()
    if (!inherits(msg, "interrupt")) {
      tryCatch(
        {
          sess$write_input("base::Sys.sleep(0)\n")
          sess$read_output()
          sess$read_error()
        },
        error = function(e) kill <<- TRUE
      )
    }
    private$workers$task[[ww]] <- NA_character_
  } else {
    kill <- TRUE
  }

  if (kill) {
    sess$close()
    private$workers <- private$workers[-ww, ]
    ## Make sure that we have enough workers running
    self$start_workers()
  }

  invisible()
}

#' External process via a process generator
#'
#' Wrap any [processx::process] object into a deferred value. The
#' process is created by a generator function.
#'
#' @param process_generator Function that returns a [processx::process]
#'   object. See details below about the current requirements for the
#'   returned process.
#' @param error_on_status Whether to fail if the process terminates
#'   with a non-zero exit status.
#' @param ... Extra arguments, passed to `process_generator`.
#' @return Deferred object.
#'
#' Current requirements for `process_generator`:
#' * It must take a `...` argument, and pass it to
#'   `processx::process$new()`.
#' * It must use the `poll_connection = TRUE` argument.
#' These requirements might be relaxed in the future.
#'
#' If you want to obtain the standard output and/or error of the
#' process, then `process_generator` must redirect them to files.
#' If you want to discard them, `process_generator` can set them to
#' `NULL`.
#'
#' `process_generator` should not use pipes (`"|"`) for the standard
#' output or error, because the process will stop running if the
#' pipe buffer gets full. We currently never read out the pipe buffer.
#'
#' @noRd
#' @examples
#' \dontrun{
#' lsgen <- function(dir = ".", ...) {
#'   processx::process$new(
#'     "ls",
#'     dir,
#'     poll_connection = TRUE,
#'     stdout = tempfile(),
#'     stderr = tempfile(),
#'     ...
#'   )
#' }
#' afun <- function() {
#'   external_process(lsgen)
#' }
#' synchronise(afun())
#' }

external_process <- function(process_generator, error_on_status = TRUE, ...) {
  process_generator
  error_on_status
  args <- list(...)
  args$encoding <- args$encoding %||% ""
  args$cleanup_tree <- args$cleanup_tree %||% TRUE

  id <- NULL

  deferred$new(
    type = "external_process",
    call = sys.call(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      px <- do.call(process_generator, args)
      stdout <- px$get_output_file()
      stderr <- px$get_error_file()
      pipe <- px$get_poll_connection()
      id <<- get_default_event_loop()$add_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(
          process = px,
          stdout = stdout,
          stderr = stderr,
          error_on_status = error_on_status,
          encoding = args$encoding
        )
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}
