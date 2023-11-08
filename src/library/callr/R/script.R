
make_vanilla_script_expr <- function(expr_file, res, error,
                                     pre_hook = NULL, post_hook = NULL,
                                     messages = FALSE,
                                     print_error = TRUE) {

  ## Code to handle errors in the child
  ## This will inserted into the main script
  if (! error %in% c("error", "stack", "debugger")) {
    throw(new_error("Unknown `error` argument: `", error, "`"))
  }

  err <- if (error == "error") {
    substitute({
      callr_data <- as.environment("tools:callr")$`__callr_data__`
      err <- callr_data$err

      if (`__traceback__`) {
        # This might be quieried for R sessions with $traceback()
        assign(".Traceback", .traceback(4), envir = callr_data)

        # Also dump frames, this might be queried as well with $debug()
        dump.frames("__callr_dump__")
        assign(".Last.dump", .GlobalEnv$`__callr_dump__`, envir = callr_data)
        rm("__callr_dump__", envir = .GlobalEnv)
      }

      e <- err$process_call(e)
      e2 <- err$new_error("error in callr subprocess")
      class(e2) <- c("callr_remote_error", class(e2))
      e2 <- err$add_trace_back(e2)
      cut <- which(e2$trace$scope == "global")[1]
      if (!is.na(cut)) {
        e2$trace <- e2$trace[-(1:cut), ]
      }

      saveRDS(list("error", e2, e), file = paste0(`__res__`, ".error"))
    }, list(
         `__res__` = res,
         `__traceback__` = getOption("callr.traceback", FALSE)
       )
    )

  } else if (error %in% c("stack", "debugger")) {
    substitute(
      {
        callr_data <- as.environment("tools:callr")$`__callr_data__`
        assign(".Traceback", .traceback(4), envir = callr_data)
        dump.frames("__dump__")         # nocov start
        saveRDS(
          list(`__type__`, e, .GlobalEnv$`__dump__`),
          file = paste0(`__res__`, ".error")
        )                               # nocov end
      },
      list(
        "__type__" = error,
        "__res__" = res
      )
    )
  }

  if (messages) {
    message <- function() {
      substitute({
        pxlib <- as.environment("tools:callr")$`__callr_data__`$pxlib
        if (is.null(e$code)) e$code <- "301"
        msg <- paste0("base64::", pxlib$base64_encode(serialize(e, NULL)))
        data <- paste0(e$code, " ", nchar(msg), "\n", msg)
        pxlib$write_fd(3L, data)

        if (inherits(e, "cli_message") &&
            !is.null(findRestart("cli_message_handled"))) {
          invokeRestart("cli_message_handled")
        } else if (inherits(e, "message") &&
                   !is.null(findRestart("muffleMessage"))) {
          invokeRestart("muffleMessage")
        }
      })
    }
  } else {
    message <- function() substitute(signalCondition(e))
  }

  ## The function to run and its arguments are saved as a list:
  ## list(fun, args). args itself is a list.
  ## So the first do.call will create the call: do.call(fun, args)
  ## The second do.call will perform fun(args).
  ##
  ## The c() is needed because the first .GlobalEnv is itself
  ## an argument to the do.call within the do.call.
  ##
  ## It is important that we do not create any temporary variables,
  ## the function is called from an empty global environment.
  substitute(
     {
      tryCatch(                         # nocov start
        withCallingHandlers(
          {
            `__pre_hook__`
            saveRDS(
              do.call(
                do.call,
                c(readRDS(`__expr_file__`), list(envir = .GlobalEnv, quote = TRUE)),
                envir = .GlobalEnv,
                quote = TRUE
              ),
              file = `__res__`,
              compress = `__compress__`
            )
            flush(stdout())
            flush(stderr())
            `__post_hook__`
            invisible()
          },
          error = function(e) { `__error__` },
          interrupt = function(e) { `__error__` },
          callr_message = function(e) { try(`__message__`) }
        ),

        ## We need to `stop()` here again, otherwise the error message
        ## is not printed to stderr. See
        ## https://github.com/r-lib/callr/issues/80
        ## However, on R 3.1 and R 3.2 throwing an error here
        ## will crash the R process. With `try()` the error is still
        ## printed to stderr, but no real error is thrown.
        error = function(e) {
          `__post_hook__`;
          if (`__print_error__`) {
            try(stop(e))
          } else {
            invisible()
          }
        },
        interrupt = function(e) {
          `__post_hook__`
          if (`__print_error__`) {
            e
          } else {
            invisible()
          }
        }
      )                                 # nocov end
    },

    list(`__error__` = err, `__expr_file__` = expr_file, `__res__` = res,
         `__pre_hook__` = pre_hook, `__post_hook__` = post_hook,
         `__message__` = message(),
         `__compress__` = getOption("callr.compress_transport", FALSE),
         `__print_error__` = print_error)
  )
}

make_vanilla_script_file <- function(expr_file, res, error, print_error) {
  expr <- make_vanilla_script_expr(
    expr_file,
    res,
    error,
    print_error = print_error
  )
  script <- deparse(expr)

  tmp <- tempfile("callr-scr-")
  cat(script, file = tmp, sep = "\n")
  tmp
}
