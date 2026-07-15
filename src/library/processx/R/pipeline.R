#' Pipeline of processes connected with pipes
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A `pipeline` object represents a sequence of processes whose standard
#' input and output streams are connected with pipes, like a Unix pipeline
#' (`cmd1 | cmd2 | cmd3`). Data flows directly between the child processes
#' via kernel-level pipes — the parent R process sees only the output of the
#' final command (when `stdout = "|"`).
#'
#' @param cmds A non-empty list of character vectors. Each vector is one
#'   command: the first element is the executable and the rest are its
#'   arguments. Example: `list(c("sort"), c("uniq", "-c"))`.
#' @param stdin Standard input for the *first* process. `NULL` to discard,
#'   `"|"` so the parent R process can write to it via `$write_input()`, or
#'   a file path.
#' @param stdout Standard output of the *last* process. `"|"` (the default)
#'   so the parent R process can read from it, `NULL` to discard, or a file
#'   path.
#' @param stderr Standard error for *all* processes. `NULL` (the default) to
#'   discard, `"|"` to create a separate readable pipe per process, `"2>&1"`
#'   to merge into stdout, or a file path. When `"|"`, use
#'   `$read_error()` to read from the last process; use `$get_processes()`
#'   to access individual process objects for other processes.
#' @param env Environment variables for all processes, or `NULL` to inherit
#'   the parent environment.
#' @param encoding Assumed encoding for stdin/stdout/stderr streams.
#' @param wd Working directory for all processes, or `NULL` for the current
#'   directory.
#' @param cleanup Whether to kill the processes on garbage collection.
#' @param cleanup_tree Whether to kill the full process trees on garbage
#'   collection.
#' @param n Number of characters or lines to read. -1 means all available.
#' @param str String to write to the process stdin.
#' @param sep Separator to add after `str`.
#' @param timeout Timeout in milliseconds. -1 means no timeout.
#' @param grace Grace period in seconds before sending SIGKILL (Unix) or
#'   terminating forcefully (Windows). Currently not used.
#' @param close_connections Whether to close connections after killing.
#' @param ... Not used, for compatibility with the generic.
#'
#' @section Methods:
#' `pipeline$new(cmds, stdin, stdout, stderr, env, encoding, wd,
#'   cleanup, cleanup_tree)`
#'
#' `$read_output(n = -1)`, `$read_output_lines(n = -1)`,
#' `$read_all_output()`, `$read_all_output_lines()` — read from the last
#' process (only meaningful when `stdout = "|"`).
#'
#' `$poll_io(timeout)` — poll the last process's connections for I/O.
#'
#' `$read_error(n = -1)`, `$read_error_lines(n = -1)`,
#' `$read_all_error()`, `$read_all_error_lines()` — read stderr of the
#' last process (only meaningful when `stderr = "|"`).
#'
#' `$write_input(str, sep = "\n")` — write to first process stdin
#' (only meaningful when `stdin = "|"`).
#'
#' `$close_input()` — close the first process stdin, signalling EOF.
#'
#' `$wait(timeout = -1)` — wait for all processes to finish.
#'
#' `$kill(grace = 0.1, close_connections = TRUE)` — kill all processes.
#'
#' `$kill_tree(grace = 0.1, close_connections = TRUE)` — kill all
#' process trees.
#'
#' `$is_alive()` — returns `TRUE` if any process is still running.
#'
#' `$get_exit_statuses()` — list of exit codes (one per process; `NULL`
#' if still running).
#'
#' `$get_pids()` — integer vector of process IDs.
#'
#' `$get_processes()` — list of [process] objects, one per command.
#'
#' `$format()` — string representation of the pipeline.
#'
#' `$print()` — print the pipeline to the screen.
#'
#' @examples
#' \dontrun{
#' # sort | uniq, reading from / writing to R
#' pl <- pipeline$new(
#'   list(c("sort"), c("uniq")),
#'   stdin = "|", stdout = "|"
#' )
#' pl$write_input("b\na\nb\na\n")
#' pl$close_input()
#' pl$read_all_output_lines()
#' pl$wait()
#' pl$get_exit_statuses()
#' }
#'
#' @export
pipeline <- R6::R6Class(
  "pipeline",
  cloneable = FALSE,

  public = list(

    #' @description Create a new pipeline.
    initialize = function(
      cmds,
      stdin = NULL,
      stdout = "|",
      stderr = NULL,
      env = NULL,
      encoding = "utf-8",
      wd = NULL,
      cleanup = TRUE,
      cleanup_tree = FALSE
    ) {
      if (!is.list(cmds) || length(cmds) == 0L) {
        throw(new_error("`cmds` must be a non-empty list of character vectors"))
      }
      for (i in seq_along(cmds)) {
        if (!is.character(cmds[[i]]) || length(cmds[[i]]) == 0L) {
          throw(new_error(paste0(
            "`cmds[[", i, "]]` must be a non-empty character vector"
          )))
        }
      }

      n <- length(cmds)

      ## Spawn all processes, creating one inter-process pipe per iteration
      ## so that later children cannot inherit handles from pipes that do not
      ## concern them.  On Windows, CreateProcess with bInheritHandles=TRUE
      ## passes every inheritable handle to the child.  Creating each pipe
      ## just before the two processes that need it are spawned — and closing
      ## both ends in the parent immediately after — ensures no child ever
      ## holds a stray write-end that would prevent EOF from propagating.
      ## On Unix, O_CLOEXEC already prevents inheritance, but the same
      ## iterative pattern keeps the logic consistent.
      procs     <- vector("list", n)
      prev_read <- NULL   ## read end of the previous inter-process pipe

      for (i in seq_len(n)) {
        cmd <- cmds[[i]]

        ## Create the pipe connecting process i's stdout to process i+1's
        ## stdin, unless this is the last process.
        if (i < n) {
          next_pipe   <- conn_create_proc_pipepair()
          proc_stdout <- next_pipe[[1L]]   ## write end → child's stdout
        } else {
          next_pipe   <- NULL
          proc_stdout <- stdout
        }

        proc_stdin <- if (i == 1L) stdin else prev_read
        ## Disable poll_connection for intermediate processes: their stdout is
        ## a connection (not "|"), so the default formula would create an
        ## unnecessary extra pipe.
        proc_poll  <- if (i < n) FALSE else NULL

        procs[[i]] <- process$new(
          cmd[[1L]],
          cmd[-1L],
          stdin            = proc_stdin,
          stdout           = proc_stdout,
          stderr           = stderr,
          env              = env,
          encoding         = encoding,
          wd               = wd,
          cleanup          = cleanup,
          cleanup_tree     = cleanup_tree,
          poll_connection  = proc_poll
        )

        ## Close parent's copies immediately: the child now owns these
        ## handles, and closing here prevents the next child from inheriting
        ## the write-end of a pipe it should only read from.
        if (!is.null(next_pipe)) close(next_pipe[[1L]])  ## write end → stdout of process i
        if (!is.null(prev_read)) close(prev_read)         ## read end  → stdin  of process i

        prev_read <- if (!is.null(next_pipe)) next_pipe[[2L]] else NULL
      }

      private$procs <- procs
      invisible(self)
    },

    ## ------------------------------------------------------------------ ##
    ##  Output (last process)                                               ##
    ## ------------------------------------------------------------------ ##

    #' @description Read output of the last process.
    read_output = function(n = -1) {
      private$last()$read_output(n)
    },

    #' @description Read output lines of the last process.
    read_output_lines = function(n = -1) {
      private$last()$read_output_lines(n)
    },

    #' @description Read all output of the last process.
    read_all_output = function() {
      private$last()$read_all_output()
    },

    #' @description Read all output lines of the last process.
    read_all_output_lines = function() {
      private$last()$read_all_output_lines()
    },

    #' @description Poll the connections of the last process for I/O.
    poll_io = function(timeout) {
      private$last()$poll_io(timeout)
    },

    ## ------------------------------------------------------------------ ##
    ##  Error (last process)                                                ##
    ## ------------------------------------------------------------------ ##

    #' @description Read stderr of the last process.
    read_error = function(n = -1) {
      private$last()$read_error(n)
    },

    #' @description Read stderr lines of the last process.
    read_error_lines = function(n = -1) {
      private$last()$read_error_lines(n)
    },

    #' @description Read all stderr of the last process.
    read_all_error = function() {
      private$last()$read_all_error()
    },

    #' @description Read all stderr lines of the last process.
    read_all_error_lines = function() {
      private$last()$read_all_error_lines()
    },

    ## ------------------------------------------------------------------ ##
    ##  Input (first process)                                               ##
    ## ------------------------------------------------------------------ ##

    #' @description Write to the first process stdin.
    write_input = function(str, sep = "\n") {
      private$procs[[1L]]$write_input(str, sep)
    },

    #' @description Close the first process stdin (signals EOF to the process).
    close_input = function() {
      close(private$procs[[1L]]$get_input_connection())
    },

    ## ------------------------------------------------------------------ ##
    ##  Lifecycle                                                           ##
    ## ------------------------------------------------------------------ ##

    #' @description Wait for all processes to finish.
    wait = function(timeout = -1) {
      ## Wait for the last process first: it consumes the pipeline output.
      ## Then wait for the rest in reverse order.
      for (p in rev(private$procs)) {
        p$wait(timeout)
      }
      invisible(self)
    },

    #' @description Kill all processes.
    kill = function(grace = 0.1, close_connections = TRUE) {
      for (p in private$procs) p$kill(grace, close_connections)
      invisible(self)
    },

    #' @description Kill all process trees.
    kill_tree = function(grace = 0.1, close_connections = TRUE) {
      for (p in private$procs) p$kill_tree(grace, close_connections)
      invisible(self)
    },

    #' @description Check if any process is still alive.
    is_alive = function() {
      any(vapply(private$procs, function(p) p$is_alive(), logical(1L)))
    },

    ## ------------------------------------------------------------------ ##
    ##  Status / accessors                                                  ##
    ## ------------------------------------------------------------------ ##

    #' @description Return exit codes for all processes.
    get_exit_statuses = function() {
      lapply(private$procs, function(p) p$get_exit_status())
    },

    #' @description Return PIDs for all processes.
    get_pids = function() {
      vapply(private$procs, function(p) p$get_pid(), integer(1L))
    },

    #' @description Return the list of process objects.
    get_processes = function() {
      private$procs
    },

    #' @description Format the pipeline as a string.
    format = function() pipeline_format(self, private),

    #' @description Print the pipeline to the screen.
    print = function(...) pipeline_print(self, private)
  ),

  private = list(
    procs = NULL,
    last  = function() private$procs[[length(private$procs)]]
  )
)
