
#' @useDynLib processx, .registration = TRUE, .fixes = "c_"
NULL

## Workaround an R CMD check false positive
dummy_r6 <- function() R6::R6Class

#' External process
#'
#' @description
#' Managing external processes from R is not trivial, and this
#' class aims to help with this deficiency. It is essentially a small
#' wrapper around the `system` base R function, to return the process
#' id of the started process, and set its standard output and error
#' streams. The process id is then used to manage the process.
#'
#' @param n Number of characters or lines to read.
#' @param grace Currently not used.
#' @param close_connections Whether to close standard input, standard
#'   output, standard error connections and the poll connection, after
#'   killing the process.
#' @param timeout Timeout in milliseconds, for the wait or the I/O
#'   polling.
#'
#' @section Batch files:
#' Running Windows batch files (`.bat` or `.cmd` files) may be complicated
#' because of the `cmd.exe` command line parsing rules. For example you
#' cannot easily have whitespace in both the command (path) and one of the
#' arguments. To work around these limitations you need to start a
#' `cmd.exe` shell explicitly and use its `call` command. For example:
#'
#' ```r
#' process$new("cmd.exe", c("/c", "call", bat_file, "arg 1", "arg 2"))
#' ```
#'
#' This works even if `bat_file` contains whitespace characters.
#' For more information about this, see this processx issue:
#' https://github.com/r-lib/processx/issues/301
#'
#' The detailed parsing rules are at
#' https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/cmd
#'
#' A very good practical guide is at
#' https://ss64.com/nt/syntax-esc.html
#'
#' @section Polling:
#' The `poll_io()` function polls the standard output and standard
#' error connections of a process, with a timeout. If there is output
#' in either of them, or they are closed (e.g. because the process exits)
#' `poll_io()` returns immediately.
#'
#' In addition to polling a single process, the [poll()] function
#' can poll the output of several processes, and returns as soon as any
#' of them has generated output (or exited).
#'
#' @section Cleaning up background processes:
#' processx kills processes that are not referenced any more (if `cleanup`
#' is set to `TRUE`), or the whole subprocess tree (if `cleanup_tree` is
#' also set to `TRUE`).
#'
#' The cleanup happens when the references of the processes object are
#' garbage collected. To clean up earlier, you can call the `kill()` or
#' `kill_tree()` method of the process(es), from an `on.exit()` expression,
#' or an error handler:
#' ```r
#' process_manager <- function() {
#'   on.exit({
#'     try(p1$kill(), silent = TRUE)
#'     try(p2$kill(), silent = TRUE)
#'   }, add = TRUE)
#'   p1 <- process$new("sleep", "3")
#'   p2 <- process$new("sleep", "10")
#'   p1$wait()
#'   p2$wait()
#' }
#' process_manager()
#' ```
#'
#' If you interrupt `process_manager()` or an error happens then both `p1`
#' and `p2` are cleaned up immediately. Their connections will also be
#' closed. The same happens at a regular exit.
#'
#' @export
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' p <- process$new("sleep", "2")
#' p$is_alive()
#' p
#' p$kill()
#' p$is_alive()
#'
#' p <- process$new("sleep", "1")
#' p$is_alive()
#' Sys.sleep(2)
#' p$is_alive()

process <- R6::R6Class(
  "process",
  public = list(

    #' @description
    #' Start a new process in the background, and then return immediately.
    #'
    #' @return R6 object representing the process.
    #' @param command Character scalar, the command to run.
    #'   Note that this argument is not passed to a shell, so no
    #'   tilde-expansion or variable substitution is performed on it.
    #'   It should not be quoted with [base::shQuote()]. See
    #'   [base::normalizePath()] for tilde-expansion. If you want to run
    #'   `.bat` or `.cmd` files on Windows, make sure you read the
    #'   'Batch files' section above.
    #' @param args Character vector, arguments to the command. They will be
    #'   passed to the process as is, without a shell transforming them,
    #'   They don't need to be escaped.
    #' @param stdin What to do with the standard input. Possible values:
    #'   * `NULL`: set to the _null device_, i.e. no standard input is
    #'     provided;
    #'   * a file name, use this file as standard input;
    #'   * `"|"`: create a (writeable) connection for stdin.
    #'   * `""` (empty string): inherit it from the main R process. If the
    #'     main R process does not have a standard input stream, e.g. in
    #'     RGui on Windows, then an error is thrown.
    #' @param stdout  What to do with the standard output. Possible values:
    #'   * `NULL`: discard it;
    #'   * A string, redirect it to this file.
    #'     Note that if you specify a relative path, it will be relative to
    #'     the current working directory, even if you specify another
    #'     directory in the `wd` argument. (See issue 324.)
    #'   * `"|"`: create a connection for it.
    #'   * `""` (empty string): inherit it from the main R process. If the
    #'     main R process does not have a standard output stream, e.g. in
    #'     RGui on Windows, then an error is thrown.
    #' @param stderr What to do with the standard error. Possible values:
    #'   * `NULL`: discard it.
    #'   * A string, redirect it to this file.
    #'     Note that if you specify a relative path, it will be relative to
    #'     the current working directory, even if you specify another
    #'     directory in the `wd` argument. (See issue 324.)
    #'   * `"|"`: create a connection for it.
    #'   * `"2>&1"`: redirect it to the same connection (i.e. pipe or file)
    #'     as `stdout`. `"2>&1"` is a way to keep standard output and error
    #'     correctly interleaved.
    #'   * `""` (empty string): inherit it from the main R process. If the
    #'     main R process does not have a standard error stream, e.g. in
    #'     RGui on Windows, then an error is thrown.
    #' @param pty Whether to create a pseudo terminal (pty) for the
    #'   background process. This is currently only supported on Unix
    #'   systems, but not supported on Solaris.
    #'   If it is `TRUE`, then the `stdin`, `stdout` and `stderr` arguments
    #'   must be `NULL`. If a pseudo terminal is created, then processx
    #'   will create pipes for standard input and standard output. There is
    #'   no separate pipe for standard error, because there is no way to
    #'   distinguish between stdout and stderr on a pty. Note that the
    #'   standard output connection of the pty is _blocking_, so we always
    #'   poll the standard output connection before reading from it using
    #'   the `$read_output()` method. Also, because `$read_output_lines()`
    #'   could still block if no complete line is available, this function
    #'   always fails if the process has a pty. Use `$read_output()` to
    #'   read from ptys.
    #' @param pty_options Unix pseudo terminal options, a named list. see
    #'   [default_pty_options()] for details and defaults.
    #' @param connections A list of processx connections to pass to the
    #'   child process. This is an experimental feature currently.
    #' @param poll_connection Whether to create an extra connection to the
    #'   process that allows polling, even if the standard input and
    #'   standard output are not pipes. If this is `NULL` (the default),
    #'   then this connection will be only created if standard output and
    #'   standard error are not pipes, and `connections` is an empty list.
    #'   If the poll connection is created, you can query it via
    #'   `p$get_poll_connection()` and it is also included in the response
    #'   to `p$poll_io()` and [poll()]. The numeric file descriptor of the
    #'   poll connection comes right after `stderr` (2), and the
    #'   connections listed in `connections`.
    #' @param env Environment variables of the child process. If `NULL`,
    #'   the parent's environment is inherited. On Windows, many programs
    #'   cannot function correctly if some environment variables are not
    #'   set, so we always set `HOMEDRIVE`, `HOMEPATH`, `LOGONSERVER`,
    #'   `PATH`, `SYSTEMDRIVE`, `SYSTEMROOT`, `TEMP`, `USERDOMAIN`,
    #'   `USERNAME`, `USERPROFILE` and `WINDIR`. To append new environment
    #'   variables to the ones set in the current process, specify
    #'   `"current"` in `env`, without a name, and the appended ones with
    #'   names. The appended ones can overwrite the current ones.
    #' @param cleanup Whether to kill the process when the `process`
    #'   object is garbage collected.
    #' @param cleanup_tree Whether to kill the process and its child
    #'   process tree when the `process` object is garbage collected.
    #' @param wd Working directory of the process. It must exist.
    #'   If `NULL`, then the current working directory is used.
    #' @param echo_cmd Whether to print the command to the screen before
    #'   running it.
    #' @param supervise Whether to register the process with a supervisor.
    #'   If `TRUE`, the supervisor will ensure that the process is
    #'   killed when the R process exits.
    #' @param windows_verbatim_args Whether to omit quoting the arguments
    #'   on Windows. It is ignored on other platforms.
    #' @param windows_hide_window Whether to hide the application's window
    #'   on Windows. It is ignored on other platforms.
    #' @param windows_detached_process Whether to use the
    #'   `DETACHED_PROCESS` flag on Windows. If this is `TRUE`, then
    #'   the child process will have no attached console, even if the
    #'   parent had one.
    #' @param encoding The encoding to assume for `stdin`, `stdout` and
    #'   `stderr`. By default the encoding of the current locale is
    #'   used. Note that `processx` always reencodes the output of the
    #'  `stdout` and `stderr` streams in UTF-8 currently.
    #'   If you want to read them without any conversion, on all platforms,
    #'   specify `"UTF-8"` as encoding.
    #' @param post_process An optional function to run when the process has
    #'   finished. Currently it only runs if `$get_result()` is called.
    #'   It is only run once.

    initialize = function(command = NULL, args = character(),
      stdin = NULL, stdout = NULL, stderr = NULL, pty = FALSE,
      pty_options = list(), connections = list(), poll_connection = NULL,
      env = NULL, cleanup = TRUE, cleanup_tree = FALSE, wd = NULL,
      echo_cmd = FALSE, supervise = FALSE, windows_verbatim_args = FALSE,
      windows_hide_window = FALSE, windows_detached_process = !cleanup,
      encoding = "",  post_process = NULL)

      process_initialize(self, private, command, args, stdin,
                         stdout, stderr, pty, pty_options, connections,
                         poll_connection, env, cleanup, cleanup_tree, wd,
                         echo_cmd, supervise, windows_verbatim_args,
                         windows_hide_window, windows_detached_process,
                         encoding, post_process),

    #' @description
    #' Cleanup method that is called when the `process` object is garbage
    #' collected. If requested so in the process constructor, then it
    #' eliminates all processes in the process's subprocess tree.

    finalize = function() {
      if (!is.null(private$tree_id) && private$cleanup_tree &&
          ps::ps_is_supported()) self$kill_tree()
    },

    #' @description
    #' Terminate the process. It also terminate all of its child
    #' processes, except if they have created a new process group (on Unix),
    #' or job object (on Windows). It returns `TRUE` if the process
    #' was terminated, and `FALSE` if it was not (because it was
    #' already finished/dead when `processx` tried to terminate it).

    kill = function(grace = 0.1, close_connections = TRUE)
      process_kill(self, private, grace, close_connections),

    #' @description
    #' Process tree cleanup. It terminates the process
    #' (if still alive), together with any child (or grandchild, etc.)
    #' processes. It uses the _ps_ package, so that needs to be installed,
    #' and _ps_ needs to support the current platform as well. Process tree
    #' cleanup works by marking the process with an environment variable,
    #' which is inherited in all child processes. This allows finding
    #' descendents, even if they are orphaned, i.e. they are not connected
    #' to the root of the tree cleanup in the process tree any more.
    #' `$kill_tree()` returns a named integer vector of the process ids that
    #' were killed, the names are the names of the processes (e.g. `"sleep"`,
    #' `"notepad.exe"`, `"Rterm.exe"`, etc.).

    kill_tree = function(grace = 0.1, close_connections = TRUE)
      process_kill_tree(self, private, grace, close_connections),

    #' @description
    #' Send a signal to the process. On Windows only the
    #' `SIGINT`, `SIGTERM` and `SIGKILL` signals are interpreted,
    #' and the special 0 signal. The first three all kill the process. The 0
    #' signal returns `TRUE` if the process is alive, and `FALSE`
    #' otherwise. On Unix all signals are supported that the OS supports,
    #' and the 0 signal as well.
    #' @param signal An integer scalar, the id of the signal to send to
    #'   the process. See [tools::pskill()] for the list of signals.

    signal = function(signal)
      process_signal(self, private, signal),

    #' @description
    #' Send an interrupt to the process. On Unix this is a
    #' `SIGINT` signal, and it is usually equivalent to pressing CTRL+C at
    #' the terminal prompt. On Windows, it is a CTRL+BREAK keypress.
    #' Applications may catch these events. By default they will quit.

    interrupt = function()
      process_interrupt(self, private),

    #' @description
    #' Query the process id.
    #' @return Integer scalar, the process id of the process.

    get_pid = function()
      process_get_pid(self, private),

    #' @description Check if the process is alive.
    #' @return Logical scalar.

    is_alive = function()
      process_is_alive(self, private),

    #' @description
    #' Wait until the process finishes, or a timeout happens.
    #' Note that if the process never finishes, and the timeout is infinite
    #' (the default), then R will never regain control. In some rare cases,
    #' `$wait()` might take a bit longer than specified to time out. This
    #' happens on Unix, when another package overwrites the processx
    #' `SIGCHLD` signal handler, after the processx process has started.
    #' One such package is parallel, if used with fork clusters, e.g.
    #' through `parallel::mcparallel()`.
    #' @return It returns the process itself, invisibly.

    wait = function(timeout = -1)
      process_wait(self, private, timeout),

    #' @description
    #' `$get_exit_status` returns the exit code of the process if it has
    #' finished and `NULL` otherwise. On Unix, in some rare cases, the exit
    #' status might be `NA`. This happens if another package (or R itself)
    #' overwrites the processx `SIGCHLD` handler, after the processx process
    #' has started. In these cases processx cannot determine the real exit
    #' status of the process. One such package is parallel, if used with
    #' fork clusters, e.g. through the `parallel::mcparallel()` function.

    get_exit_status = function()
      process_get_exit_status(self, private),

    #' @description
    #' `format(p)` or `p$format()` creates a string representation of the
    #' process, usually for printing.

    format = function()
      process_format(self, private),

    #' @description
    #' `print(p)` or `p$print()` shows some information about the
    #' process on the screen, whether it is running and it's process id, etc.

    print = function()
      process_print(self, private),

    #' @description
    #' `$get_start_time()` returns the time when the process was
    #' started.

    get_start_time = function()
      process_get_start_time(self, private),

    #' @description
    #' `$is_supervised()` returns whether the process is being tracked by
    #' supervisor process.

    is_supervised = function()
      process_is_supervised(self, private),

    #' @description
    #' `$supervise()` if passed `TRUE`, tells the supervisor to start
    #' tracking the process. If `FALSE`, tells the supervisor to stop
    #' tracking the process. Note that even if the supervisor is disabled
    #' for a process, if it was started with `cleanup = TRUE`, the process
    #' will still be killed when the object is garbage collected.
    #' @param status Whether to turn on of off the supervisor for this
    #'   process.

    supervise = function(status)
      process_supervise(self, private, status),

    ## Output

    #' @description
    #' `$read_output()` reads from the standard output connection of the
    #' process. If the standard output connection was not requested, then
    #' then it returns an error. It uses a non-blocking text connection. This
    #' will work only if `stdout="|"` was used. Otherwise, it will throw an
    #' error.

    read_output = function(n = -1)
      process_read_output(self, private, n),

    #' @description
    #' `$read_error()` is similar to `$read_output`, but it reads
    #' from the standard error stream.

    read_error = function(n = -1)
      process_read_error(self, private, n),

    #' @description
    #' `$read_output_lines()` reads lines from standard output connection
    #' of the process. If the standard output connection was not requested,
    #' then it returns an error. It uses a non-blocking text connection.
    #' This will work only if `stdout="|"` was used. Otherwise, it will
    #' throw an error.

    read_output_lines = function(n = -1)
      process_read_output_lines(self, private, n),

    #' @description
    #' `$read_error_lines()` is similar to `$read_output_lines`, but
    #' it reads from the standard error stream.

    read_error_lines = function(n = -1)
      process_read_error_lines(self, private, n),

    #' @description
    #' `$is_incomplete_output()` return `FALSE` if the other end of
    #' the standard output connection was closed (most probably because the
    #' process exited). It return `TRUE` otherwise.

    is_incomplete_output = function()
      process_is_incompelete_output(self, private),

    #' @description
    #' `$is_incomplete_error()` return `FALSE` if the other end of
    #' the standard error connection was closed (most probably because the
    #' process exited). It return `TRUE` otherwise.

    is_incomplete_error = function()
      process_is_incompelete_error(self, private),

    #' @description
    #' `$has_input_connection()` return `TRUE` if there is a connection
    #' object for standard input; in other words, if `stdout="|"`. It returns
    #' `FALSE` otherwise.

    has_input_connection = function()
      process_has_input_connection(self, private),

    #' @description
    #' `$has_output_connection()` returns `TRUE` if there is a connection
    #' object for standard output; in other words, if `stdout="|"`. It returns
    #' `FALSE` otherwise.

    has_output_connection = function()
      process_has_output_connection(self, private),

    #' @description
    #' `$has_error_connection()` returns `TRUE` if there is a connection
    #' object for standard error; in other words, if `stderr="|"`. It returns
    #' `FALSE` otherwise.

    has_error_connection = function()
      process_has_error_connection(self, private),

    #' @description
    #' `$has_poll_connection()` return `TRUE` if there is a poll connection,
    #' `FALSE` otherwise.

    has_poll_connection = function()
      process_has_poll_connection(self, private),

    #' @description
    #' `$get_input_connection()` returns a connection object, to the
    #' standard input stream of the process.

    get_input_connection =  function()
      process_get_input_connection(self, private),

    #' @description
    #' `$get_output_connection()` returns a connection object, to the
    #' standard output stream of the process.

    get_output_connection = function()
      process_get_output_connection(self, private),

    #' @description
    #' `$get_error_conneciton()` returns a connection object, to the
    #' standard error stream of the process.

    get_error_connection = function()
      process_get_error_connection(self, private),

    #' @description
    #' `$read_all_output()` waits for all standard output from the process.
    #' It does not return until the process has finished.
    #' Note that this process involves waiting for the process to finish,
    #' polling for I/O and potentially several `readLines()` calls.
    #' It returns a character scalar. This will return content only if
    #' `stdout="|"` was used. Otherwise, it will throw an error.

    read_all_output = function()
      process_read_all_output(self, private),

    #' @description
    #' `$read_all_error()` waits for all standard error from the process.
    #' It does not return until the process has finished.
    #' Note that this process involves waiting for the process to finish,
    #' polling for I/O and potentially several `readLines()` calls.
    #' It returns a character scalar. This will return content only if
    #' `stderr="|"` was used. Otherwise, it will throw an error.

    read_all_error = function()
      process_read_all_error(self, private),

    #' @description
    #' `$read_all_output_lines()` waits for all standard output lines
    #' from a process. It does not return until the process has finished.
    #' Note that this process involves waiting for the process to finish,
    #' polling for I/O and potentially several `readLines()` calls.
    #' It returns a character vector. This will return content only if
    #' `stdout="|"` was used. Otherwise, it will throw an error.

    read_all_output_lines = function()
      process_read_all_output_lines(self, private),

    #' @description
    #' `$read_all_error_lines()` waits for all standard error lines from
    #' a process. It does not return until the process has finished.
    #' Note that this process involves waiting for the process to finish,
    #' polling for I/O and potentially several `readLines()` calls.
    #' It returns a character vector. This will return content only if
    #' `stderr="|"` was used. Otherwise, it will throw an error.

    read_all_error_lines = function()
      process_read_all_error_lines(self, private),

    #' @description
    #' `$write_input()` writes the character vector (separated by `sep`) to
    #' the standard input of the process. It will be converted to the specified
    #' encoding. This operation is non-blocking, and it will return, even if
    #' the write fails (because the write buffer is full), or if it suceeds
    #' partially (i.e. not the full string is written). It returns with a raw
    #' vector, that contains the bytes that were not written. You can supply
    #' this raw vector to `$write_input()` again, until it is fully written,
    #' and then the return value will be `raw(0)` (invisibly).
    #'
    #' @param str Character or raw vector to write to the standard input
    #'   of the process. If a character vector with a marked encoding,
    #'   it will be converted to `encoding`.
    #' @param sep Separator to add between `str` elements if it is a
    #'   character vector. It is ignored if `str` is a raw vector.
    #' @return Leftover text (as a raw vector), that was not written.

    write_input = function(str, sep = "\n")
      process_write_input(self, private, str, sep),

    #' @description
    #' `$get_input_file()` if the `stdin` argument was a filename,
    #' this returns the absolute path to the file. If `stdin` was `"|"` or
    #' `NULL`, this simply returns that value.

    get_input_file = function()
      process_get_input_file(self, private),

    #' @description
    #' `$get_output_file()` if the `stdout` argument was a filename,
    #' this returns the absolute path to the file. If `stdout` was `"|"` or
    #' `NULL`, this simply returns that value.

    get_output_file = function()
      process_get_output_file(self, private),

    #' @description
    #' `$get_error_file()` if the `stderr` argument was a filename,
    #' this returns the absolute path to the file. If `stderr` was `"|"` or
    #' `NULL`, this simply returns that value.

    get_error_file = function()
      process_get_error_file(self, private),

    #' @description
    #' `$poll_io()` polls the process's connections for I/O. See more in
    #' the _Polling_ section, and see also the [poll()] function
    #' to poll on multiple processes.

    poll_io = function(timeout)
      process_poll_io(self, private, timeout),

    #' @description
    #' `$get_poll_connetion()` returns the poll connection, if the process has
    #' one.

    get_poll_connection = function()
      process_get_poll_connection(self, private),

    #' @description
    #' `$get_result()` returns the result of the post processesing function.
    #' It can only be called once the process has finished. If the process has
    #' no post-processing function, then `NULL` is returned.

    get_result = function()
      process_get_result(self, private),

    #' @description
    #' `$as_ps_handle()` returns a [ps::ps_handle] object, corresponding to
    #' the process.

    as_ps_handle = function()
      process_as_ps_handle(self, private),

    #' @description
    #' Calls [ps::ps_name()] to get the process name.

    get_name = function()
      ps_method(ps::ps_name, self),

    #' @description
    #' Calls [ps::ps_exe()] to get the path of the executable.

    get_exe = function()
      ps_method(ps::ps_exe, self),

    #' @description
    #' Calls [ps::ps_cmdline()] to get the command line.

    get_cmdline = function()
      ps_method(ps::ps_cmdline, self),

    #' @description
    #' Calls [ps::ps_status()] to get the process status.

    get_status = function()
      ps_method(ps::ps_status, self),

    #' @description
    #' calls [ps::ps_username()] to get the username.

    get_username = function()
      ps_method(ps::ps_username, self),

    #' @description
    #' Calls [ps::ps_cwd()] to get the current working directory.

    get_wd = function()
      ps_method(ps::ps_cwd, self),

    #' @description
    #' Calls [ps::ps_cpu_times()] to get CPU usage data.

    get_cpu_times = function()
      ps_method(ps::ps_cpu_times, self),

    #' @description
    #' Calls [ps::ps_memory_info()] to get memory data.

    get_memory_info = function()
      ps_method(ps::ps_memory_info, self),

    #' @description
    #' Calls [ps::ps_suspend()] to suspend the process.

    suspend = function()
      ps_method(ps::ps_suspend, self),

    #' @description
    #' Calls [ps::ps_resume()] to resume a suspended process.

    resume = function()
      ps_method(ps::ps_resume, self)
  ),

  private = list(

    command = NULL,       # Save 'command' argument here
    args = NULL,          # Save 'args' argument here
    cleanup = NULL,       # cleanup argument
    cleanup_tree = NULL,  # cleanup_tree argument
    stdin = NULL,         # stdin argument or stream
    stdout = NULL,        # stdout argument or stream
    stderr = NULL,        # stderr argument or stream
    pty = NULL,           # whether we should create a PTY
    pty_options = NULL,   # various PTY options
    pstdin = NULL,        # the original stdin argument
    pstdout = NULL,       # the original stdout argument
    pstderr = NULL,       # the original stderr argument
    cleanfiles = NULL,    # which temp stdout/stderr file(s) to clean up
    wd = NULL,            # working directory (or NULL for current)
    starttime = NULL,     # timestamp of start
    echo_cmd = NULL,      # whether to echo the command
    windows_verbatim_args = NULL,
    windows_hide_window = NULL,

    status = NULL,        # C file handle

    supervised = FALSE,   # Whether process is tracked by supervisor

    stdin_pipe = NULL,
    stdout_pipe = NULL,
    stderr_pipe = NULL,
    poll_pipe = NULL,

    encoding = "",

    env = NULL,

    connections = list(),

    post_process = NULL,
    post_process_result = NULL,
    post_process_done = FALSE,

    tree_id = NULL,

    get_short_name = function()
      process_get_short_name(self, private),
    close_connections = function()
      process_close_connections(self, private)
  )
)

## See the C source code for a discussion about the implementation
## of these methods

process_wait <- function(self, private, timeout) {
  "!DEBUG process_wait `private$get_short_name()`"
  chain_clean_call(
    c_processx_wait, private$status,
    as.integer(timeout),
    private$get_short_name()
  )
  invisible(self)
}

process_is_alive <- function(self, private) {
  "!DEBUG process_is_alive `private$get_short_name()`"
  chain_call(c_processx_is_alive, private$status, private$get_short_name())
}

process_get_exit_status <- function(self, private) {
  "!DEBUG process_get_exit_status `private$get_short_name()`"
  chain_call(c_processx_get_exit_status, private$status,
               private$get_short_name())
}

process_signal <- function(self, private, signal) {
  "!DEBUG process_signal `private$get_short_name()` `signal`"
  chain_call(c_processx_signal, private$status, as.integer(signal),
               private$get_short_name())
}

process_interrupt <- function(self, private) {
  "!DEBUG process_interrupt `private$get_short_name()`"
  if (os_type() == "windows") {
    pid <- as.character(self$get_pid())
    st <- run(get_tool("interrupt"), c(pid, "c"), error_on_status = FALSE)
    if (st$status == 0) TRUE else FALSE
  } else {
    chain_call(c_processx_interrupt, private$status,
                 private$get_short_name())
  }
}

process_kill <- function(self, private, grace, close_connections) {
  "!DEBUG process_kill '`private$get_short_name()`', pid `self$get_pid()`"
  ret <- chain_call(c_processx_kill, private$status, as.numeric(grace),
                      private$get_short_name())
  if (close_connections) private$close_connections()
  ret
}

process_kill_tree <- function(self, private, grace, close_connections) {
  "!DEBUG process_kill_tree '`private$get_short_name()`', pid `self$get_pid()`"
  if (!ps::ps_is_supported()) {
    throw(new_not_implemented_error(
      "kill_tree is not supported on this platform"))
  }

  ret <- get("ps_kill_tree", asNamespace("ps"))(private$tree_id)
  if (close_connections) private$close_connections()
  ret
}

process_get_start_time <- function(self, private) {
  format_unix_time(private$starttime)
}

process_get_pid <- function(self, private) {
  chain_call(c_processx_get_pid, private$status)
}

process_is_supervised <- function(self, private) {
  private$supervised
}

process_supervise <- function(self, private, status) {
  if (status && !self$is_supervised()) {
    supervisor_watch_pid(self$get_pid())
    private$supervised <- TRUE

  } else if (!status && self$is_supervised()) {
    supervisor_unwatch_pid(self$get_pid())
    private$supervised <- FALSE
  }
}

process_get_result <- function(self, private) {
  if (self$is_alive()) throw(new_error("Process is still alive"))
  if (!private$post_process_done && is.function(private$post_process)) {
    private$post_process_result <- private$post_process()
    private$post_process_done <- TRUE
  }
  private$post_process_result
}

process_as_ps_handle <- function(self, private) {
  ps::ps_handle(self$get_pid(), self$get_start_time())
}

ps_method <- function(fun, self) {
  fun(ps::ps_handle(self$get_pid(), self$get_start_time()))
}

process_close_connections <- function(self, private) {
  for (f in c("stdin_pipe", "stdout_pipe", "stderr_pipe", "poll_pipe")) {
    if (!is.null(p <- private[[f]])) {
      chain_call(c_processx_connection_close, p)
    }
  }
}

#' Default options for pseudo terminals (ptys)
#'
#' @return Named list of default values of pty options.
#'
#' Options and default values:
#' * `echo` whether to keep the echo on the terminal. `FALSE` turns echo
#'   off.
#' * `rows` the (initial) terminal size, number of rows.
#' * `cols` the (initial) terminal size, number of columns.
#'
#' @export

default_pty_options <- function() {
  list(
    echo = FALSE,
    rows = 25L,
    cols = 80L
  )
}
