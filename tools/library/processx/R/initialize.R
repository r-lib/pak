
#' Start a process
#'
#' @param self this
#' @param private this$private
#' @param command Command to run, string scalar.
#' @param args Command arguments, character vector.
#' @param stdin Standard input, NULL to ignore.
#' @param stdout Standard output, NULL to ignore, TRUE for temp file.
#' @param stderr Standard error, NULL to ignore, TRUE for temp file.
#' @param pty Whether we create a PTY.
#' @param connections Connections to inherit in the child process.
#' @param poll_connection Whether to create a connection for polling.
#' @param env Environment vaiables.
#' @param cleanup Kill on GC?
#' @param cleanup_tree Kill process tree on GC?
#' @param wd working directory (or NULL)
#' @param echo_cmd Echo command before starting it?
#' @param supervise Should the process be supervised?
#' @param encoding Assumed stdout and stderr encoding.
#' @param post_process Post processing function.
#'
#' @keywords internal

process_initialize <- function(self, private, command, args,
                               stdin, stdout, stderr, pty, pty_options,
                               connections, poll_connection, env, cleanup,
                               cleanup_tree, wd, echo_cmd, supervise,
                               windows_verbatim_args, windows_hide_window,
                               windows_detached_process, encoding,
                               post_process) {

  "!DEBUG process_initialize `command`"

  assert_that(
    is_string(command),
    is.character(args),
    is_std_conn(stdin),
    is_std_conn(stdout),
    is_std_conn(stderr),
    is_flag(pty),
    is.list(pty_options), is_named(pty_options),
    is_connection_list(connections),
    is.null(poll_connection) || is_flag(poll_connection),
    is.null(env) || is_env_vector(env),
    is_flag(cleanup),
    is_flag(cleanup_tree),
    is_string_or_null(wd),
    is_flag(echo_cmd),
    is_flag(windows_verbatim_args),
    is_flag(windows_hide_window),
    is_flag(windows_detached_process),
    is_string(encoding),
    is.function(post_process) || is.null(post_process))

  if (cleanup_tree && !cleanup) {
    warning("`cleanup_tree` overrides `cleanup`, and process will be ",
            "killed on GC")
    cleanup <- TRUE
  }

  if (pty && os_type() != "unix") {
    throw(new_error("`pty = TRUE` is only implemented on Unix"))
  }
  if (pty && tolower(Sys.info()[["sysname"]]) == "sunos") {
    throw(new_error("`pty = TRUE` is not (yet) implemented on Solaris"))
  }
  if (pty && !is.null(stdin)) {
    throw(new_error("`stdin` must be `NULL` if `pty == TRUE`"))
  }
  if (pty && !is.null(stdout)) {
    throw(new_error("`stdout` must be `NULL` if `pty == TRUE`"))
  }
  if (pty && !is.null(stderr)) {
    throw(new_error("`stderr` must be `NULL` if `pty == TRUE`"))
  }

  def <- default_pty_options()
  pty_options <- utils::modifyList(def, pty_options)
  if (length(bad <- setdiff(names(def), names(pty_options)))) {
    throw(new_error("Uknown pty option(s): ",
                    paste(paste0("`", bad, "`"), collapse = ", ")))
  }
  pty_options$rows <- as.integer(pty_options$rows)
  pty_options$cols <- as.integer(pty_options$cols)
  pty_options <- pty_options[names(def)]

  command <- enc2path(command)
  args <- enc2path(args)

  wd <- wd %||% getwd()
  if (!is.null(wd)) {
    # check is needed if the current working directory does not exist
    # `mustWork = FALSE` is needed if the supplied wd does not exist
    wd <- enc2path(normalizePath(wd, mustWork = FALSE))
  }

  private$command <- command
  private$args <- args
  private$cleanup <- cleanup
  private$cleanup_tree <- cleanup_tree
  private$wd <- wd
  private$pstdin <- stdin
  private$pstdout <- stdout
  private$pstderr <- stderr
  private$pty <- pty
  private$pty_options <- pty_options
  private$connections <- connections
  private$env <- env
  private$echo_cmd <- echo_cmd
  private$windows_verbatim_args <- windows_verbatim_args
  private$windows_hide_window <- windows_hide_window
  private$encoding <- encoding
  private$post_process <- post_process

  poll_connection <- poll_connection %||%
    (!identical(stdout, "|") && !identical(stderr, "|") &&
     !length(connections))
  if (poll_connection) {
    pipe <- conn_create_pipepair()
    connections <- c(connections, list(pipe[[2]]))
    private$poll_pipe <- pipe[[1]]
  }

  if (echo_cmd) do_echo_cmd(command, args)

  if (!is.null(env)) env <- process_env(env)

  private$tree_id <- get_id()

  if (!is.null(wd)) {
    wd <- normalizePath(wd, winslash = "\\", mustWork = FALSE)
  }

  connections <- c(list(stdin, stdout, stderr), connections)

  "!DEBUG process_initialize exec()"
  private$status <- chain_call(
    c_processx_exec,
    command, c(command, args), pty, pty_options,
    connections, env, windows_verbatim_args, windows_hide_window,
    windows_detached_process, private, cleanup, wd, encoding,
    paste0("PROCESSX_", private$tree_id, "=YES")
  )

  ## We try the query the start time according to the OS, because we can
  ## use the (pid, start time) pair as an id when performing operations on
  ## the process, e.g. sending signals. This is only implemented on Linux,
  ## macOS and Windows and on other OSes it returns 0.0, so we just use the
  ## current time instead. (In the C process handle, there will be 0,
  ## still.)
  private$starttime <-
    chain_call(c_processx__proc_start_time, private$status)
  if (private$starttime == 0) private$starttime <- Sys.time()

  ## Need to close this, otherwise the child's end of the pipe
  ## will not be closed when the child exits, and then we cannot
  ## poll it.
  if (poll_connection) close(pipe[[2]])

  if (is.character(stdin) && stdin != "|" && stdin != "")
    stdin <- full_path(stdin)
  if (is.character(stdout) && stdout != "|" && stdout != "")
    stdout <- full_path(stdout)
  if (is.character(stderr) && stderr != "|" && stderr != "")
    stderr <- full_path(stderr)

  ## Store the output and error files, we'll open them later if needed
  private$stdin  <- stdin
  private$stdout <- stdout
  private$stderr <- stderr

  if (supervise) {
    supervisor_watch_pid(self$get_pid())
    private$supervised <- TRUE
  }

  invisible(self)
}
