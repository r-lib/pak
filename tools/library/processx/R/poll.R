
#' Poll for process I/O or termination
#'
#' Wait until one of the specified connections or processes produce
#' standard output or error, terminates, or a timeout occurs.
#'
#' @section Explanation of the return values:
#' * `nopipe` means that the stdout or stderr from this process was not
#'   captured.
#' * `ready` means that the connection or the stdout or stderr from this
#'   process are ready to read from. Note that end-of-file on these
#'   outputs also triggers `ready`.
#' * timeout`: the connections or processes are not ready to read from
#'   and a timeout happened.
#' * `closed`: the connection was already closed, before the polling
#'   started.
#' * `silent`: the connection is not ready to read from, but another
#'   connection was.
#'
#' @param processes A list of connection objects or`process` objects to
#'   wait on. (They can be mixed as well.) If this is a named list, then
#'   the returned list will have the same names. This simplifies the
#'   identification of the processes.
#' @param ms Integer scalar, a timeout for the polling, in milliseconds.
#'   Supply -1 for an infitite timeout, and 0 for not waiting at all.
#' @return A list of character vectors of length one or three.
#'   There is one list element for each connection/process, in the same
#'   order as in the input list. For connections the result is a single
#'   string scalar. For processes the character vectors' elements are named
#'   `output`, `error` and `process`. Possible values for each individual
#'   result are: `nopipe`, `ready`, `timeout`, `closed`, `silent`.
#'   See details about these below. `process` refers to the poll connection,
#'   see the `poll_connection` argument of the `process` initializer.
#'
#' @export
#' @examplesIf FALSE
#' # Different commands to run for windows and unix
#' cmd1 <- switch(
#'   .Platform$OS.type,
#'   "unix" = c("sh", "-c", "sleep 1; ls"),
#'   c("cmd", "/c", "ping -n 2 127.0.0.1 && dir /b")
#' )
#' cmd2 <- switch(
#'   .Platform$OS.type,
#'   "unix" = c("sh", "-c", "sleep 2; ls 1>&2"),
#'   c("cmd", "/c", "ping -n 2 127.0.0.1 && dir /b 1>&2")
#' )
#'
#' ## Run them. p1 writes to stdout, p2 to stderr, after some sleep
#' p1 <- process$new(cmd1[1], cmd1[-1], stdout = "|")
#' p2 <- process$new(cmd2[1], cmd2[-1], stderr = "|")
#'
#' ## Nothing to read initially
#' poll(list(p1 = p1, p2 = p2), 0)
#'
#' ## Wait until p1 finishes. Now p1 has some output
#' p1$wait()
#' poll(list(p1 = p1, p2 = p2), -1)
#'
#' ## Close p1's connection, p2 will have output on stderr, eventually
#' close(p1$get_output_connection())
#' poll(list(p1 = p1, p2 = p2), -1)
#'
#' ## Close p2's connection as well, no nothing to poll
#' close(p2$get_error_connection())
#' poll(list(p1 = p1, p2 = p2), 0)

poll <- function(processes, ms) {
  pollables <- processes
  assert_that(is_list_of_pollables(pollables))
  assert_that(is_integerish_scalar(ms))

  if (length(pollables) == 0) {
    return(structure(list(), names = names(pollables)))
  }

  proc <- vapply(pollables, inherits, logical(1), "process")
  conn <- vapply(pollables, is_connection, logical(1))
  type <- ifelse(proc, 1L, ifelse(conn, 2L, 3L))

  pollables[proc] <- lapply(pollables[proc], function(p) {
    list(get_private(p)$status, get_private(p)$poll_pipe)
  })

  res <- chain_call(c_processx_poll, pollables, type, as.integer(ms))
  res <- lapply(res, function(x) poll_codes[x])
  res[proc] <- lapply(res[proc], function(x) {
    set_names(x, c("output", "error", "process"))
  })
  names(res) <- names(pollables)
  res
}

#' Create a pollable object from a curl multi handle's file descriptors
#'
#' @param fds A list of file descriptors, as returned by
#'   [curl::multi_fdset()].
#' @return Pollable object, that be used with [poll()] directly.
#'
#' @export

curl_fds <- function(fds) {
  structure(
    list(fds$reads, fds$writes, fds$exceptions),
    class = "processx_curl_fds")
}
