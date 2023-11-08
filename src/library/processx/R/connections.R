
#' Processx connections
#'
#' These functions are currently experimental and will change
#' in the future. Note that processx connections are  _not_
#' compatible with R's built-in connection system.
#'
#' `conn_create_fd()` creates a connection from a file descriptor.
#'
#' @param fd Integer scalar, a Unix file descriptor.
#' @param encoding Encoding of the readable connection when reading.
#' @param close Whether to close the OS file descriptor when closing
#'   the connection. Sometimes you want to leave it open, and use it again
#'   in a `conn_create_fd` call.
#' Encoding to re-encode `str` into when writing.
#'
#' @family processx connections
#' @rdname processx_connections
#' @export

conn_create_fd <- function(fd, encoding = "", close = TRUE) {
  assert_that(
    is_integerish_scalar(fd),
    is_string(encoding),
    is_flag(close))
  fd <- as.integer(fd)
  chain_call(c_processx_connection_create_fd, fd, encoding, close)
}

#' Processx FIFOs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Create a FIFO for inter-process communication
#' Note that these functions are currently experimental.
#'
#' @details
#' `conn_create_fifo()` creates a FIFO and connects to it.
#' On Unix this is a proper FIFO in the file system, in the R temporary
#' directory. On Windows it is a named pipe.
#'
#' Use [conn_file_name()] to query the name of the FIFO, and
#' `conn_connect_fifo()` to connect to the other end.
#'
#' # Notes
#'
#' ## In general Unix domain sockets work better than FIFOs, so we suggest
#' you use sockets if you can. See [conn_create_unix_socket()].
#'
#' ## Creating the read end of the FIFO
#'
#' This case is simpler. To wait for a writer to connect to the FIFO
#' you can use [poll()] as usual. Then use [conn_read_chars()] or
#' [conn_read_lines()] to read from the FIFO, as usual. Use
#' [conn_is_incomplete()] *after* a read to check if there is more data,
#' or the writer is done.
#'
#' ## Creating the write end of the FIFO
#'
#' This is somewhat trickier. Creating the (non-blocking) FIFO does not
#' block. However, there is no easy way to tell if a reader is connected
#' to the other end of the FIFO or not. On Unix you can start using
#' [conn_write()] to try to write to it, and this will succeed, until the
#' buffer gets full, even if there is no reader. (When the buffer is full
#' it will return the data that was not written, as usual.)
#'
#' On Windows, using [conn_write()] to write to a FIFO without a reader
#' fails with an error. This is not great, we are planning to improve it
#' later.
#'
#' Right now, one workaround for this behavior is for the reader to
#' connunicate to the writer process independenctly that it has connected
#' to the FIFO. (E.g. another FIFO in the opposite direction can do that.)
#'
#' @param filename File name of the FIFO. On Windows it the name of the
#' pipe within the `\\?\pipe\` namespace, either the full name, or the
#' part after that prefix. If `NULL`, then a random name
#' is used, on Unix in the R temporary directory: [base::tempdir()].
#' @param read If `TRUE` then connect to the read end of the FIFO.
#'   Exactly one of `read` and `write` must be set to `TRUE`.
#' @param write If `TRUE` then connect to the write end of the FIFO.
#'   Exactly one of `read` and `write` must be set to `TRUE`.
#' @param encoding Encoding to assume.
#' @param nonblocking Whether this should be a non-blocking FIFO.
#' Note that blocking FIFOs are not well tested and might not work well with
#' [poll()], especially on Windows. We might remove this option in the
#' future and make all FIFOs non-blocking.
#'
#' @seealso [processx internals](https://processx.r-lib.org/dev/articles/internals.html)
#'
#' @rdname processx_fifos
#' @export

conn_create_fifo <- function(filename = NULL, read = NULL, write = NULL,
                             encoding = "", nonblocking = TRUE) {
  if (is.null(read) && is.null(write)) { read <- TRUE; write <- FALSE }
  if (is.null(read)) read <- !write
  if (is.null(write)) write <- !read

  if (read && write) {
    throw(new_error("Bi-directional FIFOs are not supported currently"))
  }

  assert_that(
    is_string_or_null(filename),
    is_flag(read),
    is_flag(write),
    read || write,
    ! (read && write),
    is_string(encoding),
    is_flag(nonblocking)
  )

  filename <- make_pipe_file_name(filename)

  chain_call(
    c_processx_connection_create_fifo,
    read,
    write,
    filename,
    encoding,
    nonblocking
  )
}

winpipeprefix <- "\\\\?\\pipe\\"

make_pipe_file_name <- function(filename) {
  if (is_windows()) {
    filename <- filename %||% basename(tempfile())
    if (!starts_with(filename, winpipeprefix)) {
      filename <- paste0(winpipeprefix, filename)
    }
  } else {
    filename <- filename %||% tempfile()
  }
  filename
}

#' @details
#' `conn_connect_fifo()` connects to a FIFO created with
#' `conn_create_fifo()`, typically in another process. `filename` refers
#' to the name of the pipe on Windows.
#'
#' On Windows, `conn_connect_fifo()` may be successful even if the
#' FIFO does not exist, but then later `poll()` or read/write operations
#' will fail. We are planning on changing this behavior in the future,
#' to make `conn_connect_fifo()` fail immediately, like on Unix.
#'
#' @rdname processx_fifos
#' @export
#' @examples
#' # Example for a non-blocking FIFO
#'
#' # Need to open the reading end first, otherwise Unix fails
#' reader <- conn_create_fifo()
#'
#' # Always use poll() before you read, with a timeout if you like.
#' # If you read before the other end of the FIFO is connected, then
#' # the OS (or processx?) assumes that the FIFO is done, and you cannot
#' # read anything.
#' # Now poll() tells us that there is no data yet.
#' poll(list(reader), 0)
#'
#' writer <- conn_connect_fifo(conn_file_name(reader), write = TRUE)
#' conn_write(writer, "hello\nthere!\n")
#'
#' poll(list(reader), 1000)
#' conn_read_lines(reader, 1)
#' conn_read_chars(reader)
#'
#' conn_is_incomplete(reader)
#'
#' close(writer)
#' conn_read_chars(reader)
#' conn_is_incomplete(reader)
#'
#' close(reader)

conn_connect_fifo <- function(filename, read = NULL, write = NULL,
                              encoding = "", nonblocking = TRUE) {
  if (is.null(read) && is.null(write)) { read <- TRUE; write <- FALSE }
  if (is.null(read)) read <- !write
  if (is.null(write)) write <- !read

  if (read && write) {
    throw(new_error("Bi-directional FIFOs are not supported currently"))
  }

  assert_that(
    is_string(filename),
    is_flag(read),
    is_flag(write),
    read || write,
    ! (read && write),
    is_string(encoding),
    is_flag(nonblocking)
  )

  if (is_windows()) {
    if (!starts_with(filename, winpipeprefix)) {
      filename <- paste0(winpipeprefix, filename)
    }
  }

  chain_call(
    c_processx_connection_connect_fifo,
    filename,
    read,
    write,
    encoding,
    nonblocking
  )
}

#' @details
#' `conn_file_name()` returns the name of the file associated with the
#' connection. For connections that do not refer to a file in the file
#' system it returns `NA_character()`. Except for named pipes on Windows,
#' where it returns the full name of the pipe.
#'
#' @rdname processx_connections
#' @export

conn_file_name <- function(con) {
  assert_that(is_connection(con))

  chain_call(c_processx_connection_file_name, con)
}

#' @details
#' `conn_create_pipepair()` creates a pair of connected connections, the
#' first one is writeable, the second one is readable.
#'
#' @param nonblocking Whether the pipe should be non-blocking.
#' For `conn_create_pipepair()` it must be a logical vector of length two,
#' for both ends of the pipe.
#'
#' @rdname processx_connections
#' @export

conn_create_pipepair <- function(encoding = "",
                                 nonblocking = c(TRUE, FALSE)) {
  assert_that(
    is_string(encoding),
    is.logical(nonblocking), length(nonblocking) == 2,
    !any(is.na(nonblocking)))
  chain_call(c_processx_connection_create_pipepair, encoding, nonblocking)
}

#' @details
#' `conn_read_chars()` reads UTF-8 characters from the connections. If the
#' connection itself is not UTF-8 encoded, it re-encodes it.
#'
#' @param con Processx connection object.
#' @param n Number of characters or lines to read. -1 means all available
#' characters or lines.
#'
#' @rdname processx_connections
#' @export

conn_read_chars <- function(con, n = -1)
  UseMethod("conn_read_chars", con)

#' @rdname processx_connections
#' @export

conn_read_chars.processx_connection <- function(con, n = -1) {
  processx_conn_read_chars(con, n)
}

#' @rdname processx_connections
#' @export

processx_conn_read_chars <- function(con, n = -1) {
  assert_that(is_connection(con), is_integerish_scalar(n))
  chain_call(c_processx_connection_read_chars, con, n)
}

#' @details
#' `conn_read_lines()` reads lines from a connection.
#'
#' @rdname processx_connections
#' @export

conn_read_lines <- function(con, n = -1)
  UseMethod("conn_read_lines", con)

#' @rdname processx_connections
#' @export

conn_read_lines.processx_connection <- function(con, n = -1) {
  processx_conn_read_lines(con, n)
}

#' @rdname processx_connections
#' @export

processx_conn_read_lines <- function(con, n = -1) {
  assert_that(is_connection(con), is_integerish_scalar(n))
  chain_call(c_processx_connection_read_lines, con, n)
}

#' @details
#' `conn_is_incomplete()` returns `FALSE` if the connection surely has no
#' more data.
#'
#' @rdname processx_connections
#' @export

conn_is_incomplete <- function(con)
  UseMethod("conn_is_incomplete", con)

#' @rdname processx_connections
#' @export

conn_is_incomplete.processx_connection <- function(con) {
  processx_conn_is_incomplete(con)
}

#' @rdname processx_connections
#' @export

processx_conn_is_incomplete <- function(con) {
  assert_that(is_connection(con))
  ! chain_call(c_processx_connection_is_eof, con)
}

#' @details
#' `conn_write()` writes a character or raw vector to the connection.
#' It might not be able to write all bytes into the connection, in which
#' case it returns the leftover bytes in a raw vector. Call `conn_write()`
#' again with this raw vector.
#'
#' @param str Character or raw vector to write.
#' @param sep Separator to use if `str` is a character vector. Ignored if
#' `str` is a raw vector.
#'
#' @rdname processx_connections
#' @export

conn_write <- function(con, str, sep = "\n", encoding = "")
  UseMethod("conn_write", con)

#' @rdname processx_connections
#' @export

conn_write.processx_connection <- function(con, str, sep = "\n",
                                           encoding = "") {
  processx_conn_write(con, str, sep, encoding)
}

#' @rdname processx_connections
#' @export

processx_conn_write <- function(con, str, sep = "\n", encoding = "") {
  assert_that(
    is_connection(con),
    (is.character(str) && all(! is.na(str))) || is.raw(str),
    is_string(sep),
    is_string(encoding))

  if (is.character(str)) {
    pstr <- paste(str, collapse = sep)
    str <- iconv(pstr, "", encoding, toRaw = TRUE)[[1]]
  }
  invisible(chain_call(c_processx_connection_write_bytes, con, str))
}

#' @details
#' `conn_create_file()` creates a connection to a file.
#'
#' @param filename File name. For `conn_create_pipe()` on Windows, a
#' `\\?\pipe` prefix is added to this, if it does not have such a prefix.
#' For `conn_create_pipe()` it can also be `NULL`, in which case a random
#' file name is used via `tempfile()`.
#' @param read Whether the connection is readable.
#' @param write Whethe the connection is writeable.
#'
#' @rdname processx_connections
#' @export

conn_create_file <- function(filename, read = NULL, write = NULL) {
  if (is.null(read) && is.null(write)) { read <- TRUE; write <- FALSE }
  if (is.null(read)) read <- !write
  if (is.null(write)) write <- !read

  assert_that(
    is_string(filename),
    is_flag(read),
    is_flag(write),
    read || write)

  chain_call(c_processx_connection_create_file, filename, read, write)
}

#' @details
#' `conn_set_stdout()` set the standard output of the R process, to the
#' specified connection.
#'
#' @param drop Whether to close the original stdout/stderr, or keep it
#' open and return a connection to it.
#'
#' @rdname processx_connections
#' @export

conn_set_stdout <- function(con, drop = TRUE) {
  assert_that(
    is_connection(con),
    is_flag(drop))

  flush(stdout())
  invisible(chain_call(c_processx_connection_set_stdout, con, drop))
}

#' @details
#' `conn_set_stderr()` set the standard error of the R process, to the
#' specified connection.
#'
#' @rdname processx_connections
#' @export

conn_set_stderr <- function(con, drop = TRUE) {
  assert_that(
    is_connection(con),
    is_flag(drop))

  flush(stderr())
  invisible(chain_call(c_processx_connection_set_stderr, con, drop))
}

#' @details
#' `conn_get_fileno()` return the integer file desciptor that belongs to
#' the connection.
#'
#' @rdname processx_connections
#' @export

conn_get_fileno <- function(con) {
  chain_call(c_processx_connection_get_fileno, con)
}

#' @details
#' `conn_disable_inheritance()` can be called to disable the inheritance
#' of all open handles. Call this function as soon as possible in a new
#' process to avoid inheriting the inherited handles even further.
#' The function is best effort to close the handles, it might still leave
#' some handles open. It should work for `stdin`, `stdout` and `stderr`,
#' at least.
#'
#' @rdname processx_connections
#' @export

conn_disable_inheritance <- function() {
  chain_call(c_processx_connection_disable_inheritance)
}

#' @rdname processx_connections
#' @export

close.processx_connection <- function(con, ...) {
  processx_conn_close(con, ...)
}

#' @param ... Extra arguments, for compatibility with the `close()`
#'    generic, currently ignored by processx.
#' @rdname processx_connections
#' @export

processx_conn_close <- function(con, ...) {
  chain_call(c_processx_connection_close, con)
}

#' @details
#' `is_valid_fd()` returns `TRUE` if `fd` is a valid open file
#' descriptor. You can use it to check if the R process has standard
#' input, output or error. E.g. R processes running in GUI (like RGui)
#' might not have any of the standard streams available.
#'
#' If a stream is redirected to the null device (e.g. in a callr
#' subprocess), that is is still a valid file descriptor.
#'
#' @rdname processx_connections
#' @export
#' @examples
#' is_valid_fd(0L)      # stdin
#' is_valid_fd(1L)      # stdout
#' is_valid_fd(2L)      # stderr

is_valid_fd <- function(fd) {
  assert_that(is_integerish_scalar(fd))
  fd <- as.integer(fd)
  chain_call(c_processx_is_valid_fd, fd)
}

#' Unix domain sockets
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Cross platform point-to-point inter-process communication with
#' Unix=domain sockets, implemented via named pipes on Windows.
#' These connection are always bidirectional, i.e. you can read from them
#' and also write to them.
#'
#' @details
#' `conn_create_unix_socket()` creates a server socket. The new socket
#' is listening at `filename`. See `filename` above.
#'
#' `conn_connect_unix_socket()` creates a client socket and connects it to
#' a server socket.
#'
#' `conn_accept_unix_socket()` accepts a client connection at a server
#' socket.
#'
#' `conn_unix_socket_state()` returns the state of the socket. Currently it
#' can return: `"listening"`, `"connected_server"`, `"connected_client"`.
#' It is possible that other states (e.g. for a closed socket) will be added
#' in the future.
#'
#' ## Notes
#'
#' * [poll()] works on sockets, but only polls for data to read, and
#'   currently ignores the write-end of the socket.
#' * [poll()] also works for accepting client connections. It will return
#'   `"connect"`is a client connection is available for a server socket.
#'   After this you can call `conn_accept_unix_socket()` to accept the
#'   client connection.
#'
#' @param filename File name of the socket. On Windows it the name of the
#' pipe within the `\\?\pipe\` namespace, either the full name, or the
#' part after that prefix. If `NULL`, then a random name
#' is used, on Unix in the R temporary directory: [base::tempdir()].
#' @param encoding Encoding to assume when reading from the socket.
#' @param con Connection. An error is thrown if not a socket connection.
#' @return A new socket connection.
#'
#' @seealso [processx internals](https://processx.r-lib.org/dev/articles/internals.html)
#'
#' @rdname processx_sockets
#' @export

conn_create_unix_socket <- function(filename = NULL, encoding = "") {

  assert_that(
    is_string_or_null(filename),
    is_string(encoding)
  )

  filename <- make_pipe_file_name(filename)

  chain_call(
    c_processx_connection_create_socket,
    filename,
    encoding
  )
}

#' @rdname processx_sockets
#' @export

conn_connect_unix_socket <- function(filename, encoding = "") {

  assert_that(
    is_string_or_null(filename),
    is_string(encoding)
  )

  if (is_windows()) {
    if (!starts_with(filename, winpipeprefix)) {
      filename <- paste0(winpipeprefix, filename)
    }
  }

  chain_call(
    c_processx_connection_connect_socket,
    filename,
    encoding
  )
}

#' @rdname processx_sockets
#' @export

conn_accept_unix_socket <- function(con) {
  assert_that(is_connection(con))

  invisible(chain_call(
    c_processx_connection_accept_socket,
    con
  ))
}

#' @rdname processx_sockets
#' @export

conn_unix_socket_state <- function(con) {
  assert_that(is_connection(con))

  code <- chain_call(
    c_processx_connection_socket_state,
    con
  )

  c("listening", "listening", "connected_server", "connected_client")[code]
}
