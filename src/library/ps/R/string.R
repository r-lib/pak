#' Encode a `ps_handle` as a short string
#'
#' A convenient format for passing between processes, naming semaphores, or
#' using as a directory/file name. Will always be 12 alphanumeric characters,
#' with the first character guarantied to be a letter. Encodes the pid and
#' creation time for a process.
#'
#' @param p Process handle.
#'
#' @return A process string (scalar character), that can be passed to
#' `ps_handle()` in place of a pid.
#'
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' (p <- ps_handle())
#' (str <- ps_string(p))
#' ps_handle(pid = str)

ps_string <- function(p = ps_handle()) {
  assert_ps_handle(p)
  ps__str_encode(p)
}


ps__str_encode <- function(p) {

  # Assumptions:
  #   - Date < 8888-12-02 (Windows only).
  #   - System uptime < 6918 years (Unix only).
  #   - PID <= 768,369,472 (current std max = 4,194,304).
  #   - PIDs are not reused within the same millisecond.

  # Surprisingly, `ps_boot_time()` is not constant from process to process, and
  # `ps_create_time()` is derived from `ps_boot_time()` on Unix. Therefore:
  #   - On Windows, encode `ps_create_time()`
  #   - On Unix, encode `ps_create_time() - `ps_boot_time()`

  pid  <- ps_pid(p)
  time <- as.numeric(ps_create_time(p))

  if (.Platform$OS.type == "unix")
    time <- time - as.numeric(ps_boot_time())

  time <- round(time, 3) * 1000 # millisecond resolution

  map <- c(letters, LETTERS, 0:9)
  paste(
    collapse = '',
    map[
      1 +
        c(
          floor(pid  / 62^(3:0)) %% 62,
          floor(time / 62^(7:0)) %% 62
        )
    ]
  )
}


ps__str_decode <- function(str) {

  map <- structure(0:61, names = c(letters, LETTERS, 0:9))
  val <- map[strsplit(str, '', fixed = TRUE)[[1]]]
  pid <- sum(val[01:04] * 62^(3:0))

  tryCatch(
    expr = {
      p <- ps_handle(pid = pid)
      stopifnot(str == ps__str_encode(p))
      p
    },
    error = function(e) {

      time <- sum(val[05:12] * 62^(7:0)) / 1000

      if (.Platform$OS.type == "unix")
        time <- time + as.numeric(ps_boot_time())

      ps_handle(pid = pid, time = format_unix_time(time))
    }
  )
}
