run_r <- function(options) {
  oldwd <- getwd()
  setwd(options$wd)
  on.exit(setwd(oldwd), add = TRUE)

  pty <- isTRUE(options$extra$pty)

  if (pty && !is.null(options$stderr)) {
    stop(
      "`stderr` cannot be set when `pty = TRUE`; ",
      "stdout and stderr are merged into the pty stream"
    )
  }

  ## We redirect stderr to stdout if either of these are true:
  ## - stderr is the string "2>&1"
  ## - both stdout and stderr are non-null, and they are the same
  stderr_to_stdout <- with(
    options,
    (!is.null(stderr) && stderr == "2>&1") ||
      (!is.null(stdout) && !is.null(stderr) && stdout == stderr)
  )

  otel::log_debug("callr start subprocess")

  res <- with(
    options,
    with_envvar(
      env,
      do.call(
        processx::run,
        c(
          list(
            bin,
            args = real_cmdargs,
            stdout_line_callback = real_callback(stdout),
            stdout_callback = real_block_callback,
            echo_cmd = echo,
            echo = show,
            spinner = spinner,
            error_on_status = fail_on_status,
            timeout = timeout
          ),
          if (!pty) {
            list(
              stderr_line_callback = real_callback(stderr),
              stderr_callback = real_block_callback,
              stderr_to_stdout = stderr_to_stdout
            )
          },
          extra
        )
      )
    )
  )

  res$command <- c(options$bin, options$real_cmdargs)
  res
}
