
run_r <- function(options) {

  oldwd <- getwd()
  setwd(options$wd)
  on.exit(setwd(oldwd), add = TRUE)

  ## We redirect stderr to stdout if either of these are true:
  ## - stderr is the string "2>&1"
  ## - both stdout and stderr are non-null, and they are the same
  stderr_to_stdout <- with(
    options,
    (!is.null(stderr) && stderr == "2>&1") ||
    (!is.null(stdout) && !is.null(stderr) && stdout == stderr)
  )

  res <- with(
    options,
    with_envvar(
      env,
      do.call(processx::run, c(list(
        bin, args = real_cmdargs,
        stdout_line_callback = real_callback(stdout),
        stderr_line_callback = real_callback(stderr),
        stdout_callback = real_block_callback,
        stderr_callback = real_block_callback,
        stderr_to_stdout = stderr_to_stdout,
        echo_cmd = echo, echo = show, spinner = spinner,
        error_on_status = fail_on_status, timeout = timeout),
        extra)
      )
    )
  )

  res$command <- c(options$bin, options$real_cmdargs)
  res
}
