
# processx

> Execute and Control System Processes

<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/r-lib/processx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/processx/actions/workflows/R-CMD-check.yaml)
[![](https://www.r-pkg.org/badges/version/processx)](https://www.r-pkg.org/pkg/processx)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/processx)](https://www.r-pkg.org/pkg/processx)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/processx/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/processx?branch=main)
<!-- badges: end -->

Tools to run system processes in the background, read their standard
output and error and kill them.

processx can poll the standard output and error of a single process, or
multiple processes, using the operating system’s polling and waiting
facilities, with a timeout.

------------------------------------------------------------------------

-   [Features](#features)
-   [Installation](#installation)
-   [Usage](#usage)
    -   [Running an external process](#running-an-external-process)
        -   [Errors](#errors)
        -   [Showing output](#showing-output)
        -   [Spinner](#spinner)
        -   [Callbacks for I/O](#callbacks-for-io)
    -   [Managing external processes](#managing-external-processes)
        -   [Starting processes](#starting-processes)
        -   [Killing a process](#killing-a-process)
        -   [Standard output and error](#standard-output-and-error)
        -   [End of output](#end-of-output)
        -   [Polling the standard output and
            error](#polling-the-standard-output-and-error)
        -   [Polling multiple processes](#polling-multiple-processes)
        -   [Waiting on a process](#waiting-on-a-process)
        -   [Exit statuses](#exit-statuses)
        -   [Mixing processx and the parallel base R
            package](#mixing-processx-and-the-parallel-base-r-package)
        -   [Errors](#errors-1)
-   [Related tools](#related-tools)
-   [Code of Conduct](#code-of-conduct)
-   [License](#license)

## Features

-   Start system processes in the background and find their process id.
-   Read the standard output and error, using non-blocking connections
-   Poll the standard output and error connections of a single process
    or multiple processes.
-   Write to the standard input of background processes.
-   Check if a background process is running.
-   Wait on a background process, or multiple processes, with a timeout.
-   Get the exit status of a background process, if it has already
    finished.
-   Kill background processes.
-   Kill background process, when its associated object is garbage
    collected.
-   Kill background processes and all their child processes.
-   Works on Linux, macOS and Windows.
-   Lightweight, it only depends on the also lightweight R6 and ps
    packages.

## Installation

Install the stable version from CRAN:

``` r
install.packages("processx")
```

## Usage

``` r
library(processx)
```

> Note: the following external commands are usually present in macOS and
> Linux systems, but not necessarily on Windows. We will also use the
> `px` command line tool (`px.exe` on Windows), that is a very simple
> program that can produce output to `stdout` and `stderr`, with the
> specified timings.

``` r
px <- paste0(
  system.file(package = "processx", "bin", "px"),
  system.file(package = "processx", "bin", .Platform$r_arch, "px.exe")
)
px
```

    #> [1] "/Users/gaborcsardi/Library/R/arm64/4.2/library/processx/bin/px"

### Running an external process

The `run()` function runs an external command. It requires a single
command, and a character vector of arguments. You don’t need to quote
the command or the arguments, as they are passed directly to the
operating system, without an intermediate shell.

``` r
run("echo", "Hello R!")
```

    #> $status
    #> [1] 0
    #> 
    #> $stdout
    #> [1] "Hello R!\n"
    #> 
    #> $stderr
    #> [1] ""
    #> 
    #> $timeout
    #> [1] FALSE

Short summary of the `px` binary we are using extensively below:

``` r
result <- run(px, "--help", echo = TRUE)
```

    #> Usage: px [command arg] [command arg] ...
    #> 
    #> Commands:
    #>   sleep  <seconds>           -- sleep for a number os seconds
    #>   out    <string>            -- print string to stdout
    #>   err    <string>            -- print string to stderr
    #>   outln  <string>            -- print string to stdout, add newline
    #>   errln  <string>            -- print string to stderr, add newline
    #>   errflush                   -- flush stderr stream
    #>   cat    <filename>          -- print file to stdout
    #>   return <exitcode>          -- return with exitcode
    #>   writefile <path> <string>  -- write to file
    #>   write <fd> <string>        -- write to file descriptor
    #>   echo <fd1> <fd2> <nbytes>  -- echo from fd to another fd
    #>   getenv <var>               -- environment variable to stdout

> Note: From version 3.0.1, processx does not let you specify a full
> shell command line, as this involves starting a grandchild process
> from the child process, and it is difficult to clean up the grandchild
> process when the child process is killed. The user can still start a
> shell (`sh` or `cmd.exe`) directly of course, and then proper cleanup
> is the user’s responsibility.

#### Errors

By default `run()` throws an error if the process exits with a non-zero
status code. To avoid this, specify `error_on_status = FALSE`:

``` r
run(px, c("out", "oh no!", "return", "2"), error_on_status = FALSE)
```

    #> $status
    #> [1] 2
    #> 
    #> $stdout
    #> [1] "oh no!"
    #> 
    #> $stderr
    #> [1] ""
    #> 
    #> $timeout
    #> [1] FALSE

#### Showing output

To show the output of the process on the screen, use the `echo`
argument. Note that the order of `stdout` and `stderr` lines may be
incorrect, because they are coming from two different connections.

``` r
result <- run(px,
  c("outln", "out", "errln", "err", "outln", "out again"),
  echo = TRUE)
```

    #> out
    #> out again
    #> err

If you have a terminal that support ANSI colors, then the standard error
output is shown in red.

The standard output and error are still included in the result of the
`run()` call:

``` r
result
```

    #> $status
    #> [1] 0
    #> 
    #> $stdout
    #> [1] "out\nout again\n"
    #> 
    #> $stderr
    #> [1] "err\n"
    #> 
    #> $timeout
    #> [1] FALSE

Note that `run()` is different from `system()`, and it always shows the
output of the process on R’s proper standard output, instead of writing
to the terminal directly. This means for example that you can capture
the output with `capture.output()` or use `sink()`, etc.:

``` r
out1 <- capture.output(r1 <- system("ls"))
out2 <- capture.output(r2 <- run("ls", echo = TRUE))
```

``` r
out1
```

    #> character(0)

``` r
out2
```

    #>  [1] "CODE_OF_CONDUCT.md" "DESCRIPTION"        "LICENSE"           
    #>  [4] "LICENSE.md"         "Makefile"           "NAMESPACE"         
    #>  [7] "NEWS.md"            "R"                  "README.Rmd"        
    #> [10] "README.md"          "_pkgdown.yml"       "codecov.yml"       
    #> [13] "inst"               "man"                "processx.Rproj"    
    #> [16] "src"                "tests"

#### Spinner

The `spinner` option of `run()` puts a calming spinner to the terminal
while the background program is running. The spinner is always shown in
the first character of the last line, so you can make it work nicely
with the regular output of the background process if you like. E.g. try
this in your R terminal:

    result <- run(px,
      c("out", "  foo",
        "sleep", "1",
        "out", "\r  bar",
        "sleep", "1",
        "out", "\rX foobar\n"),
      echo = TRUE, spinner = TRUE)

#### Callbacks for I/O

`run()` can call an R function for each line of the standard output or
error of the process, just supply the `stdout_line_callback` or the
`stderr_line_callback` arguments. The callback functions take two
arguments, the first one is a character scalar, the output line. The
second one is the `process` object that represents the background
process. (See more below about `process` objects.) You can manipulate
this object in the callback, if you want. For example you can kill it in
response to an error or some text on the standard output:

``` r
cb <- function(line, proc) {
  cat("Got:", line, "\n")
  if (line == "done") proc$kill()
}
result <- run(px,
  c("outln", "this", "outln", "that", "outln", "done",
    "outln", "still here", "sleep", "10", "outln", "dead by now"), 
  stdout_line_callback = cb,
  error_on_status = FALSE,
)
```

    #> Got: this 
    #> Got: that 
    #> Got: done 
    #> Got: still here

``` r
result
```

    #> $status
    #> [1] -9
    #> 
    #> $stdout
    #> [1] "this\nthat\ndone\nstill here\n"
    #> 
    #> $stderr
    #> [1] ""
    #> 
    #> $timeout
    #> [1] FALSE

Keep in mind, that while the R callback is running, the background
process is not stopped, it is also running. In the previous example,
whether `still here` is printed or not depends on the scheduling of the
R process and the background process by the OS. Typically, it is
printed, because the R callback takes a while to run.

In addition to the line-oriented callbacks, the `stdout_callback` and
`stderr_callback` arguments can specify callback functions that are
called with output chunks instead of single lines. A chunk may contain
multiple lines (separated by `\n` or `\r\n`), or even incomplete lines.

### Managing external processes

If you need better control over possibly multiple background processes,
then you can use the R6 `process` class directly.

#### Starting processes

To start a new background process, create a new instance of the
`process` class.

``` r
p <- process$new("sleep", "20")
```

#### Killing a process

A process can be killed via the `kill()` method.

``` r
p$is_alive()
```

    #> [1] TRUE

``` r
p$kill()
```

    #> [1] TRUE

``` r
p$is_alive()
```

    #> [1] FALSE

Note that processes are finalized (and killed) automatically if the
corresponding `process` object goes out of scope, as soon as the object
is garbage collected by R:

``` r
p <- process$new("sleep", "20")
rm(p)
invisible(gc())
```

Here, the direct call to the garbage collector kills the `sleep` process
as well. See the `cleanup` option if you want to avoid this behavior.

#### Standard output and error

By default the standard output and error of the processes are ignored.
You can set the `stdout` and `stderr` constructor arguments to a file
name, and then they are redirected there, or to `"|"`, and then processx
creates connections to them. (Note that starting from processx 3.0.0
these connections are not regular R connections, because the public R
connection API was retroactively removed from R.)

The `read_output_lines()` and `read_error_lines()` methods can be used
to read complete lines from the standard output or error connections.
They work similarly to the `readLines()` base R function.

Note, that the connections have a buffer, which can fill up, if R does
not read out the output, and then the process will stop, until R reads
the connection and the buffer is freed.

> **Always make sure that you read out the standard output and/or
> error** **of the pipes, otherwise the background process will stop
> running!**

If you don’t need the standard output or error any more, you can also
close it, like this:

``` r
close(p$get_output_connection())
close(p$get_error_connection())
```

Note that the connections used for reading the output and error streams
are non-blocking, so the read functions will return immediately, even if
there is no text to read from them. If you want to make sure that there
is data available to read, you need to poll, see below.

``` r
p <- process$new(px,
  c("sleep", "1", "outln", "foo", "errln", "bar", "outln", "foobar"),
  stdout = "|", stderr = "|")
p$read_output_lines()
```

    #> character(0)

``` r
p$read_error_lines()
```

    #> character(0)

#### End of output

The standard R way to query the end of the stream for a non-blocking
connection, is to use the `isIncomplete()` function. *After a read
attempt*, this function returns `FALSE` if the connection has surely no
more data. (If the read attempt returns no data, but `isIncomplete()`
returns `TRUE`, then the connection might deliver more data in the
future.

The `is_incomplete_output()` and `is_incomplete_error()` functions work
similarly for `process` objects.

#### Polling the standard output and error

The `poll_io()` method waits for data on the standard output and/or
error of a process. It will return if any of the following events
happen:

-   data is available on the standard output of the process (assuming
    there is a connection to the standard output).
-   data is available on the standard error of the process (assuming the
    is a connection to the standard error).
-   The process has finished and the standard output and/or error
    connections were closed on the other end.
-   The specified timeout period expired.

For example the following code waits about a second for output.

``` r
p <- process$new(px, c("sleep", "1", "outln", "kuku"), stdout = "|")

## No output yet
p$read_output_lines()
```

    #> character(0)

``` r
## Wait at most 5 sec
p$poll_io(5000)
```

    #>   output    error  process 
    #>  "ready" "nopipe" "nopipe"

``` r
## There is output now
p$read_output_lines()
```

    #> [1] "kuku"

#### Polling multiple processes

If you need to manage multiple background processes, and need to wait
for output from all of them, processx defines a `poll()` function that
does just that. It is similar to the `poll_io()` method, but it takes
multiple process objects, and returns as soon as one of them have data
on standard output or error, or a timeout expires. Here is an example:

``` r
p1 <- process$new(px, c("sleep", "1", "outln", "output"), stdout = "|")
p2 <- process$new(px, c("sleep", "2", "errln", "error"), stderr = "|")

## After 100ms no output yet
poll(list(p1 = p1, p2 = p2), 100)
```

    #> $p1
    #>    output     error   process 
    #> "timeout"  "nopipe"  "nopipe" 
    #> 
    #> $p2
    #>    output     error   process 
    #>  "nopipe" "timeout"  "nopipe"

``` r
## But now we surely have something
poll(list(p1 = p1, p2 = p2), 1000)
```

    #> $p1
    #>   output    error  process 
    #>  "ready" "nopipe" "nopipe" 
    #> 
    #> $p2
    #>   output    error  process 
    #> "nopipe" "silent" "nopipe"

``` r
p1$read_output_lines()
```

    #> [1] "output"

``` r
## Done with p1
close(p1$get_output_connection())
```

    #> NULL

``` r
## The second process should have data on stderr soonish
poll(list(p1 = p1, p2 = p2), 5000)
```

    #> $p1
    #>   output    error  process 
    #> "closed" "nopipe" "nopipe" 
    #> 
    #> $p2
    #>   output    error  process 
    #> "nopipe"  "ready" "nopipe"

``` r
p2$read_error_lines()
```

    #> [1] "error"

#### Waiting on a process

As seen before, `is_alive()` checks if a process is running. The
`wait()` method can be used to wait until it has finished (or a
specified timeout expires).. E.g. in the following code `wait()` needs
to wait about 2 seconds for the `sleep` `px` command to finish.

``` r
p <- process$new(px, c("sleep", "2"))
p$is_alive()
```

    #> [1] TRUE

``` r
Sys.time()
```

    #> [1] "2022-06-10 13:57:49 CEST"

``` r
p$wait()
Sys.time()
```

    #> [1] "2022-06-10 13:57:51 CEST"

It is safe to call `wait()` multiple times:

``` r
p$wait() # already finished!
```

#### Exit statuses

After a process has finished, its exit status can be queried via the
`get_exit_status()` method. If the process is still running, then this
method returns `NULL`.

``` r
p <- process$new(px, c("sleep", "2"))
p$get_exit_status()
```

    #> NULL

``` r
p$wait()
p$get_exit_status()
```

    #> [1] 0

#### Mixing processx and the parallel base R package

In general, mixing processx (via callr or not) and parallel works fine.
If you use parallel’s ‘fork’ clusters, e.g. via
`parallel::mcparallel()`, then you might see two issues. One is that
processx will not be able to determine the exit status of some processx
processes. This is because the status is read out by parallel, and
processx will set it to `NA`. The other one is that parallel might
complain that it could not clean up some subprocesses. This is not an
error, and it is harmless, but it does hold up R for about 10 seconds,
before parallel gives up. To work around this, you can set the
`PROCESSX_NOTIFY_OLD_SIGCHLD` environment variable to a non-empty value,
before you load processx. This behavior might be the default in the
future.

#### Errors

Errors are typically signalled via non-zero exits statuses. The processx
constructor fails if the external program cannot be started, but it does
not deal with errors that happen after the program has successfully
started running.

``` r
p <- process$new("nonexistant-command-for-sure")
```

    #> Error in c("process_initialize(self, private, command, args, stdin, stdout, ", : ! Native call to `processx_exec` failed
    #> Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, …` at initialize.R:138:3:
    #> ! cannot start processx process 'nonexistant-command-for-sure' (system error 2, No such file or directory) @unix/processx.c:613 (processx_exec)

``` r
p2 <- process$new(px, c("sleep", "1", "command-does-not-exist"))
p2$wait()
p2$get_exit_status()
```

    #> [1] 5

## Related tools

-   The [`ps` package](https://ps.r-lib.org/) can query, list,
    manipulate all system processes (not just subprocesses), and
    processx uses it internally for some of its functionality. You can
    also convert a `processx::process` object to a `ps::ps_handle` with
    the `as_ps_handle()` method.

-   The [`callr` package](https://callr.r-lib.org/) uses processx to
    start another R process, and run R code in it, in the foreground or
    background.

## Code of Conduct

Please note that the processx project is released with a [Contributor
Code of Conduct](https://processx.r-lib.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## License

MIT © Mango Solutions, RStudio, Gábor Csárdi
