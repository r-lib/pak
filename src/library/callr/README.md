

<!-- README.md is generated from README.Rmd. Please edit that file -->

# callr

> Call R from R

<!-- badges: start -->
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/r-lib/callr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/callr/actions/workflows/R-CMD-check.yaml)
[![](https://www.r-pkg.org/badges/version/callr)](https://www.r-pkg.org/pkg/callr)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/callr)](https://www.r-pkg.org/pkg/callr)
[![Codecov test coverage](https://codecov.io/gh/r-lib/callr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/callr?branch=main)
<!-- badges: end -->


It is sometimes useful to perform a computation in a separate R process,
without affecting the current R process at all. This packages does exactly
that.

---

-   <a href="#features" id="toc-features">Features</a>
-   <a href="#installation" id="toc-installation">Installation</a>
-   <a href="#synchronous-one-off-r-processes"
    id="toc-synchronous-one-off-r-processes">Synchronous, one-off R
    processes</a>
    -   <a href="#passing-arguments" id="toc-passing-arguments">Passing
        arguments</a>
    -   <a href="#using-packages" id="toc-using-packages">Using packages</a>
    -   <a href="#error-handling" id="toc-error-handling">Error handling</a>
    -   <a href="#standard-output-and-error"
        id="toc-standard-output-and-error">Standard output and error</a>
-   <a href="#background-r-processes"
    id="toc-background-r-processes">Background R processes</a>
-   <a href="#multiple-background-r-processes-and-poll"
    id="toc-multiple-background-r-processes-and-poll">Multiple background R
    processes and <code>poll()</code></a>
-   <a href="#persistent-r-sessions"
    id="toc-persistent-r-sessions">Persistent R sessions</a>
-   <a href="#running-r-cmd-commands"
    id="toc-running-r-cmd-commands">Running <code>R CMD</code> commands</a>
-   <a href="#code-of-conduct" id="toc-code-of-conduct">Code of Conduct</a>
-   <a href="#license" id="toc-license">License</a>

## Features

-   Calls an R function, with arguments, in a subprocess.
-   Copies function arguments to the subprocess and copies the return
    value of the function back, seamlessly.
-   Copies error objects back from the subprocess, including a stack
    trace.
-   Shows and/or collects the standard output and standard error of the
    subprocess.
-   Supports both one-off and persistent R subprocesses.
-   Calls the function synchronously or asynchronously (in the
    background).
-   Can call `R CMD` commands, synchronously or asynchronously.
-   Can call R scripts, synchronously or asynchronously.
-   Provides extensible `r_process`, `rcmd_process` and
    `rscript_process` R6 classes, based on `processx::process`.

## Installation

Install the stable version from CRAN:

``` r
install.packages("callr")
```

## Synchronous, one-off R processes

Use `r()` to run an R function in a new R process. The results are
passed back seamlessly:

``` r
callr::r(function() var(iris[, 1:4]))
```

![](man/figures/simple.svg)<!-- -->

### Passing arguments

You can pass arguments to the function by setting `args` to the list of
arguments. This is often necessary as these arguments are explicitly
copied to the child process, whereas the evaluated function cannot refer
to variables in the parent. For example, the following does not work:

``` r
mycars <- cars
callr::r(function() summary(mycars))
```

![](man/figures/passargsfail.svg)<!-- -->

But this does:

``` r
mycars <- cars
callr::r(function(x) summary(x), args = list(mycars))
```

![](man/figures/passargsok.svg)<!-- -->

Note that the arguments will be serialized and saved to a file, so if
they are large R objects, it might take a long time for the child
process to start up.

### Using packages

You can use any R package in the child process, just make sure to refer
to it explicitly with the `::` operator. For example, the following code
creates an [igraph](https://github.com/igraph/rigraph) graph in the
child, and calculates some metrics of it.

``` r
callr::r(function() { g <- igraph::sample_gnp(1000, 4/1000); igraph::diameter(g) })
```

![](man/figures/packages.svg)<!-- -->

### Error handling

callr copies errors from the child process back to the main R session:

``` r
callr::r(function() 1 + "A")
```

![](man/figures/error1.svg)<!-- --> callr sets the `.Last.error`
variable, and after an error you can inspect this for more details about
the error, including stack traces both from the main R process and the
subprocess.

``` r
.Last.error
```

![](man/figures/error2-2.svg)<!-- -->

The error objects has two parts. The first belongs to the main process,
and the second belongs to the subprocess.

`.Last.error` also includes a stack trace, that includes both the main R
process and the subprocess:

The top part of the trace contains the frames in the main process, and
the bottom part contains the frames in the subprocess, starting with the
anonymous function.

### Standard output and error

By default, the standard output and error of the child is lost, but you
can request callr to redirect them to files, and then inspect the files
in the parent:

``` r
x <- callr::r(function() { print("hello world!"); message("hello again!") },
  stdout = "/tmp/out", stderr = "/tmp/err"
)
readLines("/tmp/out")
```

![](man/figures/io.svg)<!-- -->

``` r
readLines("/tmp/err")
```

![](man/figures/io-2.svg)<!-- -->

With the `stdout` option, the standard output is collected and can be
examined once the child process finished. The `show = TRUE` options will
also show the output of the child, as it is printed, on the console of
the parent.

## Background R processes

`r_bg()` is similar to `r()` but it starts the R process in the
background. It returns an `r_process` R6 object, that provides a rich
API:

``` r
rp <- callr::r_bg(function() Sys.sleep(.2))
rp
```

![](man/figures/bg.svg)<!-- -->

This is a list of all `r_process` methods:

``` r
ls(rp)
```

![](man/figures/bg-methods.svg)<!-- -->

These include all methods of the `processx::process` superclass and the
new `get_result()` method, to retrieve the R object returned by the
function call. Some of the handiest methods are:

-   `get_exit_status()` to query the exit status of a finished process.
-   `get_result()` to collect the return value of the R function call.
-   `interrupt()` to send an interrupt to the process. This is
    equivalent to a `CTRL+C` key press, and the R process might ignore
    it.
-   `is_alive()` to check if the process is alive.
-   `kill()` to terminate the process.
-   `poll_io()` to wait for any standard output, standard error, or the
    completion of the process, with a timeout.
-   `read_*()` to read the standard output or error.
-   `suspend()` and `resume()` to stop and continue a process.
-   `wait()` to wait for the completion of the process, with a timeout.

## Multiple background R processes and `poll()`

Multiple background R processes are best managed with the
`processx::poll()` function that waits for events (standard output/error
or termination) from multiple processes. It returns as soon as one
process has generated an event, or if its timeout has expired. The
timeout is in milliseconds.

``` r
rp1 <- callr::r_bg(function() { Sys.sleep(1/2); "1 done" })
rp2 <- callr::r_bg(function() { Sys.sleep(1/1000); "2 done" })
processx::poll(list(rp1, rp2), 1000)
```

![](man/figures/poll.svg)<!-- -->

``` r
rp2$get_result()
```

![](man/figures/poll-2.svg)<!-- -->

``` r
processx::poll(list(rp1), 1000)
```

![](man/figures/poll-3.svg)<!-- -->

``` r
rp1$get_result()
```

![](man/figures/poll-4.svg)<!-- -->

## Persistent R sessions

`r_session` is another `processx::process` subclass that represents a
persistent background R session:

``` r
rs <- callr::r_session$new()
rs
```

![](man/figures/rsession.svg)<!-- -->

`r_session$run()` is a synchronous call, that works similarly to `r()`,
but uses the persistent session. `r_session$call()` starts the function
call and returns immediately. The `r_session$poll_process()` method or
`processx::poll()` can then be used to wait for the completion or other
events from one or more R sessions, R processes or other
`processx::process` objects.

Once an R session is done with an asynchronous computation, its
`poll_process()` method returns `"ready"` and the `r_session$read()`
method can read out the result.

``` r
rs <- callr::r_session$new()
rs$run(function() runif(10))
```

![](man/figures/rsession2.svg)<!-- -->

``` r
rs$call(function() rnorm(10))
rs
```

![](man/figures/rsession2-2.svg)<!-- -->

``` r
rs$poll_process(2000)
```

![](man/figures/rsession-4.svg)<!-- -->

``` r
rs$read()
```

![](man/figures/rsession-5.svg)<!-- -->

## Running `R CMD` commands

The `rcmd()` function calls an `R CMD` command. For example, you can
call `R CMD INSTALL`, `R CMD check` or `R CMD config` this way:

``` r
callr::rcmd("config", "CC")
```

![](man/figures/rcmd.svg)<!-- -->

This returns a list with three components: the standard output, the
standard error, and the exit (status) code of the `R CMD` command.

## Code of Conduct

Please note that the callr project is released with a [Contributor Code
of Conduct](https://callr.r-lib.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## License

MIT © Mango Solutions, RStudio
