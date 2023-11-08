
# ps

> List, Query, Manipulate System Processes

<!-- badges: start -->
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/r-lib/ps/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/ps/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/ps)](https://cran.r-project.org/package=ps)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/ps)](https://www.r-pkg.org/pkg/ps)
[![Codecov test coverage](https://codecov.io/gh/r-lib/ps/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/ps?branch=main)
<!-- badges: end -->

ps implements an API to query and manipulate system processes. Most of its
code is based on the [psutil](https://github.com/giampaolo/psutil) Python
package.


-   [Installation](#installation)
-   [Supported platforms](#supported-platforms)
-   [Listing all processes](#listing-all-processes)
-   [Process API](#process-api)
    -   [Query functions](#query-functions)
    -   [Process manipulation](#process-manipulation)
-   [Finished and zombie processes](#finished-and-zombie-processes)
-   [Pid reuse](#pid-reuse)
-   [Recipes](#recipes)
    -   [Find process by name](#find-process-by-name)
    -   [Wait for a process to finish](#wait-for-a-process-to-finish)
    -   [Wait for several processes to
        finish](#wait-for-several-processes-to-finish)
    -   [Kill process tree](#kill-process-tree)
    -   [Terminate children](#terminate-children)
    -   [Filtering and sorting
        processes](#filtering-and-sorting-processes)
-   [Code of Conduct](#code-of-conduct)
-   [License](#license)

## Installation

You can install the released version of ps from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ps")
```

``` r
library(ps)
library(pillar) # nicer printing of data frames
```

## Supported platforms

ps currently supports Windows (from Vista), macOS and Linux systems. On
unsupported platforms the package can be installed and loaded, but all
of its functions fail with an error of class `"not_implemented"`.

## Listing all processes

`ps_pids()` returns all process ids on the system. This can be useful to
iterate over all processes.

``` r
ps_pids()[1:20]
```

    ##  [1]   0   1 299 300 302 303 304 306 308 310 311 314 316 319 323 324 325 326 329 330

`ps()` returns a data frame, with data about each process. It contains a
handle to each process, in the `ps_handle` column, you can use these to
perform more queries on the processes.

``` r
ps()
```

    ## # A data frame: 445 × 11
    ##      pid  ppid name           username status     user   system     rss      vms created            
    ##  * <int> <int> <chr>          <chr>    <chr>     <dbl>    <dbl>   <dbl>    <dbl> <dttm>             
    ##  1 28286 24879 R              gaborcs… runni…  9.29e-3  1.29e-3  1.39e8  4.19e11 2022-06-17 11:08:04
    ##  2 28115   552 Google Chrome… gaborcs… runni…  1.19e-3  3.95e-4  6.32e7  4.61e11 2022-06-17 11:07:01
    ##  3 27238     1 com.apple.iCl… gaborcs… runni…  1.94e-3  1.13e-3  8.80e6  4.18e11 2022-06-17 11:03:47
    ##  4 27008   552 Google Chrome… gaborcs… runni…  1.15e-2  2.54e-3  1.11e8  4.61e11 2022-06-17 11:03:08
    ##  5 25835 25439 R              gaborcs… runni…  2.49e-2  6.02e-3  1.45e8  4.19e11 2022-06-17 10:57:41
    ##  6 25771 25439 zsh            gaborcs… runni…  3.11e-5  1.55e-4  3.42e6  4.19e11 2022-06-17 10:57:40
    ##  7 25439 25438 zsh            gaborcs… runni…  2.48e-3  1.30e-3  1.39e7  4.19e11 2022-06-17 10:57:39
    ##  8 25438   635 login          gaborcs… runni… NA       NA       NA      NA       2022-06-17 10:57:39
    ##  9 25386 24879 Emacs-arm64-1… gaborcs… runni…  9.98e-1  1.52e-1  3.07e8  4.20e11 2022-06-17 10:57:30
    ## 10 25211 24879 zsh            gaborcs… runni…  6.82e-4  2.85e-3  3.54e6  4.19e11 2022-06-17 10:57:08
    ## # … with 435 more rows, and 1 more variable: ps_handle <I<list>>

## Process API

This is a short summary of the API. Please see the documentation of the
various methods for details, in particular regarding handles to finished
processes and pid reuse. See also “Finished and zombie processes” and
“pid reuse” below.

`ps_handle(pid)` creates a process handle for the supplied process id.
If `pid` is omitted, a handle to the calling process is returned:

``` r
p <- ps_handle()
p
```

    ## <ps::ps_handle> PID=28286, NAME=R, AT=2022-06-17 11:08:04

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 28286

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2022-06-17 11:08:04 GMT"

The process id and the creation time uniquely identify a process in a
system. ps uses them to make sure that it reports information about, and
manipulates the correct process.

`ps_is_running(p)` returns whether `p` is still running. It handles pid
reuse safely.

``` r
ps_is_running(p)
```

    ## [1] TRUE

`ps_ppid(p)` returns the pid of the parent of `p`.

``` r
ps_ppid(p)
```

    ## [1] 24879

`ps_parent(p)` returns a process handle to the parent process of `p`.

``` r
ps_parent(p)
```

    ## <ps::ps_handle> PID=24879, NAME=zsh, AT=2022-06-17 10:57:07

`ps_name(p)` returns the name of the program `p` is running.

``` r
ps_name(p)
```

    ## [1] "R"

`ps_exe(p)` returns the full path to the executable the `p` is running.

``` r
ps_exe(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/bin/exec/R"

`ps_cmdline(p)` returns the command line (executable and arguments) of
`p`.

``` r
ps_cmdline(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/bin/exec/R"
    ## [2] "--no-echo"                                                              
    ## [3] "--no-restore"                                                           
    ## [4] "-e"                                                                     
    ## [5] "rmarkdown::render(\"README.Rmd\")"

`ps_status(p)` returns the status of the process. Possible values are OS
dependent, but typically there is `"running"` and `"stopped"`.

``` r
ps_status(p)
```

    ## [1] "running"

`ps_username(p)` returns the name of the user the process belongs to.

``` r
ps_username(p)
```

    ## [1] "gaborcsardi"

`ps_uids(p)` and `ps_gids(p)` return the real, effective and saved user
ids of the process. They are only implemented on POSIX systems.

``` r
if (ps_os_type()[["POSIX"]]) ps_uids(p)
```

    ##      real effective     saved 
    ##       501       501       501

``` r
if (ps_os_type()[["POSIX"]]) ps_gids(p)
```

    ##      real effective     saved 
    ##        20        20        20

`ps_cwd(p)` returns the current working directory of the process.

``` r
ps_cwd(p)
```

    ## [1] "/Users/gaborcsardi/works/ps"

`ps_terminal(p)` returns the name of the terminal of the process, if
any. For processes without a terminal, and on Windows it returns
`NA_character_`.

``` r
ps_terminal(p)
```

    ## [1] "/dev/ttys006"

`ps_environ(p)` returns the environment variables of the process.
`ps_environ_raw(p)` does the same, in a different form. Typically they
reflect the environment variables at the start of the process.

``` r
ps_environ(p)[c("TERM", "USER", "SHELL", "R_HOME")]
```

    ## TERM                          xterm-256color
    ## USER                          gaborcsardi
    ## SHELL                         /bin/zsh
    ## R_HOME                        /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources

`ps_num_threads(p)` returns the current number of threads of the
process.

``` r
ps_num_threads(p)
```

    ## [1] 3

`ps_cpu_times(p)` returns the CPU times of the process, similarly to
`proc.time()`.

``` r
ps_cpu_times(p)
```

    ##            user          system   children_user children_system 
    ##     0.010692116     0.001444821              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##          rss          vms      pfaults      pageins 
    ##    148783104 419159621632         9884            0

`ps_children(p)` lists all child processes (potentially recursively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=25211, NAME=zsh, AT=2022-06-17 10:57:08
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=25386, NAME=Emacs-arm64-11_2, AT=2022-06-17 10:57:30
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=28286, NAME=R, AT=2022-06-17 11:08:04

`ps_num_fds(p)` returns the number of open file descriptors (handles on
Windows):

``` r
ps_num_fds(p)
```

    ## [1] 4

``` r
f <- file(tmp <- tempfile(), "w")
ps_num_fds(p)
```

    ## [1] 5

``` r
close(f)
unlink(tmp)
```

`ps_open_files(p)` lists all open files:

``` r
ps_open_files(p)
```

    ## # A data frame: 4 × 2
    ##      fd path                                                                       
    ##   <int> <chr>                                                                      
    ## 1     0 /dev/ttys006                                                               
    ## 2     1 /dev/ttys006                                                               
    ## 3     2 /dev/ttys006                                                               
    ## 4     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/Rscript6e7e.sDuQan

``` r
f <- file(tmp <- tempfile(), "w")
ps_open_files(p)
```

    ## # A data frame: 5 × 2
    ##      fd path                                                                                
    ##   <int> <chr>                                                                               
    ## 1     0 /dev/ttys006                                                                        
    ## 2     1 /dev/ttys006                                                                        
    ## 3     2 /dev/ttys006                                                                        
    ## 4     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/Rscript6e7e.sDuQan         
    ## 5     4 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpBQnIMy/file6e7e757f921d

``` r
close(f)
unlink(tmp)
ps_open_files(p)
```

    ## # A data frame: 4 × 2
    ##      fd path                                                                       
    ##   <int> <chr>                                                                      
    ## 1     0 /dev/ttys006                                                               
    ## 2     1 /dev/ttys006                                                               
    ## 3     2 /dev/ttys006                                                               
    ## 4     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/Rscript6e7e.sDuQan

### Process manipulation

`ps_suspend(p)` suspends (stops) the process. On POSIX it sends a
SIGSTOP signal. On Windows it stops all threads.

`ps_resume(p)` resumes the process. On POSIX it sends a SIGCONT signal.
On Windows it resumes all stopped threads.

`ps_send_signal(p)` sends a signal to the process. It is implemented on
POSIX systems only. It makes an effort to work around pid reuse.

`ps_terminate(p)` send SIGTERM to the process. On POSIX systems only.

`ps_kill(p)` terminates the process. Sends `SIGKILL` on POSIX systems,
uses `TerminateProcess()` on Windows. It make an effort to work around
pid reuse.

`ps_interrupt(p)` interrupts a process. It sends a `SIGINT` signal on
POSIX systems, and it can send a CTRL+C or a CTRL+BREAK event on
Windows.

## Finished and zombie processes

ps handles finished and Zombie processes as much as possible.

The essential `ps_pid()`, `ps_create_time()`, `ps_is_running()`
functions and the `format()` and `print()` methods work for all
processes, including finished and zombie processes. Other functions fail
with an error of class `"no_such_process"` for finished processes.

The `ps_ppid()`, `ps_parent()`, `ps_children()`, `ps_name()`,
`ps_status()`, `ps_username()`, `ps_uids()`, `ps_gids()`,
`ps_terminal()`, `ps_children()` and the signal sending functions work
properly for zombie processes. Other functions fail with
`"zombie_process"` error.

## Pid reuse

ps functions handle pid reuse as well as technically possible.

The query functions never return information about the wrong process,
even if the process has finished and its process id was re-assigned.

On Windows, the process manipulation functions never manipulate the
wrong process.

On POSIX systems, this is technically impossible, it is not possible to
send a signal to a process without creating a race condition. In ps the
time window of the race condition is very small, a few microseconds, and
the process would need to finish, *and* the OS would need to reuse its
pid within this time window to create problems. This is very unlikely to
happen.

## Recipes

In the spirit of [psutil
recipes](http://psutil.readthedocs.io/en/latest/#recipes).

### Find process by name

Using `ps()` and dplyr:

``` r
library(dplyr)
find_procs_by_name <- function(name) {
  ps() %>%
    filter(name == !!name)  %>%
    pull(ps_handle)
}

find_procs_by_name("R")
```

    ## [[1]]
    ## <ps::ps_handle> PID=28286, NAME=R, AT=2022-06-17 11:08:04
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=25835, NAME=R, AT=2022-06-17 10:57:41
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=65603, NAME=R, AT=2022-06-17 10:00:56

Without creating the full table of processes:

``` r
find_procs_by_name <- function(name) {
  procs <- lapply(ps_pids(), function(p) {
    tryCatch({
      h <- ps_handle(p)
      if (ps_name(h) == name) h else NULL },
      no_such_process = function(e) NULL,
      access_denied = function(e) NULL
    )
  })
  procs[!vapply(procs, is.null, logical(1))]
  }

find_procs_by_name("R")
```

    ## [[1]]
    ## <ps::ps_handle> PID=25835, NAME=R, AT=2022-06-17 10:57:41
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=28286, NAME=R, AT=2022-06-17 11:08:04
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=65603, NAME=R, AT=2022-06-17 10:00:56

### Wait for a process to finish

On POSIX, there is no good way to wait for non-child processes to
finish, so we need to write a sleep-wait loop to do it. (On Windows, and
BSD systems, including macOS, there are better solutions.)

``` r
as_secs <- function(x) as.numeric(x, units = "secs")

wait_for_process <- function(proc, timeout = Inf, sleep = 0.1) {
  sleep <- as_secs(sleep)
  deadline <- Sys.time() + timeout
  while (ps_is_running(proc) && (timeout == Inf || Sys.time() < deadline)) {
    to <- min(as_secs(deadline - Sys.time()), sleep)
    Sys.sleep(to)
  }
  ! ps_is_running(proc)
}

px <- processx::process$new("sleep", "2")
p <- ps_handle(px$get_pid())
wait_for_process(p, 1)
```

    ## [1] FALSE

``` r
wait_for_process(p)
```

    ## [1] TRUE

### Wait for several processes to finish

This is similar, but we need to wait on all processes in a loop.

``` r
wait_for_processes <- function(procs, timeout = Inf) {
  gone <- list()
  alive <- procs
  deadline <- Sys.time() + timeout

  check_gone <- function(proc, timeout) {
    proc_gone <- wait_for_process(proc, timeout = timeout)
    if (proc_gone) {
      gone <<- c(gone, list(proc))
      alive <<- setdiff(alive, list(proc))
    }
  }

  while (length(alive)) {
    if (timeout <= 0) break
    for (proc in alive) {
      max_timeout <- 1 / length(alive)
      if (timeout != Inf) {
        timeout <- min(as_secs(deadline - Sys.time()), max_timeout)
        if (timeout <= 0) break
        check_gone(proc, timeout)
      } else {
        check_gone(proc, max_timeout)
      }
    }
  }
  list(gone = gone, alive = alive)
}

px1 <- processx::process$new("sleep", "10")
px2 <- processx::process$new("sleep", "10")
px3 <- processx::process$new("sleep", "1")
px4 <- processx::process$new("sleep", "1")

p1 <- ps_handle(px1$get_pid())
p2 <- ps_handle(px2$get_pid())
p3 <- ps_handle(px3$get_pid())
p4 <- ps_handle(px4$get_pid())

wait_for_processes(list(p1, p2, p3, p4), timeout = 2)
```

    ## $gone
    ## $gone[[1]]
    ## <ps::ps_handle> PID=28305, NAME=???, AT=2022-06-17 11:08:06
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=28304, NAME=???, AT=2022-06-17 11:08:06
    ## 
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=28302, NAME=sleep, AT=2022-06-17 11:08:06
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=28303, NAME=sleep, AT=2022-06-17 11:08:06

### Kill process tree

This sends a signal, so it’ll only work on Unix. Use `ps_kill()` instead
of `ps_send_signal()` on Windows.

``` r
kill_proc_tree <- function(pid, sig = signals()$SIGTERM,
                           include_parent = TRUE) {
  if (pid == Sys.getpid() && include_parent) stop("I refuse to kill myself")
  parent <- ps_handle(pid)
  children <- ps_children(parent, recursive = TRUE)
  if (include_parent) children <- c(children, parent)
  for (p in children) ps_send_signal(p, sig)
  wait_for_processes(children, timeout = 0.1)
}

p1 <- processx::process$new("sleep", "10")
p2 <- processx::process$new("sleep", "10")
p3 <- processx::process$new("sleep", "10")
kill_proc_tree(Sys.getpid(), include_parent = FALSE)
```

    ## $gone
    ## $gone[[1]]
    ## <ps::ps_handle> PID=28302, NAME=???, AT=2022-06-17 11:08:06
    ## 
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=28303, NAME=???, AT=2022-06-17 11:08:06
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=28309, NAME=???, AT=2022-06-17 11:08:08
    ## 
    ## $alive[[3]]
    ## <ps::ps_handle> PID=28310, NAME=???, AT=2022-06-17 11:08:08
    ## 
    ## $alive[[4]]
    ## <ps::ps_handle> PID=28311, NAME=???, AT=2022-06-17 11:08:09

### Terminate children

Note, that some R IDEs, including RStudio, run a multithreaded R
process, and other threads may start processes as well.
`reap_children()` will clean up all these as well, potentially causing
the IDE to misbehave or crash.

``` r
reap_children <- function(timeout = 3) {
  procs <- ps_children(ps_handle())

  ## SIGTERM
  lapply(procs, ps_terminate)

  ga <- wait_for_processes(procs, timeout = timeout)

  ## SIGKILL to the survivers
  if (length(ga$alive)) lapply(ga$alive, ps_kill)

  ga2 <- wait_for_processes(ga$alive, timeout = timeout)

  ## Some might still survive
  list(gone = c(ga$gone, ga2$gone), alive = ga2$alive)
}

pxs <- replicate(3, processx::process$new("sleep", "3"))
reap_children()
```

    ## $gone
    ## $gone[[1]]
    ## <ps::ps_handle> PID=28312, NAME=???, AT=2022-06-17 11:08:09
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=28313, NAME=???, AT=2022-06-17 11:08:09
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=28314, NAME=???, AT=2022-06-17 11:08:09
    ## 
    ## 
    ## $alive
    ## list()

### Filtering and sorting processes

Process name ending with “sh”:

``` r
ps() %>%
  filter(grepl("sh$", name))
```

    ## # A data frame: 15 × 11
    ##      pid  ppid name    username status    user  system    rss     vms created             ps_handle 
    ##    <int> <int> <chr>   <chr>    <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 25771 25439 zsh     gaborcs… runni… 3.11e-5 1.55e-4 3.42e6 4.19e11 2022-06-17 10:57:40 <ps_handl>
    ##  2 25439 25438 zsh     gaborcs… runni… 2.48e-3 1.30e-3 1.39e7 4.19e11 2022-06-17 10:57:39 <ps_handl>
    ##  3 25211 24879 zsh     gaborcs… runni… 6.82e-4 2.85e-3 3.54e6 4.19e11 2022-06-17 10:57:08 <ps_handl>
    ##  4 24879 24878 zsh     gaborcs… runni… 8.86e-3 6.64e-3 1.53e7 4.19e11 2022-06-17 10:57:07 <ps_handl>
    ##  5 65545 65211 zsh     gaborcs… runni… 3.22e-5 1.54e-4 1.03e6 4.19e11 2022-06-17 10:00:55 <ps_handl>
    ##  6 65211 65207 zsh     gaborcs… runni… 2.27e-3 1.23e-3 1.87e6 4.19e11 2022-06-17 10:00:55 <ps_handl>
    ##  7 64395 64394 bash    gaborcs… runni… 6.15e-5 1.92e-4 1.08e6 4.18e11 2022-06-17 10:00:46 <ps_handl>
    ##  8 63945 63613 zsh     gaborcs… runni… 7.52e-5 3.53e-4 2.00e6 4.19e11 2022-06-17 10:00:28 <ps_handl>
    ##  9 63613 63612 zsh     gaborcs… runni… 3.48e-3 2.36e-3 9.63e6 4.19e11 2022-06-17 10:00:27 <ps_handl>
    ## 10 63097 62765 zsh     gaborcs… runni… 1.05e-4 4.88e-4 2.03e6 4.19e11 2022-06-17 09:57:05 <ps_handl>
    ## 11 62765 62764 zsh     gaborcs… runni… 4.12e-3 3.52e-3 1.04e7 4.19e11 2022-06-17 09:57:04 <ps_handl>
    ## 12 54571 54239 zsh     gaborcs… runni… 7.58e-4 3.27e-3 2.64e6 4.19e11 2022-06-17 09:41:33 <ps_handl>
    ## 13 54239 54238 zsh     gaborcs… runni… 1.53e-2 1.27e-2 1.14e7 4.19e11 2022-06-17 09:41:32 <ps_handl>
    ## 14  1243     1 Report… gaborcs… runni… 1.59e-2 1.36e-2 6.75e6 4.18e11 2022-06-15 17:42:09 <ps_handl>
    ## 15  1063     1 Plash   gaborcs… runni… 6.35e-2 5.96e-2 3.88e7 4.20e11 2022-06-15 17:41:45 <ps_handl>

Processes owned by user:

``` r
ps() %>%
  filter(username == Sys.info()[["user"]]) %>%
  select(pid, name)
```

    ## # A data frame: 276 × 2
    ##      pid name                           
    ##    <int> <chr>                          
    ##  1 28308 Google Chrome Helper (Renderer)
    ##  2 28307 Google Chrome Helper (Renderer)
    ##  3 28306 Google Chrome Helper (Renderer)
    ##  4 28286 R                              
    ##  5 28115 Google Chrome Helper (Renderer)
    ##  6 27238 com.apple.iCloudHelper         
    ##  7 27008 Google Chrome Helper (Renderer)
    ##  8 25835 R                              
    ##  9 25771 zsh                            
    ## 10 25439 zsh                            
    ## # … with 266 more rows

Processes consuming more than 100MB of memory:

``` r
ps() %>%
  filter(rss > 100 * 1024 * 1024)
```

    ## # A data frame: 18 × 11
    ##      pid  ppid name    username status    user  system    rss     vms created             ps_handle 
    ##    <int> <int> <chr>   <chr>    <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 28286 24879 R       gaborcs… runni… 1.81e-2 5.89e-3 1.62e8 4.20e11 2022-06-17 11:08:04 <ps_handl>
    ##  2 28115   552 Google… gaborcs… runni… 2.28e-3 6.07e-4 1.07e8 4.66e11 2022-06-17 11:07:01 <ps_handl>
    ##  3 27008   552 Google… gaborcs… runni… 1.15e-2 2.54e-3 1.11e8 4.61e11 2022-06-17 11:03:08 <ps_handl>
    ##  4 25835 25439 R       gaborcs… runni… 2.49e-2 6.03e-3 1.45e8 4.19e11 2022-06-17 10:57:41 <ps_handl>
    ##  5 25386 24879 Emacs-… gaborcs… runni… 9.99e-1 1.52e-1 3.07e8 4.20e11 2022-06-17 10:57:30 <ps_handl>
    ##  6 22559   552 Google… gaborcs… runni… 1.01e-1 1.36e-2 2.93e8 4.61e11 2022-06-17 10:53:21 <ps_handl>
    ##  7 20177   552 Google… gaborcs… runni… 9.79e-2 1.85e-2 1.50e8 4.62e11 2022-06-17 10:19:03 <ps_handl>
    ##  8 20125     1 Amazon… gaborcs… runni… 5.65e-1 1.13e-1 2.09e8 3.79e10 2022-06-17 10:17:38 <ps_handl>
    ##  9 62712 54239 Emacs-… gaborcs… runni… 3.07e-1 4.55e-2 1.13e8 4.19e11 2022-06-17 09:56:55 <ps_handl>
    ## 10 62557   552 Google… gaborcs… runni… 3.54e-1 6.36e-2 1.76e8 4.62e11 2022-06-17 09:56:06 <ps_handl>
    ## 11 59570   552 Google… gaborcs… runni… 5.44e-2 1.43e-2 1.32e8 4.66e11 2022-06-17 09:47:10 <ps_handl>
    ## 12 51439   552 Google… gaborcs… runni… 1.28e+0 1.54e-1 2.88e8 4.66e11 2022-06-17 09:36:51 <ps_handl>
    ## 13  1274   552 Google… gaborcs… runni… 1.08e+1 1.52e+0 5.44e8 4.66e11 2022-06-15 17:42:51 <ps_handl>
    ## 14  1019   552 Google… gaborcs… runni… 2.53e+0 6.05e-1 1.28e8 4.72e11 2022-06-15 17:41:44 <ps_handl>
    ## 15  1017   552 Google… gaborcs… runni… 2.39e+0 6.77e-1 2.26e8 4.61e11 2022-06-15 17:41:44 <ps_handl>
    ## 16   614   552 Google… gaborcs… runni… 5.76e+1 3.46e+1 2.83e8 4.53e11 2022-06-15 17:41:42 <ps_handl>
    ## 17   552     1 Google… gaborcs… runni… 3.96e+1 1.34e+1 5.25e8 4.53e11 2022-06-15 17:41:40 <ps_handl>
    ## 18   544     1 iTerm2  gaborcs… runni… 2.29e+1 3.58e+0 5.36e8 4.21e11 2022-06-15 17:41:40 <ps_handl>

Top 3 memory consuming processes:

``` r
ps() %>%
  top_n(3, rss) %>%
  arrange(desc(rss))
```

    ## # A data frame: 3 × 11
    ##     pid  ppid name        username status  user system    rss     vms created             ps_handle 
    ##   <int> <int> <chr>       <chr>    <chr>  <dbl>  <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ## 1   544     1 iTerm2      gaborcs… runni…  22.9   3.58 5.51e8 4.21e11 2022-06-15 17:41:40 <ps_handl>
    ## 2  1274   552 Google Chr… gaborcs… runni…  10.8   1.52 5.44e8 4.66e11 2022-06-15 17:42:51 <ps_handl>
    ## 3   552     1 Google Chr… gaborcs… runni…  39.6  13.4  5.25e8 4.53e11 2022-06-15 17:41:40 <ps_handl>

Top 3 processes which consumed the most CPU time:

``` r
ps() %>%
  mutate(cpu_time = user + system) %>%
  top_n(3, cpu_time) %>%
  arrange(desc(cpu_time)) %>%
  select(pid, name, cpu_time)
```

    ## # A data frame: 3 × 3
    ##     pid name                       cpu_time
    ##   <int> <chr>                         <dbl>
    ## 1   614 Google Chrome Helper (GPU)     92.2
    ## 2   552 Google Chrome                  53.0
    ## 3   544 iTerm2                         26.5

## Code of Conduct

Please note that the ps project is released with a [Contributor Code of
Conduct](https://ps.r-lib.org/CODE_OF_CONDUCT.html). By contributing to
this project, you agree to abide by its terms.

## License

MIT © RStudio
