
globalVariables("private")

#' testthat reporter that checks if child processes are cleaned up in tests
#'
#' `CleanupReporter` takes an existing testthat `Reporter` object, and
#' wraps it, so it checks for leftover child processes, at the specified
#' place, see the `proc_unit` argument below.
#'
#' Child processes can be reported via a failed expectation, cleaned up
#' silently, or cleaned up and reported (the default).
#'
#' If a `test_that()` block has an error, `CLeanupReporter` does not
#' emit any expectations at the end of that block. The error will lead to a
#' test failure anyway. It will still perform the cleanup, if requested,
#' however.
#'
#' The constructor of the `CleanupReporter` class has options:
#' * `file`: the output file, if any, this is passed to `reporter`.
#' * `proc_unit`: when to perform the child process check and cleanup.
#'   Possible values:
#'     * `"test"`: at the end of each [testthat::test_that()] block
#'       (the default),
#'     * `"testsuite"`: at the end of the test suite.
#' * `proc_cleanup`: Logical scalar, whether to kill the leftover
#'   processes, `TRUE` by default.
#' * `proc_fail`: Whether to create an expectation, that fails if there
#'   are any processes alive, `TRUE` by default.
#' * `proc_timeout`: How long to wait for the processes to quit. This is
#'   sometimes needed, because even if some kill signals were sent to
#'   child processes, it might take a short time for these to take effect.
#'   It defaults to one second.
#' * `rconn_unit`: When to perform the R connection cleanup. Possible values
#'   are `"test"` and `"testsuite"`, like for `proc_unit`.
#' * `rconn_cleanup`: Logical scalar, whether to clean up leftover R
#'   connections. `TRUE` by default.
#' * `rconn_fail`: Whether to fail for leftover R connections. `TRUE` by
#'   default.
#' * `file_unit`: When to check for open files. Possible values are
#'    `"test"` and `"testsuite"`, like for `proc_unit`.
#' * `file_fail`: Whether to fail for leftover open files. `TRUE` by
#'   default.
#' * `conn_unit`: When to check for open network connections.
#'   Possible values are `"test"` and `"testsuite"`, like for `proc_unit`.
#' * `conn_fail`: Whether to fail for leftover network connections.
#'   `TRUE` by default.
#'
#' @note Some IDEs, like RStudio, start child processes frequently, and
#' sometimes crash when these are killed, only use this reporter in a
#' terminal session. In particular, you can always use it in the
#' idiomatic `testthat.R` file, that calls `test_check()` during
#' `R CMD check`.
#'
#' @param reporter A testthat reporter to wrap into a new `CleanupReporter`
#'   class.
#' @return New reporter class that behaves exactly like `reporter`,
#'   but it checks for, and optionally cleans up child processes, at the
#'   specified granularity.
#'
#' @section Examples:
#' This is how to use this reporter in `testthat.R`:
#' ```
#' library(testthat)
#' library(mypackage)
#'
#' if  (ps::ps_is_supported()) {
#'   reporter <- ps::CleanupReporter(testthat::ProgressReporter)$new(
#'     proc_unit = "test", proc_cleanup = TRUE)
#' } else {
#'   ## ps does not support this platform
#'   reporter <- "progress"
#' }
#'
#' test_check("mypackage", reporter = reporter)
#' ```
#'
#' @export

CleanupReporter <- function(reporter = testthat::ProgressReporter) {

  R6::R6Class("CleanupReporter",
    inherit = reporter,
    public = list(

      initialize = function(
        file = getOption("testthat.output_file", stdout()),
        proc_unit = c("test", "testsuite"),
        proc_cleanup = TRUE, proc_fail = TRUE, proc_timeout = 1000,
        rconn_unit = c("test", "testsuite"),
        rconn_cleanup = TRUE, rconn_fail = TRUE,
        file_unit = c("test", "testsuite"), file_fail = TRUE,
        conn_unit = c("test", "testsuite"), conn_fail = TRUE) {

        if (!ps::ps_is_supported()) {
          stop("CleanupReporter is not supported on this platform")
        }

        super$initialize(file = file)
        private$proc_unit <- match.arg(proc_unit)
        private$proc_cleanup <- proc_cleanup
        private$proc_fail <- proc_fail
        private$proc_timeout <- proc_timeout

        private$rconn_unit <- match.arg(rconn_unit)
        private$rconn_cleanup <- rconn_cleanup
        private$rconn_fail <- rconn_fail

        private$file_unit <- match.arg(file_unit)
        private$file_fail <- file_fail

        private$conn_unit <- match.arg(conn_unit)
        private$conn_fail <- conn_fail

        invisible(self)
      },

      start_test = function(context, test) {
        private$has_error <- FALSE
        super$start_test(context, test)
        if (private$file_unit == "test") private$files <- ps_open_files(ps_handle())
        if (private$rconn_unit == "test") private$rconns <- showConnections()
        if (private$proc_unit == "test") private$tree_id <- ps::ps_mark_tree()
        if (private$conn_unit == "test") private$conns <- ps_connections(ps_handle())
      },

      end_test = function(context, test) {
        if (private$proc_unit == "test") self$do_proc_cleanup(test)
        if (private$rconn_unit == "test") self$do_rconn_cleanup(test)
        if (private$file_unit == "test") self$do_file_cleanup(test)
        if (private$conn_unit == "test") self$do_conn_cleanup(test)
        super$end_test(context, test)
      },

      add_result = function(context, test, result) {
        if (inherits(result, "expectation_error")) {
          private$has_error <- TRUE
        }
        super$add_result(context, test, result)
      },

      start_reporter = function() {
        super$start_reporter()
        if (private$file_unit == "testsuite") private$files <- ps_open_files(ps_handle())
        if (private$rconn_unit == "testsuite") private$rconns <- showConnections()
        if (private$proc_unit == "testsuite") private$tree_id <- ps::ps_mark_tree()
        if (private$conn_unit == "testsuite") private$conns <- ps_connections(ps_handle())
      },

      end_reporter = function() {
        super$end_reporter()
        if (private$proc_unit  == "testsuite") {
          self$do_proc_cleanup("testsuite", quote = "")
        }
        if (private$rconn_unit  == "testsuite") {
          self$do_rconn_cleanup("testsuite", quote = "")
        }
        if (private$file_unit  == "testsuite") {
          self$do_file_cleanup("testsuite", quote = "")
        }
        if (private$conn_unit  == "testsuite") {
          self$do_conn_cleanup("testsuite", quote = "")
        }
      },

      do_proc_cleanup = function(test, quote = "'") {
        Sys.unsetenv(private$tree_id)
        deadline <- Sys.time() + private$proc_timeout / 1000
        if (private$proc_fail) {
          while (length(ret <- ps::ps_find_tree(private$tree_id)) &&
                 Sys.time() < deadline) Sys.sleep(0.05)
          # maybe gc() will clean up something
          if (length(ret) > 0) {
            gc()
            ret <- ps::ps_find_tree(private$tree_id)
          }
        }
        if (private$proc_cleanup) {
          ret <- ps::ps_kill_tree(private$tree_id)
        }
        if (private$proc_fail && !private$has_error)  {
          testthat::with_reporter(self, start_end_reporter = FALSE, {
            self$expect_cleanup(test, ret, quote)
          })
        }
      },

      do_rconn_cleanup = function(test, quote = "'") {
        old <- private$rconns
        new <- showConnections()
        private$rconns <- NULL
        leftover <- ! new[, "description"] %in% old[, "description"]

        # maybe gc() will clean up some
        if (sum(leftover) > 0) {
          gc()
          new <- showConnections()
          leftover <- ! new[, "description"] %in% old[, "description"]
        }

        if (private$rconn_cleanup) {
          for (no in as.integer(rownames(new)[leftover])) {
            tryCatch(close(getConnection(no)), error = function(e) NULL)
          }
        }

        if (private$rconn_fail && !private$has_error) {
          act <- testthat::quasi_label(rlang::enquo(test), test)
          testthat::expect(
            sum(leftover) == 0,
            sprintf(
              "%s did not close R connections: %s",
              encodeString(act$lab, quote = quote),
              paste0(encodeString(new[leftover, "description"], quote = "'"),
                     " (", rownames(new)[leftover], ")", collapse = ",  ")))
        }
      },

      do_file_cleanup = function(test, quote = "'") {
        old <- private$files
        new <- ps_open_files(ps_handle())
        private$files <- NULL
        leftover <- ! new$path %in% old$path

        ## Need to ignore some open files:
        ## * /dev/urandom might be opened internally by curl, openssl, etc.
        leftover <- leftover & new$path != "/dev/urandom"

        # maybe gc() will clean up some
        if (sum(leftover) > 0) {
          gc()
          new <- ps_open_files(ps_handle())
          leftover <- ! new$path %in% old$path
          leftover <- leftover & new$path != "/dev/urandom"
        }

        if (private$file_fail && !private$has_error) {
          act <- testthat::quasi_label(rlang::enquo(test), test)
          testthat::expect(
            sum(leftover) == 0,
            sprintf(
              "%s did not close open files: %s",
              encodeString(act$lab, quote = quote),
              paste0(encodeString(new$path[leftover], quote = "'"),
                     collapse = ",  ")))
        }
      },

      do_conn_cleanup = function(test, quote = "'") {
        old <- private$conns[, 1:6]
        private$conns <- NULL

        ## On windows, sometimes it takes time to remove the connection
        ## from the processes connection tables, so we try waiting a bit.
        ## We haven't seen issues with this on other OSes yet.
        deadline <- Sys.time() + as.difftime(0.5, units = "secs")
        done <- FALSE
        repeat {
          new <- ps_connections(ps_handle())[, 1:6]
          ## This is a connection that is used internally on macOS,
          ## for DNS resolution. We'll just ignore it. Looks like this:
          ## # A data frame: 2 x 6
          ##    fd family  type        laddr lport raddr
          ## <int> <chr>   <chr>       <chr> <int> <chr>
          ##     7 AF_UNIX SOCK_STREAM <NA>     NA /var/run/mDNSResponder
          ##    10 AF_UNIX SOCK_STREAM <NA>     NA /var/run/mDNSResponder
          new <- new[
            new$family != "AF_UNIX" |
            new$type != "SOCK_STREAM" |
            is.na(new$raddr) |
            paste(tolower(basename(new$raddr))) != "mdnsresponder", ]

          leftover <- ! apply(new, 1, paste, collapse = "&") %in%
            apply(old, 1, paste, collapse = "&")

          # is this the final try, or are we all clean?
          if (done || sum(leftover) == 0) break

          # if Unix, then try again after gc()
          # on Windows, gc() after a timeout, then quit
          if (!ps_os_type()[["WINDOWS"]] || Sys.time() >= deadline) {
            gc()
            done <- TRUE
            next
          }

          Sys.sleep(0.05)
        }

        if (private$conn_fail && !private$has_error) {
          left <- new[leftover,]
          act <- testthat::quasi_label(rlang::enquo(test), test)
          testthat::expect(
            sum(leftover) == 0,
            sprintf(
              "%s did not close network connections: \n%s",
              encodeString(act$lab, quote = quote),
              paste(format(left), collapse = "\n")))
        }
      },

      expect_cleanup = function(test, pids, quote) {
        act <- testthat::quasi_label(rlang::enquo(test), test)
        act$pids <- length(pids)
        testthat::expect(
          length(pids) == 0,
          sprintf("%s did not clean up processes: %s",
                  encodeString(act$lab, quote = quote),
                  paste0(encodeString(names(pids), quote = "'"),
                         " (", pids, ")", collapse = ", ")))

        invisible(act$val)
      }
    ),

    private = list(
      proc_unit = NULL,
      proc_cleanup = NULL,
      proc_fail = NULL,
      proc_timeout = NULL,

      rconn_unit = NULL,
      rconn_cleanup = NULL,
      rconn_fail = NULL,
      rconns = NULL,

      file_unit = NULL,
      file_fail = NULL,
      files = NULL,

      conn_unit = NULL,
      conn_fail = NULL,
      conns = NULL,

      tree_id = NULL,

      has_error = FALSE
    )
  )
}
