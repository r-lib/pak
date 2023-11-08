
#' Evaluate an expression in another R session, in the background
#'
#' Starts evaluating an R function call in a background R process, and
#' returns immediately.
#'
#' @inheritSection r Security considerations
#' @inheritParams r
#' @param supervise Whether to register the process with a supervisor. If \code{TRUE},
#'   the supervisor will ensure that the process is killed when the R process
#'   exits.
#' @param ... Extra arguments are passed to the [processx::process]
#'   constructor.
#' @return An `r_process` object, which inherits from [process],
#'   so all `process` methods can be called on it, and in addition it also
#'   has a `get_result()` method to collect the result.
#'
#' @export
#' @examplesIf FALSE
#' rx <- r_bg(function() 1 + 2)
#'
#' # wait until it is done
#' rx$wait()
#' rx$is_alive()
#' rx$get_result()

r_bg <- function(func, args = list(), libpath = .libPaths(),
                 repos = default_repos(),
                 stdout = "|", stderr = "|",
                 poll_connection = TRUE,
                 error = getOption("callr.error", "error"),
                 cmdargs = c("--slave", "--no-save", "--no-restore"),
                 system_profile = FALSE, user_profile = "project",
                 env = rcmd_safe_env(), supervise = FALSE,
                 package = FALSE, arch = "same", ...) {

  options <- as.list(environment())
  options$extra  <- list(...)
  options$load_hook <- default_load_hook()
  r_process$new(options = options)
}
