
#' Run an `R CMD` command in the background
#'
#' The child process is started in the background, and the function
#' return immediately.
#'
#' @inheritSection r Security considerations
#' @inheritParams rcmd
#' @param supervise Whether to register the process with a supervisor. If \code{TRUE},
#'   the supervisor will ensure that the process is killed when the R process
#'   exits.
#' @param ... Extra arguments are passed to the [processx::process]
#'   constructor.
#' @return It returns a [process] object.
#'
#' @family R CMD commands
#' @export

rcmd_bg <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                    stdout = "|", stderr = "|",
                    poll_connection = TRUE,
                    repos = default_repos(),
                    system_profile = FALSE, user_profile = "project",
                    env = rcmd_safe_env(), wd = ".",
                    supervise = FALSE, ...) {

  options <- as.list(environment())
  options$extra <- list(...)
  rcmd_process$new(options = options)
}
