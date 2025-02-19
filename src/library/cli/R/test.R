
#' Test cli output with testthat
#'
#' Use this function in your testthat test files, to test cli output.
#' It requires testthat edition 3, and works best with snapshot tests.
#'
#' `test_that_cli()` calls [testthat::test_that()] multiple times, with
#' different cli configurations. This makes it simple to test cli output
#' with and without ANSI colors, with and without Unicode characters.
#'
#' Currently available configurations:
#' * `plain`: no ANSI colors, ASCII characters only.
#' * `ansi`: ANSI colors, ASCII characters only.
#' * `unicode`: no ANSI colors, Unicode characters.
#' * `fancy`; ANSI colors, Unicode characters.
#'
#' See examples below and in cli's own tests, e.g. in
#' <https://github.com/r-lib/cli/tree/main/tests/testthat>
#' and the corresponding snapshots at
#' <https://github.com/r-lib/cli/tree/main/tests/testthat/_snaps>
#'
#' ## Important note regarding Windows
#'
#' Because of base R's limitation to record Unicode characters on Windows,
#' we suggest that you record your snapshots on Unix, or you restrict
#' your tests to ASCII configurations.
#'
#' Unicode tests on Windows are automatically skipped by testthat
#' currently.
#'
#' @param desc Test description, passed to [testthat::test_that()], after
#' appending the name of the cli configuration to it.
#' @param code Test code, it is modified to set up the cli config, and
#' then passed to [testthat::test_that()]
#' @param configs cli configurations to test `code` with. The default is
#' `NULL`, which includes all possible configurations. It can also be a
#' character vector, to restrict the tests to some configurations only.
#' See available configurations below.
#' @param links Whether to run the code with various hyperlinks allowed.
#' If `NULL` then hyperlinks are turned off. Otherwise it can be a character
#' vector with possible hyperlink configurations:
#'   * `"all"`: turn on all hyperlinks,
#'   * `"none"`: turn off all hyperlinks.
#'
#' @export
#' @examples
#' # testthat cannot record or compare snapshots when you run these
#' # examples interactively, so you might want to copy them into a test
#' # file
#'
#' # Default configurations
#' cli::test_that_cli("success", {
#'   testthat::local_edition(3)
#'   testthat::expect_snapshot({
#'     cli::cli_alert_success("wow")
#'   })
#' })
#'
#' # Only use two configurations, because this output does not have colors
#' cli::test_that_cli(configs = c("plain", "unicode"), "cat_bullet", {
#'   testthat::local_edition(3)
#'   testthat::expect_snapshot({
#'     cli::cat_bullet(letters[1:5])
#'   })
#' })
#'
#' # You often need to evaluate all cli calls of a test case in the same
#' # environment. Use `local()` to do that:
#' cli::test_that_cli("theming", {
#'   testthat::local_edition(3)
#'   testthat::expect_snapshot(local({
#'     cli::cli_div(theme = list(".alert" = list(before = "!!! ")))
#'     cli::cli_alert("wow")
#'   }))
#' })

test_that_cli <- function(desc, code,
                          configs = c("plain", "ansi", "unicode", "fancy"),
                          links = NULL) {
  code <- substitute(code)

  configs <- apply(expand.grid(configs, links %||% ""), 1, paste, collapse = "-")
  configs <- sub("-$", "", configs)

  doconfigs <- list(
    list(id = "plain",   unicode = FALSE, num_colors =   1, links = FALSE),
    list(id = "ansi",    unicode = FALSE, num_colors = 256, links = FALSE),
    list(id = "unicode", unicode = TRUE,  num_colors =   1, links = FALSE),
    list(id = "fancy",   unicode = TRUE,  num_colors = 256, links = FALSE),

    list(id = "plain-none",   unicode = FALSE, num_colors =   1, links = FALSE),
    list(id = "ansi-none",    unicode = FALSE, num_colors = 256, links = FALSE),
    list(id = "unicode-none", unicode = TRUE,  num_colors =   1, links = FALSE),
    list(id = "fancy-none",   unicode = TRUE,  num_colors = 256, links = FALSE),

    list(id = "plain-all",   unicode = FALSE, num_colors =   1, links = TRUE),
    list(id = "ansi-all",    unicode = FALSE, num_colors = 256, links = TRUE),
    list(id = "unicode-all", unicode = TRUE,  num_colors =   1, links = TRUE),
    list(id = "fancy-all",   unicode = TRUE,  num_colors = 256, links = TRUE)
  )

  parent <- parent.frame()
  lapply(doconfigs, function(conf) {
    if (!is.null(configs) && ! conf$id %in% configs) return()
    code2 <- substitute({
      testthat::local_reproducible_output(
        crayon = num_colors > 1,
        unicode = unicode
      )
      withr::local_options(
        cli.hyperlink = links,
        cli.hyperlink_help = links,
        cli.hyperlink_run = links,
        cli.hyperlink_vignette = links,
        cli.hyperlink_file_url_format = NULL,
        cli.hyperlink_run_url_format = NULL,
        cli.hyperlink_help_url_format = NULL,
        cli.hyperlink_vignette_url_format = NULL
      )
      withr::local_envvar(
        R_CLI_HYPERLINK_FILE_URL_FORMAT = NA_character_,
        R_CLI_HYPERLINK_RUN_URL_FORMAT = NA_character_,
        R_CLI_HYPERLINK_HELP_URL_FORMAT = NA_character_,
        R_CLI_HYPERLINK_VIGNETTE_URL_FORMAT = NA_character_
      )
      code_
    }, c(conf, list(code_ = code)))
    desc2 <- paste0(desc, " [", conf$id, "]")
    test <- substitute(
      testthat::test_that(desc, code),
      list(desc = desc2, code = code2)
    )
    eval(test, envir = parent)
  })
}

local_clean_cli_context <- function(.local_envir = parent.frame()) {
  withr::local_options(
    .local_envir = .local_envir,
    cli.hyperlink = NULL,
    cli.hyperlink_run = NULL,
    cli.hyperlink_help = NULL,
    cli.hyperlink_vignette = NULL,
    cli.hyperlink_file_url_format = NULL,
    cli.hyperlink_run_url_format = NULL,
    cli.hyperlink_help_url_format = NULL,
    cli.hyperlink_vignette_url_format = NULL,
    cli.num_colors = NULL,
    cli.palette = NULL,
    crayon.enabled = NULL
  )
  withr::local_envvar(
    .local_envir = .local_envir,
    R_CLI_HYPERLINKS = NA_character_,
    R_CLI_HYPERLINK_RUN = NA_character_,
    R_CLI_HYPERLINK_HELP = NA_character_,
    R_CLI_HYPERLINK_VIGNETTE = NA_character_,
    R_CLI_HYPERLINK_FILE_URL_FORMAT = NA_character_,
    R_CLI_HYPERLINK_RUN_URL_FORMAT = NA_character_,
    R_CLI_HYPERLINK_HELP_URL_FORMAT = NA_character_,
    R_CLI_HYPERLINK_VIGNETTE_URL_FORMAT = NA_character_,
    RSTUDIO_CLI_HYPERLINKS = NA_character_,
    R_CLI_NUM_COLORS = NA_character_,
    NO_COLOR = NA_character_,
    WT_SESSION = NA_character_,
    CI = NA_character_,
    TEAMCITY_VERSION = NA_character_,
    TERM_PROGRAM = NA_character_,
    TERM_PROGRAM_VERSION = NA_character_,
    VTE_VERSION = NA_character_
  )
}
