should_show_progress_bar <- function() {
  # Option takes precedence
  opt <- getOption("pkg.show_progress", NULL)
  if (!is.null(opt)) return(isTRUE(opt))

  # FALSE on CI, this should be in cli, probably
  if (Sys.getenv("CI", "") != "") return(FALSE)

  # FALSE in knitr, this should be in cli, probably
  if (isTRUE(getOption("knitr.in.progress"))) return(FALSE)

  # FALSE in testthat as well, this should NOT be in cli, maybe
  if (identical(Sys.getenv("TESTTHAT"), "true")) return(FALSE)

  # Otherwise trust cli
  cli::is_dynamic_tty()
}
