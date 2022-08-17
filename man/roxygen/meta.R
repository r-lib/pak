
if (exists(".knitr_asciicast_process", envir = .GlobalEnv)) {
  rm(list = ".knitr_asciicast_process", envir = .GlobalEnv)
}
opts <- list(
  asciicast_knitr_output = "html",
  asciicast_at = "end",
  asciicast_end_wait = 3,
  asciicast_echo = FALSE,
  asciicast_theme = "pkgdown",
  asciicast_omit_last_line = TRUE,
  asciicast_include_style = FALSE,
  asciicast_html_details = TRUE
)
asciicast::init_knitr_engine(
  echo = TRUE,
  echo_input = FALSE,
  interactive = FALSE,
  options = opts,
  timeout = as.integer(Sys.getenv("ASCIICAST_TIMEOUT", 10))
)
do.call(base::options, opts)

list(
  markdown = TRUE,
  knitr_chunk_options = list(
    cache =  TRUE,
    cache_lazy = FALSE,
    cache.path = file.path(getwd(), "man/_cache/"),
    fig.path = file.path(getwd(), "man/figures"),
    fig.width = 8,
    error = TRUE
  ),
  restrict_image_formats = TRUE
)
