if (exists(".knitr_asciicast_process", envir = .GlobalEnv)) {
  rm(list = ".knitr_asciicast_process", envir = .GlobalEnv)
}

asciicast::init_knitr_engine(
  echo = TRUE,
  echo_input = FALSE,
  timeout = as.integer(Sys.getenv("ASCIICAST_TIMEOUT", 10)),
  startup = quote(options(cli.num_colors = 256))
)

knitr::opts_chunk$set(
  asciicast_knitr_output = "html",
  asciicast_include_style = FALSE,
  cache = TRUE,
  cache.path = file.path(getwd(), "man/_cache/"),
  fig.path = file.path(getwd(), "man/figures"),
  error = TRUE
)

list(
  markdown = TRUE,
  restrict_image_formats = TRUE
)
