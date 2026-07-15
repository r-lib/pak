
client_env <- local({
  env <- new.env(parent = emptyenv())
  env$`__callr_data__` <- new.env(parent = baseenv())

  rsfile <- file.path("R", "aaa-rstudio-detect.R")
  sys.source(rsfile, envir = env$`__callr_data__`, keep.source = FALSE)
  errfile <- file.path("R", "standalone-errors.R")
  sys.source(errfile, envir = env$`__callr_data__`, keep.source = FALSE)
  loadfile <- file.path("R", "load-client.R")
  sys.source(loadfile, envir = env$`__callr_data__`, keep.source = FALSE)

  env
})
