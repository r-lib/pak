
(function() {
  trace <- rev(sys.calls())
  source_arg <- NULL
  for (call in trace) {
    if (call[[1]] == quote(source) && is.character(call[[2]])) {
      source_arg <- call[[2]]
      break
    }
  }

  stream <- "stable"
  if (!is.null(source_arg)) {
    if (grepl("\\?stream=[a-z]+$", source_arg)) {
      stream <- sub("^.*\\?stream=", "", source_arg)
    } else if (grepl("\\?dev$", source_arg) || grepl("\\?devel$", source_arg)) {
      stream <- "devel"
    } else if (grepl("\\?rc$", source_arg)) {
      stream <- "rc"
    }
  }

  if (stream == "dev") {
    stream <- "devel"
  }

  message("Installing pak from stream ", stream, ".")

  install.packages("pak", repos = sprintf(
    "https://r-lib.github.io/p/pak/%s/%s/%s/%s",
    stream,
    .Platform$pkgType,
    R.Version()$os,
    R.Version()$arch
  ))
})()
