list_files <- function(path, filename) {
  files <- dir(path = path, pattern = filename, all.files = TRUE, full.names = TRUE)
  dirs <- dir.exists(files)
  files <- files[!dirs]
  files
}

match_contents <- function(f, contents, n, fixed) {
  if (is.null(contents)) {
    return(TRUE)
  }

  fc <- readLines(f, n)
  any(grepl(contents, fc, fixed = fixed))
}
