make_unzip_process <- function(
  zipfile,
  exdir = ".",
  post_process = NULL,
  stdout = "|",
  stderr = "2>&1",
  ...
) {
  up <- zip::unzip_process()
  up$new(
    zipfile,
    exdir = exdir,
    post_process = post_process,
    stdout = stdout,
    stderr = stderr,
    ...
  )
}
