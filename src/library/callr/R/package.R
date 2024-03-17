
## R CMD check workaround
dummy_r6 <- function() R6::R6Class

clients <- NULL
sofiles <- NULL
env_file <- NULL

## We save this as an RDS, so it can be loaded quickly
.onLoad <- function(libname, pkgname) {
  err$onload_hook()
  env_file <<- tempfile("callr-env-")
  clients <<- asNamespace("processx")$client
  sofiles <<- get_client_files()
  client_env$`__callr_data__`$sofile <- sofiles
  client_env$`__callr_data__`$pxdir <- system.file(package = "processx")
}

prepare_client_files <- function() {
  for (aa in names(client_env$`__callr_data__`$sofile)) {
    fn <- client_env$`__callr_data__`$sofile[[aa]]
    if (!file.exists(fn)) {
      dir.create(dirname(fn), recursive = TRUE)
      writeBin(clients[[aa]]$bytes, fn)
    }
  }

  if (!file.exists(env_file)) {
    saveRDS(client_env, file = env_file, version = 2, compress = FALSE)
  }
  invisible()
}

get_client_files <- function() {
  archs <- ls(clients)
  vapply(archs, function(aa) {
    hash <- substr(clients[[aa]]$md5, 1, 7)

    # Filename must be `client.ext` so that `dyn.load()` can find
    # the init function
    file.path(
      tempdir(),
      "callr",
      sub("arch-", "", aa),  # Might be empty
      hash,
      paste0("client", .Platform$dynlib.ext)
    )
  }, character(1))
}

.onUnload <- function(libpath) {
  unlink(
    normalizePath(c(sofiles, env_file), mustWork = FALSE),
    recursive = TRUE,
    force = TRUE
  )
}
