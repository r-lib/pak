
load_client_lib <- function(sofile = NULL, pxdir = NULL) {
  ext <- .Platform$dynlib.ext
  sofile_in_processx <- function() {
    arch <- .Platform$r_arch
    if (!is.null(pxdir)) {
      sofile <- file.path(pxdir, "libs", arch, paste0("client", ext))
      if (file.exists(sofile)) return(sofile)
    }

    sofile <- system.file(
      "libs", arch, paste0("client", ext),
      package = "processx")
    if (sofile != "" && file.exists(sofile)) return(sofile)

    # Try this as well, this is for devtools/pkgload
    sofile <- system.file(
      "src", paste0("client", ext),
      package = "processx")
    if (sofile != "" && file.exists(sofile)) return(sofile)         # nocov

    # stop() here and not throw(), because this function should be standalone
    stop("Cannot find client file")
  }

  # We set this to `FALSE` if we load the library from the processx
  # install path since that library might be shared (e.g. in tests)
  need_cleanup <- TRUE

  if (is.null(sofile) || Sys.getenv("CALLR_NO_TEMP_DLLS", "false") == "true") {
    sofile <- sofile_in_processx()
    lib <- dyn.load(sofile)
    need_cleanup <- FALSE
  } else {
    # This is the usual case, first we try loading it from the
    # temporary directory. If that fails (e.g. noexec), then
    # from processx. We saved the location of processx when we
    # loaded callr, just in case the used changes the lib path.
    lib <- tryCatch(dyn.load(sofile), error = function(err) err)
    if (inherits(lib, "error")) {
      sofile <- sofile_in_processx()
      tryCatch(
        expr = {
          lib <- dyn.load(sofile)
          need_cleanup <- FALSE
        },
	error = function(err2) {
	  err2$message <- err2$message <- paste0(" after ", lib$message)
	  stop(err2)
	}
      )
    }
  }

  # cleanup if setup fails
  if (need_cleanup) {
    on.exit(try(dyn.unload(sofile), silent = TRUE), add = TRUE)
  }

  sym_encode <- getNativeSymbolInfo("processx_base64_encode", lib)
  sym_decode <- getNativeSymbolInfo("processx_base64_decode", lib)
  sym_disinh <- getNativeSymbolInfo("processx_disable_inheritance", lib)
  sym_write  <- getNativeSymbolInfo("processx_write", lib)
  sym_setout <- getNativeSymbolInfo("processx_set_stdout", lib)
  sym_seterr <- getNativeSymbolInfo("processx_set_stderr", lib)
  sym_setoutf <- getNativeSymbolInfo("processx_set_stdout_to_file", lib)
  sym_seterrf <- getNativeSymbolInfo("processx_set_stderr_to_file", lib)

  env <- new.env(parent = emptyenv())
  env$.path <- sofile
  env$.lib <- lib

  mycall <- .Call

  env$base64_encode <- function(x) rawToChar(mycall(sym_encode, x))
  env$base64_decode <- function(x) {
    if (is.character(x)) {
      x <- charToRaw(paste(gsub("\\s+", "", x), collapse = ""))
    }
    mycall(sym_decode, x)
  }

  env$disable_fd_inheritance <- function() mycall(sym_disinh)

  env$write_fd <- function(fd, data) {
    if (is.character(data)) data <- charToRaw(paste0(data, collapse = ""))
    len <- length(data)
    repeat {
      written <- mycall(sym_write, fd, data)
      len <- len - written
      if (len == 0) break
      if (written) data <- data[-(1:written)]
      Sys.sleep(.1)
    }
  }

  env$set_stdout <- function(fd, drop = TRUE) {
    mycall(sym_setout, as.integer(fd), as.logical(drop))
  }

  env$set_stderr <- function(fd, drop = TRUE) {
    mycall(sym_seterr, as.integer(fd), as.logical(drop))
  }

  env$set_stdout_file <- function(path) {
    mycall(sym_setoutf, as.character(path)[1])
  }

  env$set_stderr_file <- function(path) {
    mycall(sym_seterrf, as.character(path)[1])
  }

  env$.finalize <- function() {
    if (need_cleanup) {
      dyn.unload(env$.path)
    }
    rm(list = ls(env, all.names = TRUE), envir = env)
  }

  penv <- environment()
  parent.env(penv) <- baseenv()

  reg.finalizer(
    env,
    function(e) if (".finalize" %in% names(e)) e$.finalize(),
    onexit = TRUE)

  ## Clear the cleanup method
  on.exit(NULL)
  env
}
