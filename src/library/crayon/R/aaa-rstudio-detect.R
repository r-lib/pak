
rstudio <- local({

  standalone_env <- environment()
  parent.env(standalone_env) <- baseenv()

  # -- Collect data ------------------------------------------------------

  data <- NULL

  get_data <- function() {
    envs <- c(
      "R_BROWSER",
      "R_PDFVIEWER",
      "RSTUDIO",
      "RSTUDIO_TERM",
      "RSTUDIO_CONSOLE_COLOR",
      "RSTUDIOAPI_IPC_REQUESTS_FILE",
      "XPC_SERVICE_NAME",
      "ASCIICAST")

    d <- list(
      pid = Sys.getpid(),
      envs = Sys.getenv(envs),
      api = tryCatch(
        asNamespace("rstudioapi")$isAvailable(),
        error = function(err) FALSE
      ),
      tty = isatty(stdin()),
      gui = .Platform$GUI,
      args = commandArgs(),
      search = search()
    )
    d$ver <- if (d$api) asNamespace("rstudioapi")$getVersion()
    d$desktop <- if (d$api) asNamespace("rstudioapi")$versionInfo()$mode

    d
  }

  # -- Auto-detect environment -------------------------------------------

  is_rstudio <- function() {
    Sys.getenv("RSTUDIO") == "1"
  }

  detect <- function(clear_cache = FALSE) {
    # Check this up front, in case we are in a testthat 3e test block.
    # We cannot cache this, because we might be in RStudio in reality.
    if (!is_rstudio()) {
      return(get_caps(type = "not_rstudio"))
    }

    # Cached?
    if (clear_cache) data <<- NULL
    if (!is.null(data)) return(get_caps(data))

    # Otherwise get data
    new <- get_data()

    # Cache unless told otherwise
    cache <- TRUE

    new$type <- if (new$envs[["RSTUDIO"]] != "1") {
      # 1. Not RStudio at all
      "not_rstudio"

    } else if (new$gui == "RStudio" && new$api) {
      # 2. RStudio console, properly initialized
      "rstudio_console"

    } else if (! new$api && basename(new$args[1]) == "RStudio") {
      # 3. RStudio console, initializing
      cache <- FALSE
      "rstudio_console_starting"

    } else if (new$gui == "Rgui") {
      # Still not RStudio, but Rgui that was started from RStudio
      "not_rstudio"

    } else if (new$tty && new$envs[["ASCIICAST"]] != "true") {
      # 4. R in the RStudio terminal
      # This could also be a subprocess of the console or build pane
      # with a pseudo-terminal. There isn't really a way to rule that
      # out, without inspecting some process data with ps::ps_*().
      # At least we rule out asciicast
      "rstudio_terminal"

    } else if (! new$tty &&
               new$envs[["RSTUDIO_TERM"]] == "" &&
               new$envs[["R_BROWSER"]] == "false" &&
               new$envs[["R_PDFVIEWER"]] == "false" &&
               is_build_pane_command(new$args)) {
      # 5. R in the RStudio build pane
      # https://github.com/rstudio/rstudio/blob/master/src/cpp/session/
      # modules/build/SessionBuild.cpp#L231-L240
      "rstudio_build_pane"

    } else if (new$envs[["RSTUDIOAPI_IPC_REQUESTS_FILE"]] != "" &&
               grepl("rstudio", new$envs[["XPC_SERVICE_NAME"]])) {
      # RStudio job, XPC_SERVICE_NAME=0 in the subprocess of a job
      # process. Hopefully this is reliable.
      "rstudio_job"

    } else if (new$envs[["RSTUDIOAPI_IPC_REQUESTS_FILE"]] != "" &&
               any(grepl("SourceWithProgress.R", new$args))) {
      # Or we can check SourceWithProgress.R in the command line, see
      # https://github.com/r-lib/cli/issues/367
      "rstudio_job"

    } else {
      # Otherwise it is a subprocess of the console, terminal or
      # build pane, and it is hard to say which, so we do not try.
      "rstudio_subprocess"
    }

    installing <- Sys.getenv("R_PACKAGE_DIR", "")
    if (cache && installing == "") data <<- new

    get_caps(new)
  }

  is_build_pane_command <- function(args) {
    cmd <- gsub("[\"']", "", args[[length(args)]], useBytes = TRUE)
    rcmd <- sub("[(].*$", "", cmd)
    rcmd %in% c("devtools::build", "devtools::test", "devtools::check")
  }

  # -- Capabilities ------------------------------------------------------

  caps <- list()

  caps$not_rstudio <- function(data) {
    list(
      type = "not_rstudio",
      dynamic_tty = FALSE,
      ansi_tty = FALSE,
      ansi_color = FALSE,
      num_colors = 1L
    )
  }

  caps$rstudio_console <- function(data) {
    list(
      type = "rstudio_console",
      dynamic_tty = TRUE,
      ansi_tty = FALSE,
      ansi_color = data$envs[["RSTUDIO_CONSOLE_COLOR"]] != "",
      num_colors = as.integer(data$envs[["RSTUDIO_CONSOLE_COLOR"]])
    )
  }

  caps$rstudio_console_starting <- function(data) {
    res <- caps$rstudio_console(data)
    res$type <- "rstudio_console_starting"
    res
  }

  caps$rstudio_terminal <- function(data) {
    list(
      type = "rstudio_terminal",
      dynamic_tty = TRUE,
      ansi_tty = FALSE,
      ansi_color = FALSE,
      num_colors = 1L
    )
  }

  caps$rstudio_build_pane <- function(data) {
    list(
      type = "rstudio_build_pane",
      dynamic_tty = TRUE,
      ansi_tty = FALSE,
      ansi_color = data$envs[["RSTUDIO_CONSOLE_COLOR"]] != "",
      num_colors = as.integer(data$envs[["RSTUDIO_CONSOLE_COLOR"]])
    )
  }

  caps$rstudio_job <- function(data) {
    list(
      type = "rstudio_job",
      dynamic_tty = FALSE,
      ansi_tty = FALSE,
      ansi_color = data$envs[["RSTUDIO_CONSOLE_COLOR"]] != "",
      num_colors = as.integer(data$envs[["RSTUDIO_CONSOLE_COLOR"]])
    )
  }

  caps$rstudio_subprocess <- function(data) {
    list(
      type = "rstudio_subprocess",
      dynamic_tty = FALSE,
      ansi_tty = FALSE,
      ansi_color = FALSE,
      num_colors = 1L
    )
  }

  get_caps <- function(data, type = data$type) caps[[type]](data)

  structure(
    list(
      .internal = standalone_env,
      is_rstudio = is_rstudio,
      detect = detect
    ),
    class = c("standalone_rstudio_detect", "standalone")
  )
})
