# nocov start

gitcreds_get <- NULL
gitcreds_set <- NULL
gitcreds_delete <- NULL
gitcreds_list_helpers <- NULL
gitcreds_cache_envvar <- NULL
gitcreds_fill <- NULL
gitcreds_approve <- NULL
gitcreds_reject <- NULL
gitcreds_parse_output <- NULL

gitcreds <- local({
  # ------------------------------------------------------------------------
  # Public API
  # ------------------------------------------------------------------------

  gitcreds_get <<- function(
    url = "https://github.com",
    use_cache = TRUE,
    set_cache = TRUE
  ) {
    stopifnot(
      is_string(url),
      has_no_newline(url),
      is_flag(use_cache),
      is_flag(set_cache)
    )

    cache_ev <- gitcreds_cache_envvar(url)
    if (use_cache && !is.null(ans <- gitcreds_get_cache(cache_ev))) {
      return(ans)
    }

    check_for_git()

    out <- gitcreds_fill(list(url = url), dummy = TRUE)
    creds <- gitcreds_parse_output(out, url)

    if (set_cache) {
      gitcreds_set_cache(cache_ev, creds)
    }

    creds
  }

  gitcreds_set <<- function(url = "https://github.com") {
    if (!is_interactive()) {
      throw(new_error(
        "gitcreds_not_interactive_error",
        message = "`gitcreds_set()` only works in interactive sessions"
      ))
    }
    stopifnot(is_string(url), has_no_newline(url))
    check_for_git()

    current <- tryCatch(
      gitcreds_get(url, use_cache = FALSE, set_cache = FALSE),
      gitcreds_no_credentials = function(e) NULL
    )

    if (!is.null(current)) {
      gitcreds_set_replace(url, current)
    } else {
      gitcreds_set_new(url)
    }

    msg("-> Removing credentials from cache...")
    gitcreds_delete_cache(gitcreds_cache_envvar(url))

    msg("-> Done.")
    invisible()
  }

  #' Replace credentials with new ones
  #'
  #' It only works interactively, because of `menu()` in `ack()` and
  #' `readline()`.
  #'
  #' We need to set a username, it is compulsory for git credential.
  #' 1. If there was one in the url, then we use that.
  #' 2. Otherwise if git has a username configured for this URL, we use that.
  #' 3. Otherwise we use the username in the credentials we are replacing.
  #'
  #' @param url URL.
  #' @param current Must not be `NULL`, and it must contain a
  #' `gitcreds` object. (Well, a named list, really.)
  #' @noRd
  #' @return Nothing.

  gitcreds_set_replace <- function(url, current) {
    # Potentially take username from the credential we are replacing
    current_username <- current$username

    # Keep warning until there is a credential to replace.
    # In case there are multiple credentials for the same URL.
    while (!is.null(current)) {
      if (!ack(url, current, "Replace")) {
        throw(new_error("gitcreds_abort_replace_error"))
      }

      msg("\n-> Removing current credentials...")
      gitcreds_reject(current)

      current <- tryCatch(
        gitcreds_get(url, use_cache = FALSE, set_cache = FALSE),
        gitcreds_no_credentials = function(e) NULL
      )
      if (!is.null(current)) msg("\n!! Found more matching credentials!")
    }

    msg("")
    pat <- readline("? Enter new password or token: ")

    username <- get_url_username(url) %||%
      gitcreds_username(url) %||%
      current_username

    msg("-> Adding new credentials...")
    gitcreds_approve(list(url = url, username = username, password = pat))

    invisible()
  }

  #' Set new credentials
  #'
  #' This should not replace or remove any old credentials, but of course
  #' we cannot be sure, because credential helpers pretty much do what they
  #' want.
  #'
  #' We need to set a username, it is compulsory for git credential.
  #' 1. If there was one in the url, then we use that.
  #' 2. Otherwise if git has a username configured for this URL, we use that.
  #' 3. Otherwise we use a default username.
  #'
  #' @param url URL.
  #' @noRd
  #' @return Nothing.

  gitcreds_set_new <- function(url) {
    msg("\n")
    pat <- readline("? Enter password or token: ")

    username <- get_url_username(url) %||%
      gitcreds_username(url) %||%
      default_username()

    msg("-> Adding new credentials...")
    gitcreds_approve(list(url = url, username = username, password = pat))

    invisible()
  }

  gitcreds_delete <<- function(url = "https://github.com") {
    if (!is_interactive()) {
      throw(new_error(
        "gitcreds_not_interactive_error",
        message = "`gitcreds_delete()` only works in interactive sessions"
      ))
    }
    stopifnot(is_string(url))
    check_for_git()

    current <- tryCatch(
      gitcreds_get(url, use_cache = FALSE, set_cache = FALSE),
      gitcreds_no_credentials = function(e) NULL
    )

    if (is.null(current)) {
      return(invisible(FALSE))
    }

    if (!ack(url, current, "Delete")) {
      throw(new_error("gitcreds_abort_delete_error"))
    }

    msg("-> Removing current credentials...")
    gitcreds_reject(current)

    msg("-> Removing credentials from cache...")
    gitcreds_delete_cache(gitcreds_cache_envvar(url))

    msg("-> Done.")

    invisible(TRUE)
  }

  gitcreds_list_helpers <<- function() {
    check_for_git()
    out <- git_run(c("config", "--get-all", "credential.helper"))
    clear <- rev(which(out == ""))
    if (length(clear)) out <- out[-(1:clear[1])]
    out
  }

  gitcreds_cache_envvar <<- function(url) {
    pcs <- parse_url(url)
    bad <- is.na(pcs$protocol) | is.na(pcs$host)
    if (any(bad)) {
      stop("Invalid URL(s): ", paste(url[bad], collapse = ", "))
    }

    proto <- sub("^https?_$", "", paste0(pcs$protocol, "_"))
    user <- ifelse(pcs$username != "", paste0(pcs$username, "_AT_"), "")
    host0 <- sub("^api[.]github[.]com$", "github.com", pcs$host)
    host1 <- gsub("[.:]+", "_", host0)
    host <- gsub("[^a-zA-Z0-9_-]", "x", host1)

    slug1 <- paste0(proto, user, host)

    # fix the user name ambiguity, not that it happens often...
    slug2 <- ifelse(grepl("^AT_", slug1), paste0("AT_", slug1), slug1)

    # env vars cannot start with a number
    slug3 <- ifelse(grepl("^[0-9]", slug2), paste0("AT_", slug2), slug2)

    paste0("GITHUB_PAT_", toupper(slug3))
  }

  gitcreds_get_cache <- function(ev) {
    val <- Sys.getenv(ev, NA_character_)
    if (is.na(val) && ev == "GITHUB_PAT_GITHUB_COM") {
      val <- Sys.getenv("GITHUB_PAT", NA_character_)
    }
    if (is.na(val) && ev == "GITHUB_PAT_GITHUB_COM") {
      val <- Sys.getenv("GITHUB_TOKEN", NA_character_)
    }
    if (is.na(val) || val == "") {
      return(NULL)
    }
    if (val == "FAIL" || grepl("^FAIL:", val)) {
      class <- strsplit(val, ":", fixed = TRUE)[[1]][2]
      if (is.na(class)) class <- "gitcreds_no_credentials"
      throw(new_error(class))
    }

    unesc <- function(x) {
      gsub("\\\\(.)", "\\1", x)
    }

    # split on `:` that is not preceded by a `\`
    spval <- strsplit(val, "(?<!\\\\):", perl = TRUE)[[1]]
    spval0 <- unesc(spval)

    # Single field, then the token
    if (length(spval) == 1) {
      return(new_gitcreds(
        protocol = NA_character_,
        host = NA_character_,
        username = NA_character_,
        password = unesc(val)
      ))
    }

    # Two fields? Then it is username:password
    if (length(spval) == 2) {
      return(new_gitcreds(
        protocol = NA_character_,
        host = NA_character_,
        username = spval0[1],
        password = spval0[2]
      ))
    }

    # Otherwise a full record
    if (length(spval) %% 2 == 1) {
      warning(
        "Invalid gitcreds credentials in env var `",
        ev,
        "`. ",
        "Maybe an unescaped ':' character?"
      )
      return(NULL)
    }

    creds <- structure(
      spval0[seq(2, length(spval0), by = 2)],
      names = spval[seq(1, length(spval0), by = 2)]
    )
    do.call("new_gitcreds", as.list(creds))
  }

  gitcreds_set_cache <- function(ev, creds) {
    esc <- function(x) gsub(":", "\\:", x, fixed = TRUE)
    keys <- esc(names(creds))
    vals <- esc(unlist(creds, use.names = FALSE))
    value <- paste0(keys, ":", vals, collapse = ":")
    do.call("set_env", list(structure(value, names = ev)))
    invisible(NULL)
  }

  gitcreds_delete_cache <- function(ev) {
    Sys.unsetenv(ev)
  }

  # ------------------------------------------------------------------------
  # Raw git credential API
  # ------------------------------------------------------------------------

  gitcreds_fill <<- function(input, args = character(), dummy = TRUE) {
    if (dummy) {
      helper <- paste0(
        "credential.helper=\"! echo protocol=dummy;",
        "echo host=dummy;",
        "echo username=dummy;",
        "echo password=dummy\""
      )
      args <- c(args, "-c", helper)
    }

    gitcreds_run("fill", input, args)
  }

  gitcreds_approve <<- function(creds, args = character()) {
    gitcreds_run("approve", creds, args)
  }

  gitcreds_reject <<- function(creds, args = character()) {
    gitcreds_run("reject", creds, args)
  }

  gitcreds_parse_output <<- function(txt, url) {
    if (is.null(txt) || txt[1] == "protocol=dummy") {
      throw(new_error("gitcreds_no_credentials", url = url))
    }
    nms <- sub("=.*$", "", txt)
    vls <- sub("^[^=]+=", "", txt)
    structure(as.list(vls), names = nms, class = "gitcreds")
  }

  #' Run a `git credential` command
  #'
  #' @details
  #' We set the [gitcreds_env()] environment variables, to avoid dialog boxes
  #' from some credential helpers and also validation that potentiall needs
  #' an internet connection.
  #'
  #' @param command Command name, e.g. `"fill"`.
  #' @param input Named list of input, see
  #' https://git-scm.com/docs/git-credential#IOFMT
  #' @param args Extra command line arguments, added after `git` and
  #' _before_ `command`, to allow `git -c ... fill`.
  #' @return Standard output, line by line.
  #'
  #' @noRd
  #' @seealso [git_run()].

  gitcreds_run <- function(command, input, args = character()) {
    env <- gitcreds_env()
    oenv <- set_env(env)
    on.exit(set_env(oenv), add = TRUE)

    stdin <- create_gitcreds_input(input)

    git_run(c(args, "credential", command), input = stdin)
  }

  # ------------------------------------------------------------------------
  # Helpers specific to git
  # ------------------------------------------------------------------------

  #' Run a git command
  #'
  #' @details
  #' Currently we don't set the credential specific environment variables
  #' here, and credential helpers invoked by `git` behave the same way as
  #' they would from the command line.
  #'
  #' ## Errors
  #'
  #' On error `git_run()` returns an error with class `git_error` and
  #' also `gitcreds_error`. The error object includes
  #' * `args` the command line arguments,
  #' * `status`: the exit status of the command,
  #' * `stdout`: the standard output of the command, line by line.
  #' * `stderr`: the standard error of the command, line by line.
  #'
  #' @param args Command line arguments.
  #' @param input The standard input (the `input` argument of [system2()].
  #' @noRd
  #' @return Standard output, line by line.

  git_run <- function(args, input = NULL) {
    stderr_file <- tempfile("gitcreds-stderr-")
    on.exit(unlink(stderr_file, recursive = TRUE), add = TRUE)
    if (!is.null(input)) {
      stdin_file <- tempfile("gitcreds-stdin-")
      on.exit(unlink(stdin_file, recursive = TRUE), add = TRUE)
      writeBin(charToRaw(input), stdin_file)
      stdin <- stdin_file
    } else {
      stdin <- ""
    }
    out <- tryCatch(
      suppressWarnings(system2(
        "git",
        args,
        stdin = stdin,
        stdout = TRUE,
        stderr = stderr_file
      )),
      error = function(e) NULL
    )

    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
      throw(new_git_error(
        "git_error",
        args = args,
        stdout = out,
        status = attr(out, "status"),
        stderr = read_file(stderr_file)
      ))
    }

    out
  }

  #' Request confirmation from the user, to replace or delete credentials
  #'
  #' This function only works in interactive sessions.
  #'
  #' @param url URL to delete or set new credentials for.
  #' @param current The current credentials.
  #' @return `FALSE` is the user changed their mind, to keep the current
  #' credentials. `TRUE` for replacing/deleting them.
  #'
  #' @noRd
  #' @seealso [gitcreds_set()].

  ack <- function(url, current, what = "Replace") {
    msg("\n-> Your current credentials for ", squote(url), ":\n")
    msg(paste0(format(current, header = FALSE), collapse = "\n"), "\n")

    choices <- c(
      "Abort update with error, and keep the existing credentials",
      paste(what, "these credentials"),
      if (has_password(current)) "See the password / token"
    )

    repeat {
      ch <- utils::menu(title = "-> What would you like to do?", choices)

      if (ch == 1) return(FALSE)
      if (ch == 2) return(TRUE)

      msg("\nCurrent password: ", current$password, "\n\n")
    }
  }

  #' Whether a `gitcreds` credential has a non-empty `password`
  #'
  #' This is usually `TRUE`.
  #'
  #' @param creds `gitcreds`
  #' @noRd
  #' @return `TRUE` is there is a `password`

  has_password <- function(creds) {
    is_string(creds$password) && creds$password != ""
  }

  #' Create a string that can be passed as standard input to `git credential`
  #' commands
  #'
  #' @param args Usually a `gitcreds` object, but can be a named list in
  #' general. This is a format: https://git-scm.com/docs/git-credential#IOFMT
  #' @noRd
  #' @return String.

  create_gitcreds_input <- function(args) {
    paste0(
      paste0(names(args), "=", args, collapse = "\n"),
      "\n\n"
    )
  }

  #' Environment to set for all `git credential` commands.
  #' @noRd
  #' @return Named character vector.

  gitcreds_env <- function() {
    # Avoid interactivity and validation with some common credential helpers
    c(
      GCM_INTERACTIVE = "Never",
      GCM_MODAL_PROMPT = "false",
      GCM_VALIDATE = "false",
      GCM_GUI_PROMPT = "false"
    )
  }

  #' Check if `git` is installed and can run
  #'
  #' If not installed, a `gitcreds_nogit_error` is thrown.
  #'
  #' @noRd
  #' @return Nothing

  check_for_git <- function() {
    # This is simpler than Sys.which(), and also less fragile
    has_git <- tryCatch(
      {
        suppressWarnings(system2(
          "git",
          "--version",
          stdout = TRUE,
          stderr = null_file()
        ))
        TRUE
      },
      error = function(e) FALSE
    )

    if (!has_git) throw(new_error("gitcreds_nogit_error"))
  }

  #' Query the `username` to use for `git config credential`
  #'
  #' @details
  #' The current working directory matters for this command, as you can
  #' configure `username` in a local `.git/config` file (via
  #' `git config --local`).
  #'
  #' @param url URL to query the username for, or `NULL`. If not `NULL`,
  #' then we first try to query an URL-specific username. See
  #' https://git-scm.com/docs/gitcredentials for more about URL-specific
  #' credential config
  #' @noRd
  #' @return A string with the username, or `NULL` if no default was found.

  gitcreds_username <- function(url = NULL) {
    gitcreds_username_for_url(url) %||% gitcreds_username_generic()
  }

  gitcreds_username_for_url <- function(url) {
    if (is.null(url)) return(NULL)
    tryCatch(
      git_run(c(
        "config",
        "--get-urlmatch",
        "credential.username",
        shQuote(url)
      )),
      git_error = function(err) {
        if (err$status == 1) NULL else throw(err)
      }
    )
  }

  gitcreds_username_generic <- function() {
    tryCatch(
      git_run(c("config", "credential.username")),
      git_error = function(err) {
        if (err$status == 1) NULL else throw(err)
      }
    )
  }

  #' User name to use when creating a credential, if there is nothing better
  #'
  #' These user names are typical for some git tools, e.g.
  #' [Git Credential Manager for Windows](http://microsoft.github.io/Git-Credential-Manager-for-Windows/)
  #' (`manager`) and
  #' [Git Credential Manager Core](https://github.com/Microsoft/Git-Credential-Manager-Core)
  #' (`manager-core`).
  #'
  #' @noRd
  #' @return Character string

  default_username <- function() {
    "PersonalAccessToken"
  }

  new_gitcreds <- function(...) {
    structure(list(...), class = "gitcreds")
  }

  # ------------------------------------------------------------------------
  # Errors
  # ------------------------------------------------------------------------

  gitcred_errors <- function() {
    c(
      git_error = "System git failed",
      gitcreds_nogit_error = "Could not find system git",
      gitcreds_not_interactive_error = "gitcreds needs an interactive session",
      gitcreds_abort_replace_error = "User aborted updating credentials",
      gitcreds_abort_delete_error = "User aborted deleting credentials",
      gitcreds_no_credentials = "Could not find any credentials",
      gitcreds_no_helper = "No credential helper is set",
      gitcreds_multiple_helpers = "Multiple credential helpers, only using the first",
      gitcreds_unknown_helper = "Unknown credential helper, cannot list credentials"
    )
  }

  new_error <- function(class, ..., message = "", call. = TRUE, domain = NULL) {
    if (message == "") message <- gitcred_errors()[[class]]
    message <- .makeMessage(message, domain = domain)
    cond <- list(message = message, ...)
    if (call.) cond$call <- sys.call(-1)
    class(cond) <- c(class, "gitcreds_error", "error", "condition")
    cond
  }

  new_git_error <- function(class, ..., stderr) {
    cond <- new_error(class, ..., stderr = stderr)
    cond$message <- paste0(cond$message, ": ", stderr)
    cond
  }

  new_warning <- function(
    class,
    ...,
    message = "",
    call. = TRUE,
    domain = NULL
  ) {
    if (message == "") message <- gitcred_errors()[[class]]
    message <- .makeMessage(message, domain = domain)
    cond <- list(message = message, ...)
    if (call.) cond$call <- sys.call(-1)
    class(cond) <- c(class, "gitcreds_warning", "warning", "condition")
    cond
  }

  throw <- function(cond) {
    cond
    if ("error" %in% class(cond)) {
      stop(cond)
    } else if ("warning" %in% class(cond)) {
      warning(cond)
    } else if ("message" %in% class(cond)) {
      message(cond)
    } else {
      signalCondition(cond)
    }
  }

  # ------------------------------------------------------------------------
  # Genetic helpers
  # ------------------------------------------------------------------------

  #' Set/remove env var and return the old values
  #'
  #' @param envs Named character vector or list of env vars to set. `NA`
  #' values will un-set an env var.
  #' @noRd
  #' @return Character vector, the old values of the supplied environment
  #' variables, `NA` for the ones that were not set.

  set_env <- function(envs) {
    current <- Sys.getenv(names(envs), NA_character_, names = TRUE)
    na <- is.na(envs)
    if (any(na)) {
      Sys.unsetenv(names(envs)[na])
    }
    if (any(!na)) {
      do.call("Sys.setenv", as.list(envs[!na]))
    }
    invisible(current)
  }

  #' Get the user name from a `protocol://username@host/path` URL.
  #'
  #' @param url URL
  #' @noRd
  #' @return String or `NULL` if `url` does not have a username.

  get_url_username <- function(url) {
    nm <- parse_url(url)$username
    if (nm == "") NULL else nm
  }

  #' Parse URL
  #'
  #' It does not parse query parameters, as we don't deal with them here.
  #' The port number is included in the host name, if present.
  #'
  #' @param url Character vector of one or more URLs.
  #' @noRd
  #' @return Data frame with string columns: `protocol`, `username`,
  #' `password`, `host`, `path`.

  parse_url <- function(url) {
    re_url <- paste0(
      "^(?<protocol>[a-zA-Z0-9]+)://",
      "(?:(?<username>[^@/:]+)(?::(?<password>[^@/]+))?@)?",
      "(?<host>[^/]+)",
      "(?<path>.*)$" # don't worry about query params here...
    )

    mch <- re_match(url, re_url)
    mch[, setdiff(colnames(mch), c(".match", ".text")), drop = FALSE]
  }

  is_string <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x)
  }

  is_flag <- function(x) {
    is.logical(x) && length(x) == 1 && !is.na(x)
  }

  has_no_newline <- function(url) {
    !grepl("\n", url, fixed = TRUE)
  }

  # From the rematch2 package

  re_match <- function(text, pattern, perl = TRUE, ...) {
    stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
    text <- as.character(text)

    match <- regexpr(pattern, text, perl = perl, ...)

    start <- as.vector(match)
    length <- attr(match, "match.length")
    end <- start + length - 1L

    matchstr <- substring(text, start, end)
    matchstr[start == -1] <- NA_character_

    res <- data.frame(
      stringsAsFactors = FALSE,
      .text = text,
      .match = matchstr
    )

    if (!is.null(attr(match, "capture.start"))) {
      gstart <- attr(match, "capture.start")
      glength <- attr(match, "capture.length")
      gend <- gstart + glength - 1L

      groupstr <- substring(text, gstart, gend)
      groupstr[gstart == -1] <- NA_character_
      dim(groupstr) <- dim(gstart)

      res <- cbind(groupstr, res, stringsAsFactors = FALSE)
    }

    names(res) <- c(attr(match, "capture.names"), ".text", ".match")
    res
  }

  null_file <- function() {
    if (get_os() == "windows") "nul:" else "/dev/null"
  }

  get_os <- function() {
    if (.Platform$OS.type == "windows") {
      "windows"
    } else if (Sys.info()[["sysname"]] == "Darwin") {
      "macos"
    } else if (Sys.info()[["sysname"]] == "Linux") {
      "linux"
    } else {
      "unknown"
    }
  }

  `%||%` <- function(l, r) if (is.null(l)) r else l

  #' Like [message()], but print to standard output in interactive
  #' sessions
  #'
  #' To avoid red output in RStudio, RGui, and R.app.
  #'
  #' @inheritParams message
  #' @noRd
  #' @return Nothing

  msg <- function(..., domain = NULL, appendLF = TRUE) {
    cnd <- .makeMessage(..., domain = domain, appendLF = appendLF)
    withRestarts(muffleMessage = function() NULL, {
      signalCondition(simpleMessage(cnd))
      output <- default_output()
      cat(cnd, file = output, sep = "")
    })
    invisible()
  }

  #' Where to print messages to
  #'
  #' If the session is not interactive, then it potentially matters
  #' whether we print to stdout or stderr, so we print to stderr.
  #'
  #' The same applies when there is a sink for stdout or stderr.
  #'
  #' @noRd
  #' @return The connection to print to.

  default_output <- function() {
    if (is_interactive() && no_active_sink()) stdout() else stderr()
  }

  no_active_sink <- function() {
    # See ?sink.number for the explanation
    sink.number("output") == 0 && sink.number("message") == 2
  }

  #' Smarter `interactive()`
  #'
  #' @noRd
  #' @return Logical scalar.

  is_interactive <- function() {
    opt <- getOption("rlib_interactive")
    opt2 <- getOption("rlang_interactive")
    if (isTRUE(opt)) {
      TRUE
    } else if (identical(opt, FALSE)) {
      FALSE
    } else if (isTRUE(opt2)) {
      TRUE
    } else if (identical(opt2, FALSE)) {
      FALSE
    } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
      FALSE
    } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
      FALSE
    } else {
      base::interactive()
    }
  }

  #' Squote wrapper to avoid smart quotes
  #'
  #' @inheritParams sQuote
  #' @inherit sQuote return
  #' @noRd
  #' @return Character vector.

  squote <- function(x) {
    old <- options(useFancyQuotes = FALSE)
    on.exit(options(old), add = TRUE)
    sQuote(x)
  }

  #' Read all of a file
  #'
  #' @param path File to read.
  #' @param ... Passed to [readChar()].
  #' @noRd
  #' @return String.

  read_file <- function(path, ...) {
    readChar(path, nchars = file.info(path)$size, ...)
  }

  environment()
})

# nocov end
