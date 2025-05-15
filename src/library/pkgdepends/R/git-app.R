# TODO: allow restriting to dumb, v1, v2 protocol

git_app <- function(
  git_root,
  git_timeout = as.difftime(1, units = "mins"),
  filter = TRUE,
  cleanup = TRUE
) {
  app <- webfakes::new_app()
  app$locals$git_root <- git_root
  app$locals$git_timeout <- as.double(git_timeout, units = "secs") * 1000
  app$locals$git_config <- tempfile()

  reg.finalizer(app, function(app0) unlink(app$locals$git_config), TRUE)
  writeLines(
    c(
      "[uploadpack]",
      paste0("\tallowFilter = ", if (isTRUE(filter)) "true" else "false")
    ),
    app$locals$git_config
  )

  if (cleanup) {
    reg.finalizer(
      app,
      function(app) unlink(app$locals$git_root, recursive = TRUE),
      TRUE
    )
  }

  app$get(webfakes::new_regexp("^(?<path>.*)$"), function(req, res) {
    out <- tempfile()
    err <- tempfile()
    on.exit(unlink(c(out, err)), add = TRUE)
    px <- processx::process$new(
      "git",
      "http-backend",
      env = git_env_vars(req),
      stdout = out,
      stderr = err
    )
    px$wait(req$app$locals$git_timeout)
    parse_cgi_output(px, out, err, res)
  })

  app$post(webfakes::new_regexp("^(?<path>.*)$"), function(req, res) {
    tmp <- tempfile()
    out <- tempfile()
    err <- tempfile()
    on.exit(unlink(c(out, err, tmp)), add = TRUE)
    writeBin(req$.body, tmp)
    px <- processx::process$new(
      "git",
      "http-backend",
      env = git_env_vars(req),
      stdin = tmp,
      stdout = out,
      stderr = err
    )
    px$wait(req$app$locals$git_timeout)
    parse_cgi_output(px, out, err, res)
  })

  app
}

git_env_vars <- function(req) {
  url <- parse_url(req$url)
  c(
    "current",

    # For git
    GIT_CONFIG_GLOBAL = req$app$locals$git_config,
    GIT_HTTP_EXPORT_ALL = "true",
    GIT_PROJECT_ROOT = req$app$locals$git_root,
    GIT_PROTOCOL = req$get_header("Git-Protocol") %||% "",
    HTTP_GIT_PROTOCOL = req$get_header("Git-Protocol") %||% "",

    # general CGI
    CONTENT_LENGTH = if (length(req$.body)) length(req$.body),
    CONTENT_TYPE = req$get_header("content-type") %||% "",
    GATEWAY_INTERFACE = "CGI/1.1",
    PATH_INFO = req$path,
    QUERY_STRING = req$query_string,
    REMOTE_ADDR = req$remote_addr,
    REMOTE_HOST = req$remote_addr,
    REMOTE_USER = "anonymous",
    REQUEST_METHOD = toupper(req$method),
    SERVER_NAME = url$host,
    SERVER_PORT = url$port,
    SERVER_PROTOCOL = paste0("http/", req$http_version),
    SERVER_SOFTWARE = "https://github.com/r-lib/webfakes"
  )
}

parse_cgi_output <- function(px, out, err, res) {
  if (px$is_alive() || px$get_exit_status() != 0) {
    px$kill()
    res$set_status(500)$send(paste0("Internal git error: ", err))
  }

  out <- read_bin(out)
  err <- read_char(err)

  cgi_res <- split_cgi_output(out)
  headers <- cgi_res$headers

  for (idx in seq_along(headers)) {
    if (tolower(names(headers)[idx]) == "status") {
      res$set_status(parse_status(headers[[idx]]))
    } else {
      res$set_header(names(headers)[idx], headers[[idx]])
    }
  }

  if (!"status" %in% names(headers)) {
    res$set_status(200L)
  }

  res$send(cgi_res$body)
}

split_cgi_output <- function(x) {
  nlnl <- grepRaw("\r?\n\r?\n", x)[1]
  if (is.na(nlnl)) {
    stop("Invalid response from git cgi, no headers?")
  }

  headers <- parse_headers(rawToChar(x[1:(nlnl - 1L)]))

  body <- x[nlnl:length(x)]
  ndrop <- 1L
  while (body[ndrop] != 0x0a) ndrop <- ndrop + 1L
  ndrop <- ndrop + 1L
  while (body[ndrop] != 0x0a) ndrop <- ndrop + 1L
  body <- utils::tail(body, -ndrop)

  list(headers = headers, body = body)
}

parse_status <- function(x) {
  status <- as.integer(strsplit(x, " ", fixed = TRUE)[[1]][1])
  if (is.na(status)) {
    stop("Invalid status from git cgi: ", x)
  }
}

read_bin <- function(path) {
  readBin(path, "raw", file.info(path)$size)
}

parse_headers <- function(txt) {
  headers <- grep(":", parse_headers0(txt), fixed = TRUE, value = TRUE)
  out <- lapply(headers, split_header)
  names <- tolower(vapply(out, `[[`, character(1), 1))
  values <- lapply(lapply(out, `[[`, 2), trimws)
  names(values) <- names
  values
}

parse_headers0 <- function(txt, multiple = FALSE) {
  if (!length(txt)) return(NULL)
  if (is.raw(txt)) {
    txt <- rawToChar(txt)
  }
  stopifnot(is.character(txt))
  if (length(txt) > 1) {
    txt <- paste(txt, collapse = "\n")
  }
  sets <- strsplit(txt, "\\r\\n\\r\\n|\\n\\n|\\r\\r")[[1]]
  headers <- strsplit(sets, "\\r\\n|\\n|\\r")
  if (multiple) {
    headers
  } else {
    headers[[length(headers)]]
  }
}

split_header <- function(x) {
  pos <- grepRaw(":", x, fixed = TRUE)[1]
  if (is.na(pos)) {
    stop("Invalid response header from git cgi: ", x)
  }
  c(substr(x, 1, pos - 1L), substr(x, pos + 1L, nchar(x)))
}

parse_url <- function(url) {
  re_url <- paste0(
    "^(?<protocol>[a-zA-Z0-9]+)://",
    "(?:(?<username>[^@/:]+)(?::(?<password>[^@/]+))?@)?",
    "(?<url>(?<host>[^:/]+)",
    "(?::(?<port>[0-9]+))?",
    "(?<path>/.*))$" # don't worry about query params here...
  )
  re_match(url, re_url)
}
