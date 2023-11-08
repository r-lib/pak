
# Result is underined if not Linux

current_r_platform_data_linux <- function(raw, etc = "/etc") {
  os <- tryCatch(
    suppressWarnings(readLines(file.path(etc, "os-release"))),
    error = function(e) NULL
  )
  rh <- tryCatch(
    suppressWarnings(readLines(file.path(etc, "redhat-release"))),
    error = function(e) NULL
  )

  cbind(raw, parse_linux_platform_info(os, rh))
}

unknown_dist <- function() {
  data.frame(
    stringsAsFactors = FALSE,
    distribution = "unknown"
  )
}

parse_linux_platform_info <- function(os_release = NULL,
                                      redhat_release = NULL) {
  if (is.null(os_release) &&
      is.null(redhat_release)) {
    unknown_dist()

  } else if (!is.null(os_release)) {
    parse_os_release(os_release)

  } else {
    parse_redhat_release(redhat_release)
  }
}

is_quoted <- function(x) {
  l <- nchar(x)
  l >= 2 &&
    substr(x, 1, 1) %in% c("'", '"') &&
    substr(x, 1, 1) == substr(x, l, l)
}

remove_quotes <- function(x) {
  l <- nchar(x)
  if (l < 2) {
    x
  } else {
    substr(x, 2, l - 1)
  }
}

parse_os_release <- function(lines) {
  id <- grep("^ID=", lines, value = TRUE)[1]
  if (is.na(id)) return(unknown_dist())
  id <- trimws(sub("^ID=(.*)$", "\\1", id, perl = TRUE))
  if (is_quoted(id)) id <- remove_quotes(id)

  ver <- grep("^VERSION_ID=", lines, value = TRUE)[1]
  if (!is.na(ver)) {
    ver <- trimws(sub("VERSION_ID=(.*)$", "\\1", ver, perl = TRUE))
    if (is_quoted(ver)) ver <- remove_quotes(ver)
  }

  out <- data.frame(
    stringsAsFactors = FALSE,
    distribution = id
  )
  if (!is.na(ver)) out$release <- ver

  if (is.na(ver) && id == "debian") {
    pn <- grep("^PRETTY_NAME=", lines, value = TRUE)[1]
    if (!is.na(pn) && grepl("/sid\"?$", pn)) {
      out$release <- "unstable"
    }
  }

  out
}

parse_redhat_release <- function(lines) {
  pcs <- strsplit(lines[1], " ", fixed = TRUE)[[1]]
  id <- tolower(pcs[1])
  if (id == "" || is.na(id)) return(unknown_dist())

  wver <- grepl("^[-\\.0-9]+$", pcs)

  out <- data.frame(
    stringsAsFactors = FALSE,
    distribution = id
  )
  if (any(wver)) out$release <- pcs[wver][1]

  out
}
