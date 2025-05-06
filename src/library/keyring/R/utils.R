utf8 <- function(x) {
  if (is.null(x)) return(x)
  iconv(x, "", "UTF-8")
}

`%||%` <- function(l, r) if (is.null(l)) r else l

cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

confirmation <- function(prompt, yes) {
  ans <- readline(paste0(prompt, ": "))
  if (ans != yes) stop("Aborted", call. = FALSE)
}

darwin_version <- function() {
  info <- Sys.info()
  if (info[["sysname"]] != "Darwin") stop("Not macOS")
  package_version(info[["release"]])
}

file_stamp <- function(x) {
  as.character(tools::md5sum(x))
}

str_starts_with <- function(x, p) {
  ncp <- nchar(p)
  substr(x, 1, nchar(p)) == p
}

URLencode <- function(URL) {
  good <- c(LETTERS, letters, 0:9, ".", "_", "~", "-")
  x <- strsplit(URL, "")[[1L]]
  bad <- which(!x %in% good)
  tr <- function(x) {
    paste0("%", toupper(as.character(charToRaw(x))), collapse = "")
  }
  if (length(bad)) x[bad] <- vapply(x[bad], tr, character(1))
  paste(x, collapse = "")
}

get_encoding_opt <- function() {
  chk <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

  enc <- getOption("keyring.encoding_windows")
  if (!is.null(enc) && !chk(enc)) {
    stop(
      "Invalid 'keyring.encoding_windows' option, must be an ",
      "encoding name or 'auto'"
    )
  }

  enc <- enc %||% Sys.getenv("KEYRING_ENCODING_WINDOWS", "auto")

  # Confirm valid encoding. Suggest closest match if not found.
  if (enc != "auto" & !(tolower(enc) %in% tolower(iconvlist()))) {
    icl <- iconvlist()
    closest <- icl[which.min(utils::adist(enc, icl))]
    stop(sprintf(
      "Encoding not found in iconvlist(). Did you mean %s?",
      closest
    ))
  }
  enc
}

is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (
    tolower(getOption("rstudio.notebook.executing", "false")) == "true"
  ) {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    interactive()
  }
}

base64_decode <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(paste(gsub("\\s+", "", x), collapse = ""))
  }
  .Call(keyring_base64_decode, x)
}

base64_encode <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(paste(x, collapse = ""))
  }
  rawToChar(.Call(keyring_base64_encode, x))
}

sha256 <- function(x, key = NULL) {
  if (is.character(key)) {
    key <- base64_decode(key)
  }
  stopifnot(is.null(key) || is.raw(key))
  if (!is.null(key)) {
    block_size <- 64L
    if (length(key) > block_size) {
      key <- .Call(keyring_sha256, key, TRUE)
    } else if (length(key) < block_size) {
      key <- c(key, rep(raw(1), block_size - length(key)))
    }

    opad <- as.raw(bitwXor(as.integer(key), as.integer(0x5c)))
    ipad <- as.raw(bitwXor(as.integer(key), as.integer(0x36)))

    .Call(
      keyring_sha256,
      c(opad, .Call(keyring_sha256, c(ipad, x), TRUE)),
      TRUE
    )
  } else {
    .Call(keyring_sha256, x, TRUE)
  }
}

rand_bytes <- function(n = 1) {
  sodium_random(n)
}

aes_cbc_encrypt <- function(data, key, iv = rand_bytes(16)) {
  data <- path_or_raw(data)
  if (!is.raw(data)) {
    stop("The 'data' must path to a file or raw vector")
  }
  out <- .Call(keyring_aes_cbc_encrypt, data, key, iv)
  structure(out, iv = iv)
}

aes_cbc_decrypt <- function(data, key, iv = attr(data, "iv")) {
  data <- path_or_raw(data)
  if (!is.raw(data)) {
    stop("The 'data' must path to a file or raw vector")
  }
  .Call(keyring_aes_cbc_decrypt, data, key, iv)
}

path_or_raw <- function(x) {
  if (is.raw(x)) return(x)
  if (is.character(x) && length(x) == 1) {
    path <- normalizePath(x, mustWork = TRUE)
    bin <- readBin(path, raw(), file.info(path)$size)
    return(bin)
  }
  stop("`data` must be raw data vector or path to file on disk.")
}
