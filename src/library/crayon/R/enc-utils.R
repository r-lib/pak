
# keep encoding, even if useBytes = TRUE

Encoding_ <- function(x) {
  if (is.factor(x) && length(levels(x)) < length(x)) {
    Encoding(levels(x))
  } else {
    if (!is.character(x)) x <- as.character(x)
    Encoding(x)
  }
}

sub_ <- function(pattern, replacement, x, ...) {
  enc <- Encoding_(x)
  ret <- sub(pattern, replacement, x, ...)
  if (length(ret)) Encoding(ret) <- enc
  ret
}

gsub_ <- function(pattern, replacement, x, ...) {
  enc <- Encoding_(x)
  ret <- gsub(pattern, replacement, x, ...)
  if (length(ret)) Encoding(ret) <- enc
  ret
}

strsplit_ <- function(x, ...) {
  enc <- Encoding_(x)
  ret <- strsplit(x, ...)
  for (i in seq_along(ret)) {
    Encoding(ret[[i]]) <- enc[i]
  }
  ret
}
