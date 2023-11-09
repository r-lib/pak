
# keep encoding, even if useBytes = TRUE

sub_ <- function(pattern, replacement, x, ...) {
  enc <- Encoding(x)
  ret <- sub(pattern, replacement, x, ...)
  if (length(ret)) Encoding(ret) <- enc
  ret
}

gsub_ <- function(pattern, replacement, x, ...) {
  enc <- Encoding(x)
  ret <- gsub(pattern, replacement, x, ...)
  if (length(ret)) Encoding(ret) <- enc
  ret
}

strsplit_ <- function(x, ...) {
  enc <- Encoding(x)
  ret <- strsplit(x, ...)
  for (i in seq_along(ret)) {
    Encoding(ret[[i]]) <- enc[i]
  }
  ret
}
