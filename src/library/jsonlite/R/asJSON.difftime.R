setMethod("asJSON", "ts", function(x, ...) {
  asJSON(as.vector(x), ...)
})

setMethod("asJSON", "hms", function(x, hms = c("string", "secs"), ...) {
  hms <- match.arg(hms)
  output <- switch(hms, string = as.character(x), secs = as.numeric(x, units = "secs"))
  output[is.na(x)] <- NA
  asJSON(output, ...)
})
