setMethod("asJSON", "Date", function(x, Date = c("ISO8601", "epoch"), always_decimal = FALSE, ...) {
  # Validate argument
  Date <- match.arg(Date)

  # select a schema
  output <- switch(
    Date,
    ISO8601 = format(x),
    epoch = unclass(x),
    default = stop("Invalid argument for 'Date':", Date)
  )

  # Dispatch to character encoding
  asJSON(output, always_decimal = FALSE, ...)
})
