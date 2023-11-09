setMethod("asJSON", "POSIXt", function(x, POSIXt = c("string", "ISO8601", "epoch",
  "mongo"), UTC = FALSE, digits, time_format = NULL, always_decimal = FALSE, ...) {
  # note: UTC argument doesn't seem to be working consistently maybe use ?format
  # instead of ?as.character

  # Validate
  POSIXt <- match.arg(POSIXt)

  # Encode based on a schema
  if (POSIXt == "mongo") {
    return(asJSON_posix_mongo(x, ...))
  }

  # Epoch millis
  if (POSIXt == "epoch") {
    return(asJSON(floor(unclass(as.POSIXct(x)) * 1000), digits = digits, always_decimal = FALSE, ...))
  }

  # Strings
  if(is.null(time_format)){
    time_format <- if(POSIXt == "string"){
      ""
    } else if(isTRUE(UTC)){
      "%Y-%m-%dT%H:%M:%SZ"
    } else {
      "%Y-%m-%dT%H:%M:%S"
    }
  }

  if (isTRUE(UTC)) {
    asJSON(format(x, format = time_format, tz = "UTC"), ...)
  } else {
    asJSON(format(x, format = time_format), ...)
  }
})

asJSON_posix_mongo <- function(x, collapse = TRUE, indent = NA_integer_, ...){
  if (inherits(x, "POSIXlt")) {
    x <- as.POSIXct(x)
  }
  df <- data.frame("$date" = floor(unclass(x) * 1000), check.names = FALSE)
  if(inherits(x, "scalar"))
    class(df) <- c("scalar", class(df))
  tmp <- asJSON(df, digits = NA, always_decimal = FALSE, ..., collapse = FALSE)
  tmp[is.na(x)] <- asJSON(NA_character_, collapse = FALSE, ...)
  if(isTRUE(collapse)){
    collapse(tmp, inner = FALSE, indent = indent)
  } else {
    tmp
  }
}
