
ps__invalid_argument <- function(arg, ...) {
  msg <- paste0(encodeString(arg, quote = "`"), ...)
  structure(
    list(message = msg),
    class = c("invalid_argument", "error", "condition"))
}
