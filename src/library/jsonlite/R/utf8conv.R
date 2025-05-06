utf8conv <- function(x) {
  gsub("<U\\+([0-9A-F]{4})>", "\\\\u\\1", x)
}
