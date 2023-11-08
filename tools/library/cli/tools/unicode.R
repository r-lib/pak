# To create the data:

update_wide_unicode_data <- function() {
  tab <- read.delim(
    stringsAsFactors = FALSE,
    "https://unicode.org/Public/UNIDATA/EastAsianWidth.txt",
    comment.char = "#",
    sep = ";",
    strip.white = TRUE,
    header = FALSE
  )

  # Keep wide ones
  wide <- tab$V1[tab$V2 == "W"]
  first <- sub("\\.\\..*$", "", wide)
  range <- sub("^([0-9A-F]+)\\.\\.([0-9A-F]+)$", "\\\\U\\1-\\\\U\\2", wide)
  range <- sub("^([0-9A-F]+)$", "\\\\U\\1", range)

  wide_chars <- data.frame(
    stringsAsFactors = FALSE,
    test = sapply(parse(text = paste0('"', "\\U", first, '"')), eval),
    regex = sapply(parse(text = paste0('"', range, '"')), eval)
  )

  env <- new.env(parent = emptyenv())
  load("R/sysdata.rda", envir = env)

  env$wide_chars <- wide_chars
  save(list = ls(env), file = "R/sysdata.rda", envir = env, version = 2)
}
