
transformer <- function(text, envir) {
  paste0("\\Sexpr[results=rd,stage=render]{", text, "}")
}

do <- function() {
  lns <- paste(readLines("man/raw/dynamic.Rd.in"), collapse = "\n")
  lns <- glue::glue(
    lns,
    .open = "<",
    .close = ">",
    .transformer = transformer
  )
  lns <- strsplit(lns, "\n", fixed = TRUE)[[1]]
  writeLines(lns, "man/dynamic.Rd")
}

if (is.null(sys.calls())) do()
