
read_dcf <- function(file) {
  lines <- readLines(file)

  con <- textConnection(lines, local = TRUE)
  fields <- colnames(read.dcf(con))
  close(con)

  if (!length(fields)) {
    return(list(
      dcf = create_fields(character(), character()),
      notws = character()
    ))
  }

  con <- textConnection(lines, local = TRUE)
  res <- read.dcf(con, keep.white = fields)
  close(con)

  if (nrow(res) > 1) {
    stop("Empty lines found in DESCRIPTION file", call. = FALSE)
  }

  con <- textConnection(lines, local = TRUE)
  res2 <- read.dcf(con, keep.white = fields, all = TRUE)
  close(con)

  if (any(mismatch <- res != res2)) {
    stop("Duplicate DESCRIPTION fields: ",
         paste(sQuote(colnames(res)[mismatch]), collapse = ", "))
  }

  if ("Encoding" %in% colnames(res)) {
    encoding <- res[, "Encoding"]
    Encoding(res) <- encoding
    res[] <- enc2utf8(res)
    Encoding(lines) <- encoding
    lines <- enc2utf8(lines)
  }

  no_tws_fields <- sub(
    ":$",
    "",
    grep("^[^\\s]+:$", lines, perl = TRUE, value = TRUE)
  )

  notws <- res[1, match(no_tws_fields, fields)]

  list(
    dcf = create_fields(fields, res[1, ]),
    notws = notws
  )
}
