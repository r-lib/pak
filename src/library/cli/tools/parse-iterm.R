parse_iterm <- function(path) {
  doc <- xml2::read_xml(path)
  elm <- xml2::xml_find_first(doc, "/plist/dict")
  key <- sapply(
    xml2::xml_find_all(elm, "/plist/dict/key"),
    xml2::xml_text
  )
  val <- xml2::xml_find_all(elm, "/plist/dict/dict")

  get <- function(k) {
    wh <- match(k, key)
    v <- val[[wh]]
    vks <- sapply(
      xml2::xml_find_all(v, "key"),
      xml2::xml_text
    )
    if (!"Color Space" %in% vks) stop("Unknown color space")
    chd <- xml2::xml_children(v)
    csp <- xml2::xml_text(
      chd[[which(vks == "Color Space") * 2]]
    )
    if (csp != "sRGB") stop("Color space is not sRGB")
    r <- as.numeric(xml2::xml_text(chd[[which(vks == "Red Component") * 2]]))
    g <- as.numeric(xml2::xml_text(chd[[which(vks == "Green Component") * 2]]))
    b <- as.numeric(xml2::xml_text(chd[[which(vks == "Blue Component") * 2]]))
    tolower(grDevices::rgb(r, g, b))
  }

  rn <- sub(".itermcolors", "", basename(path), fixed = TRUE)
  data.frame(
    stringsAsFactors = FALSE,
    row.names = paste0("iterm-", rn),
    black = get("Ansi 0 Color"),
    red = get("Ansi 1 Color"),
    green = get("Ansi 2 Color"),
    yellow = get("Ansi 3 Color"),
    blue = get("Ansi 4 Color"),
    magenta = get("Ansi 5 Color"),
    cyan = get("Ansi 6 Color"),
    white = get("Ansi 7 Color"),
    bblack = get("Ansi 8 Color"),
    bred = get("Ansi 9 Color"),
    bgreen = get("Ansi 10 Color"),
    byellow = get("Ansi 11 Color"),
    bblue = get("Ansi 12 Color"),
    bmagenta = get("Ansi 13 Color"),
    bcyan = get("Ansi 14 Color"),
    bwhite = get("Ansi 15 Color")
  )
}

parse_all <- function(dir = "tools") {
  paths <- dir(dir, pattern = "[.]itermcolors$", full.names = TRUE)
  cols <- lapply(paths, parse_iterm)
  all <- do.call(rbind, cols)
  write.table(all, "tools/ansi-iterm-themes.txt", quote = FALSE)
}

if (is.null(sys.calls())) {
  parse_all()
}
