fixNativeSymbol <- function(symbol) {
  if (is(symbol, "NativeSymbolInfo")) {
    # method depends on version
    rVersion <- getRversion()

    if (rVersion >= "3.0") {
      # in R 3.0 determine the dll that the symbol lives in
      name <- ifelse(is.null(symbol$package), symbol$dll[["name"]], symbol$package[["name"]])

      # load package if not yet loaded
      try(getNamespace(name))
      pkgDLL <- getLoadedDLLs()[[name]]

      # reconstruct the native symbol address
      newsymbol <- getNativeSymbolInfo(name = symbol$name, PACKAGE = pkgDLL, withRegistrationInfo = TRUE)
      symbol$address <- newsymbol$address
      return(symbol)
    } else if (rVersion >= "2.14") {
      return(getNativeSymbolInfo(symbol$name))
    }
  } else {
    return(symbol)
  }
}
