# Modeled after fs::path()
path <- function(...) {
  dots <- list(...)

  if (!is.null(names(dots)) && any(names(dots) != "")) {
    warning("Arguments must be unnamed", call. = FALSE)
  }

  # Different recycling rules for zero-length vectors
  lengths <- lengths(dots)
  if (any(lengths == 0) && all(lengths %in% 0:1)) {
    return(character())
  }

  # Side effect: check recycling rules
  component_df <- as.data.frame(dots, stringsAsFactors = FALSE)

  missing <- apply(is.na(component_df), 1, any)

  components <- lapply(component_df, function(x) enc2utf8(as.character(x)))

  out <- do.call(file.path, components)
  out[missing] <- NA_character_
  Encoding(out) <- "UTF-8"
  out
}
