
#' @importFrom glue backtick

collapse <- function(...) {
  glue_ver <- package_version(getNamespaceVersion(asNamespace("glue")))
  if (glue_ver <= package_version("1.2.0")) {
    getNamespace("glue")$collapse(...)
  } else  {
    getNamespace("glue")$glue_collapse(...)
  }
}

format_items <- function(x) {
  paste0(
    collapse(backtick(x), sep = ", ", last = " and ")
  )
}
