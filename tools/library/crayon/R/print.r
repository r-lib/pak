
#' @export
#' @method print crayon

print.crayon <- function(x, ...) {
  st <- paste(names(attr(x, "_styles")), collapse = ", ")
  cat(sep = "",
    "Crayon style function, ",
    st,
    ": ",
    x("example output."),
    "\n"
  )
}
