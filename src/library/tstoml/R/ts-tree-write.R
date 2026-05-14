#' @ts ts_tree_write_details_file
#'
#' Format a TOML file:
#'
#' ```{r}
#' #| eval = FALSE
#' tree <- tstoml::ts_parse_toml("config.toml")
#' tree |> ts_tree_format() |> ts_tree_write()
#' ```
#'
#' @export

ts_tree_write.ts_tree_toml <- function(tree, file = NULL) {
  NextMethod()
}
