`yaml.load` <-
function(string, as.named.list = TRUE, handlers = NULL, error.label = NULL,
         eval.expr = getOption("yaml.eval.expr", FALSE),
         merge.precedence = c("order", "override"), merge.warning = FALSE) {

  string <- enc2utf8(paste(string, collapse = "\n"))
  eval.warning <- missing(eval.expr) && is.null(getOption("yaml.eval.expr"))
  merge.precedence <- match.arg(merge.precedence)

  .Call(C_unserialize_from_yaml, string, as.named.list, handlers, error.label,
        eval.expr, eval.warning, merge.precedence, merge.warning,
        PACKAGE="yaml")
}
