#' The [serializeJSON()] and [unserializeJSON()] functions convert between
#' \R{} objects to JSON data. Instead of using a class based mapping like
#' [toJSON()] and [fromJSON()], the serialize functions base the encoding
#' schema on the storage type, and capture all data and attributes from any object.
#' Thereby the object can be restored almost perfectly from its JSON representation, but
#' the resulting JSON output is very verbose. Apart from environments, all standard storage
#' types are supported.
#'
#' @rdname serializeJSON
#' @title serialize R objects to JSON
#' @name serializeJSON
#' @export serializeJSON unserializeJSON
#' @param x an \R{} object to be serialized
#' @param digits max number of digits (after the dot) to print for numeric values
#' @param pretty add indentation/whitespace to JSON output. See [prettify()]
#' @note JSON is a text based format which leads to loss of precision when printing numbers.
#' @examples jsoncars <- serializeJSON(mtcars)
#' mtcars2 <- unserializeJSON(jsoncars)
#' identical(mtcars, mtcars2)
#'
#' set.seed('123')
#' myobject <- list(
#'   mynull = NULL,
#'   mycomplex = lapply(eigen(matrix(-rnorm(9),3)), round, 3),
#'   mymatrix = round(matrix(rnorm(9), 3),3),
#'   myint = as.integer(c(1,2,3)),
#'   mydf = cars,
#'   mylist = list(foo='bar', 123, NA, NULL, list('test')),
#'   mylogical = c(TRUE,FALSE,NA),
#'   mychar = c('foo', NA, 'bar'),
#'   somemissings = c(1,2,NA,NaN,5, Inf, 7 -Inf, 9, NA),
#'   myrawvec = charToRaw('This is a test')
#' );
#' identical(unserializeJSON(serializeJSON(myobject)), myobject);
serializeJSON <- function(x, digits = 8, pretty = FALSE) {
  # just to verify that obj exists
  is(x)
  # we pass arguments both to asJSON as well as packaging object.
  ans <- asJSON(pack(x), digits = digits, indent = if (isTRUE(pretty)) 0L else NA_integer_)
  class(ans) <- "json"
  return(ans)
}

#' @param txt a JSON string which was created using `serializeJSON`
#' @rdname serializeJSON
unserializeJSON <- function(txt) {
  unpack(parseJSON(txt))
}
