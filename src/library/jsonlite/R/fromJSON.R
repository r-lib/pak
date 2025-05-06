#' These functions are used to convert between JSON data and \R{} objects. The [toJSON()] and [fromJSON()]
#' functions use a class based mapping, which follows conventions outlined in this paper:  <https://arxiv.org/abs/1403.2805> (also available as vignette).
#'
#' The [toJSON()] and [fromJSON()] functions are drop-in replacements for the identically named functions
#' in packages `rjson` and `RJSONIO`. Our implementation uses an alternative, somewhat more consistent mapping
#' between \R{} objects and JSON strings.
#'
#' The [serializeJSON()] and [unserializeJSON()] functions in this package use an
#' alternative system to convert between \R{} objects and JSON, which supports more classes but is much more verbose.
#'
#' A JSON string is always unicode, using `UTF-8` by default, hence there is usually no need to escape any characters.
#' However, the JSON format does support escaping of unicode characters, which are encoded using a backslash followed by
#' a lower case `"u"` and 4 hex characters, for example: `"Z\u00FCrich"`. The `fromJSON` function
#' will parse such escape sequences but it is usually preferable to encode unicode characters in JSON using native
#' `UTF-8` rather than escape sequences.
#
#' @rdname fromJSON
#' @title Convert \R{} objects to/from JSON
#' @name toJSON, fromJSON
#' @aliases fromJSON toJSON jsonlite
#' @export fromJSON toJSON
#' @param txt a JSON string, URL or file
#' @param simplifyVector coerce JSON arrays containing only primitives into an atomic vector
#' @param simplifyDataFrame coerce JSON arrays containing only records (JSON objects) into a data frame
#' @param simplifyMatrix coerce JSON arrays containing vectors of equal mode and dimension into matrix or array
#' @param flatten automatically [flatten()] nested data frames into a single non-nested data frame
#' @param x the object to be encoded
#' @param dataframe how to encode data.frame objects: must be one of 'rows', 'columns' or 'values'
#' @param matrix how to encode matrices and higher dimensional arrays: must be one of 'rowmajor' or 'columnmajor'.
#' @param Date how to encode Date objects: must be one of 'ISO8601' or 'epoch'
#' @param POSIXt how to encode POSIXt (datetime) objects: must be one of 'string', 'ISO8601', 'epoch' or 'mongo'
#' @param factor how to encode factor objects: must be one of 'string' or 'integer'
#' @param complex how to encode complex numbers: must be one of 'string' or 'list'
#' @param raw how to encode raw objects: must be one of 'base64', 'hex' or 'mongo'
#' @param null how to encode NULL values within a list: must be one of 'null' or 'list'
#' @param na how to print NA values: must be one of 'null' or 'string'. Defaults are class specific
#' @param auto_unbox automatically [unbox()] all atomic vectors of length 1. It is usually safer to avoid this and instead use the [unbox()] function to unbox individual elements.
#'   An exception is that objects of class `AsIs` (i.e. wrapped in [I()]) are not automatically unboxed. This is a way to mark single values as length-1 arrays.
#' @param digits max number of decimal digits to print for numeric values. Use [I()] to specify significant digits. Use `NA` for max precision.
#' @param force unclass/skip objects of classes with no defined JSON mapping
#' @param pretty adds indentation whitespace to JSON output. Can be TRUE/FALSE or a number specifying the number of spaces to indent (default is 2). Use a negative number for tabs instead of spaces.
#' @param ... arguments passed on to class specific `print` methods
#' @references Jeroen Ooms (2014). The `jsonlite` Package: A Practical and Consistent Mapping Between JSON Data and \R{} Objects. *arXiv:1403.2805*. <https://arxiv.org/abs/1403.2805>
#' @seealso [read_json()], [stream_in()]
#' @examples # Stringify some data
#' jsoncars <- toJSON(mtcars, pretty=TRUE)
#' cat(jsoncars)
#'
#' # Parse it back
#' fromJSON(jsoncars)
#'
#' # Parse escaped unicode
#' fromJSON('{"city" : "Z\\u00FCrich"}')
#'
#' # Decimal vs significant digits
#' toJSON(pi, digits=3)
#' toJSON(pi, digits=I(3))
#'
#' \dontrun{
#' #retrieve data frame
#' data1 <- fromJSON("https://api.github.com/users/hadley/orgs")
#' names(data1)
#' data1$login
#'
#' # Nested data frames:
#' data2 <- fromJSON("https://api.github.com/users/hadley/repos")
#' names(data2)
#' names(data2$owner)
#' data2$owner$login
#'
#' # Flatten the data into a regular non-nested dataframe
#' names(flatten(data2))
#'
#' # Flatten directly (more efficient):
#' data3 <- fromJSON("https://api.github.com/users/hadley/repos", flatten = TRUE)
#' identical(data3, flatten(data2))
#' }
fromJSON <- function(txt, simplifyVector = TRUE, simplifyDataFrame = simplifyVector, simplifyMatrix = simplifyVector, flatten = FALSE, ...) {
  # check type
  if (!is.character(txt) && !inherits(txt, "connection")) {
    stop("Argument 'txt' must be a JSON string, URL or file.")
  }

  # overload for URL or path
  if (is.character(txt) && length(txt) == 1 && nchar(txt, type = "bytes") < 2084 && !validate(txt)) {
    if (grepl("^https?://", txt, useBytes = TRUE)) {
      txt <- if (R.version$major < 4) {
        base::url(txt)
      } else {
        base::url(txt, headers = c(Accept = "application/json, text/*, */*"))
      }
    } else if (file.exists(txt)) {
      # With files we can never know for sure the encoding. Lets try UTF8 first.
      # txt <- raw_to_json(readBin(txt, raw(), file.info(txt)$size));
      txt <- file(txt)
    }
  }

  # call the actual function (with deprecated arguments)
  parse_and_simplify(txt = txt, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame, simplifyMatrix = simplifyMatrix, flatten = flatten, ...)
}

parse_and_simplify <- function(txt, simplifyVector = TRUE, simplifyDataFrame = simplifyVector, simplifyMatrix = simplifyVector, flatten = FALSE, unicode = TRUE, validate = TRUE, bigint_as_char = FALSE, ...) {
  if (!missing(unicode)) {
    message("Argument unicode has been deprecated. YAJL always parses unicode.")
  }

  if (!missing(validate)) {
    message("Argument validate has been deprecated. YAJL automatically validates json while parsing.")
  }

  # parse
  obj <- parseJSON(txt, bigint_as_char)

  # post processing
  if (any(isTRUE(simplifyVector), isTRUE(simplifyDataFrame), isTRUE(simplifyMatrix))) {
    return(simplify(obj, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame, simplifyMatrix = simplifyMatrix, flatten = flatten, ...))
  } else {
    return(obj)
  }
}


# Backward compatiblity
fromJSON_string <- parse_and_simplify
