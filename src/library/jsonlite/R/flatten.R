#' Flatten nested data frames
#'
#' In a nested data frame, one or more of the columns consist of another data
#' frame. These structures frequently appear when parsing JSON data from the web.
#' We can flatten such data frames into a regular 2 dimensional tabular structure.
#'
#' @export
#' @param x a data frame
#' @param recursive flatten recursively
#' @examples options(stringsAsFactors=FALSE)
#' x <- data.frame(driver = c("Bowser", "Peach"), occupation = c("Koopa", "Princess"))
#' x$vehicle <- data.frame(model = c("Piranha Prowler", "Royal Racer"))
#' x$vehicle$stats <- data.frame(speed = c(55, 34), weight = c(67, 24), drift = c(35, 32))
#' str(x)
#' str(flatten(x))
#' str(flatten(x, recursive = FALSE))
#'
#' \dontrun{
#' data1 <- fromJSON("https://api.github.com/users/hadley/repos")
#' colnames(data1)
#' colnames(data1$owner)
#' colnames(flatten(data1))
#'
#' # or for short:
#' data2 <- fromJSON("https://api.github.com/users/hadley/repos", flatten = TRUE)
#' colnames(data2)
#' }
#'
flatten <- function(x, recursive = TRUE) {
  stopifnot(is.data.frame(x))
  nr <- nrow(x)
  dfcolumns <- vapply(x, is.data.frame, logical(1))
  if (!any(dfcolumns)) {
    return(x)
  }
  x <- if (recursive) {
    c(x[!dfcolumns], do.call(c, lapply(x[dfcolumns], flatten)))
  } else {
    c(x[!dfcolumns], do.call(c, x[dfcolumns]))
  }
  class(x) <- "data.frame"
  row.names(x) <- if (!nr) character(0) else 1:nr
  x
}

#1,2,3,df1,5,6,7,df2,9
