#' Combine pages into a single data frame
#'
#' The `rbind_pages` function is used to combine a list of data frames into a single
#' data frame. This is often needed when working with a JSON API that limits the amount
#' of data per request. If we need more data than what fits in a single request, we need to
#' perform multiple requests that each retrieve a fragment of data, not unlike pages in a
#' book. In practice this is often implemented using a `page` parameter in the API. The
#' `rbind_pages` function can be used to combine these pages back into a single dataset.
#'
#' The `rbind_pages` function uses [vctrs::vec_rbind()]
#' to bind the pages together. This generalizes [`base::rbind()`][base::cbind] in two
#' ways:
#'
#' - Not each column has to be present in each of the individual data frames; missing
#'   columns will be filled up in `NA` values.
#' - Data frames can be nested (can contain other data frames).
#'
#' @export
#' @param pages a list of data frames, each representing a *page* of data
#' @examples # Basic example
#' x <- data.frame(foo = rnorm(3), bar = c(TRUE, FALSE, TRUE))
#' y <- data.frame(foo = rnorm(2), col = c("blue", "red"))
#' rbind_pages(list(x, y))
#'
#' \donttest{
#' baseurl <- "https://projects.propublica.org/nonprofits/api/v2/search.json"
#' pages <- list()
#' for(i in 0:20){
#'   mydata <- fromJSON(paste0(baseurl, "?order=revenue&sort_order=desc&page=", i))
#'   message("Retrieving page ", i)
#'   pages[[i+1]] <- mydata$organizations
#' }
#' organizations <- rbind_pages(pages)
#' nrow(organizations)
#' colnames(organizations)
#' }
rbind_pages <- function(pages) {
  loadpkg("vctrs")

  #validate input
  stopifnot(is.list(pages))

  # All elements must be data frames or NULL.
  pages <- Filter(
    function(x) {
      !is.null(x)
    },
    pages
  )
  stopifnot(all(vapply(pages, is.data.frame, logical(1))))

  do.call(vctrs::vec_rbind, pages)
}
