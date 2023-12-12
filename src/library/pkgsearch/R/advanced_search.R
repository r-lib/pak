
#' Advanced CRAN package search
#'
#' See the Elastic documentation for the syntax and features:
#' https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html
#'
#' @param ... Search terms. For named terms, the name specifies the field
#'   to search for. For unnamed ones, the term is taken as is. The
#'   individual terms are combined with the `AND` operator.
#' @param json A character string that contains the query to
#'   send to Elastic. If this is not `NULL`, then you cannot specify
#'   any search terms in `...`.
#' @inheritParams pkg_search
#'
#' @return Search hits.
#' @export
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # All orphaned packages
#' advanced_search(Maintainer = "ORPHANED")
#'
#' # Packages with both Hester and Wickham as authors
#' advanced_search(Author = "Hester", Author = "Wickham")
#' advanced_search("Author: Hester AND Author: Wickham")
#'
#' # Packages with Hester but not Wickham as author
#' advanced_search(Author = "Hester AND NOT Wickham")
#'
#' # Packages with Hester as an Author, and Wickham in any field
#' advanced_search(Author = "Hester", "Wickham")
#'
#' # Packages with Hester as an Author and Wickham nowhere in the metadata
#' advanced_search(Author = "Hester", "NOT Wickham")
#'
#' # Packages for permutation tests and permissive licenses
#' advanced_search("permutation test AND NOT License: GPL OR GNU")
#'
#' # Packages that have a certain field
#' advanced_search("_exists_" = "URL")
#'
#' # Packages that do not have a certain field:
#' advanced_search("NOT _exists_: URL")
#'
#' # The same but as JSON query
#' query <- '{
#' "query": {
#'   "bool": {
#'     "must_not": {
#'       "exists": {
#'         "field": "URL"
#'       }
#'     }
#'   }
#' }
#' }'
#' advanced_search(json = query)
#'
#' # Regular expressions
#' advanced_search(Author = "/Joh?nathan/")
#'
#' # Fuzzy search
#' advanced_search(Author = "Johnathan~1")

advanced_search <- function(..., json = NULL, format = c("short", "long"),
                            from = 1, size = 10) {

  terms <- unlist(list(...))
  format <- match.arg(format)

  if (!is.null(json) && length(terms) > 0) {
    throw(new_error("You cannot specify `json` together with search terms."))
  }

  if (is.null(json)) {
    if (is.null(names(terms))) names(terms) <- rep("", length(terms))

    q <- ifelse(
      names(terms) == "",
      terms,
      paste0("(", names(terms), ":", terms, ")")
    )

    qstr <- tojson$write_str(list(
      query = list(
        query_string = list(
          query = paste0(q, collapse = " AND "),
          default_field = "*"
        )
      )
    ), opts = list(auto_unbox = TRUE, pretty = TRUE))

  } else {
    qstr <- json
  }

  server <- Sys.getenv("R_PKG_SEARCH_SERVER", "search.r-pkg.org")
  port <- as.integer(Sys.getenv("R_PKG_SEARCH_PORT", "80"))

  resp <- do_query(qstr, server, port, from, size)

  result <- format_result(
    resp,
    "advanced search",
    format = format,
    from = from,
    size = size,
    server = server,
    port = port,
    qstr = qstr
  )

  s_data$prev_q <- list(type = "advanced", result = result)

  result
}
