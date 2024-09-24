
## ----------------------------------------------------------------------

s_data <- new.env(parent = emptyenv())

#' Search CRAN packages
#'
#' @description
#' `pkg_search()` starts a new search query, or shows the details of the
#' previous query, if called without arguments.
#'
#' `ps()` is an alias to `pkg_search()`.
#'
#' `more()` retrieves that next page of results for the previous query.
#'
#' @details
#' Note that the search needs a working Internet connection.
#'
#' @param query Search query string. If this argument is missing or
#'   `NULL`, then the results of the last query are printed, in
#'   _short_ and _long_ formats, in turns for successive
#'   `pkg_search()` calls. If this argument is missing, then all
#'   other arguments are ignored.
#' @param format Default formatting of the results. _short_ only
#'   outputs the name and title of the packages, _long_ also
#'   prints the author, last version, full description and URLs.
#'   Note that this only affects the default printing, and you can
#'   still inspect the full results, even if you specify _short_
#'   here.
#' @param from Where to start listing the results, for pagination.
#' @param size The number of results to list.
#' @return A data frame with columns:
#'   * `score`: Score of the hit. See Section _Scoring_ for some details.
#'   * `package`: Package name.
#'   * `version`: Latest package version.
#'   * `title`: Package title.
#'   * `description`: Short package description.
#'   * `date`: Time stamp of the last release.
#'   * `maintainer_name`: Name of the package maintainer.
#'   * `maintainer_email`: Email address of the package maintainer.
#'   * `revdeps`: Number of (strong and weak) reverse dependencies of the
#'     package.
#'   * `downloads_last_month`: Raw number of package downloads last month,
#'     from the RStudio CRAN mirror.
#'   * `license`: Package license.
#'   * `url`: Package URL(s).
#'   * `bugreports`: URL of issue tracker, or email address for bug reports.
#'
#' @export
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example
#' ps("survival")
#'
#' # Pagination
#' ps("networks")
#' more()
#'
#' # Details
#' ps("visualization")
#' ps()
#'
#' # See the underlying data frame
#' ps("ropensci")
#' ps()[]

pkg_search <- function(query = NULL, format = c("short", "long"),
                       from = 1, size = 10) {

  if (is.null(query)) return(pkg_search_again())
  format <- match.arg(format)
  server <- Sys.getenv("R_PKG_SEARCH_SERVER", "https://search.r-pkg.org")

  make_pkg_search(query, format, from, size, server)
}

#' @rdname pkg_search
#' @export

ps <- pkg_search

make_pkg_search <- function(query, format, from, size, server) {

  qry <- make_query(query = query)
  rsp <- do_query(qry, server = server, from = from, size = size)
  rst <- format_result(rsp, query = query, format = format, from = from,
                       size = size, server = server)

  s_data$prev_q <- list(type = "simple", result = rst)

  rst
}

#' @rdname pkg_search
#' @export

more <- function(format = NULL, size = NULL) {
  if (is.null(s_data$prev_q)) {
    throw(new_error("No query, start with 'pkg_search()'"))
  }

  rst <- s_data$prev_q$result

  if (s_data$prev_q$type == "simple") {
    make_pkg_search(
      query = meta(rst)$query,
      format = format %||% meta(rst)$format,
      from = meta(rst)$from + meta(rst)$size,
      size = size %||% meta(rst)$size,
      server = meta(rst)$server
    )

  } else if (s_data$prev_q$type == "advanced") {
    advanced_search(
      json = meta(rst)$qstr,
      format = format %||% meta(rst)$format,
      from = meta(rst)$from + meta(rst)$size,
      size = size %||% meta(rst)$size
    )

  } else {
    throw(new_error("Unknown search type, internal pkgsearch error :("))
  }
}

make_query <- function(query) {

  check_string(query)

  fields <- c("Package^20", "Title^10", "Description^2",
              "Author^5", "Maintainer^6", "_all")

  query_object <- list(
    query = list(
      function_score = list(
        functions = list(
          list(
            field_value_factor = list(
              field = "revdeps",
              modifier = "sqrt",
              factor = 1)
          )
        ),

        query = list(
          bool = list(
            ## This is simply word by work match, scores add up for fields
            must = list(
              list(multi_match = list(
                     query = query,
                     type = "most_fields"
                   ))
            ),
            should = list(
              ## This is matching the complete phrase, so it takes priority
              list(multi_match = list(
                     query = query,
                     fields = c("Title^10", "Description^2", "_all"),
                     type = "phrase",
                     analyzer = "english_and_synonyms",
                     boost = 10
                   )),
              ## This is if all words match (but not as a phrase)
              list(multi_match = list(
                     query = query,
                     fields = fields,
                     operator = "and",
                     analyzer = "english_and_synonyms",
                     boost = 5
                   ))
            )
          )
        )
      )
    )
  )

  tojson$write_str(
    query_object,
    opts = list(auto_unbox = TRUE, pretty = TRUE)
  )
}

do_query <- function(query, server, from, size) {

  check_count(from)
  check_count(size)

  # timeout for the curl's connect phase (in seconds)
  timeout <- getOption("timeout", 60)

  url <- server %+% "/package/_search?from=" %+%
    as.character(from - 1) %+% "&size=" %+% as.character(size)
  result <- http_post(
    url, body = query,
    headers = c("Content-Type" = "application/json"),
    options = list(timeout = timeout))
  chain_error(
    http_stop_for_status(result),
    new_query_error(result, "search server failure")
  )

  rawToChar(result$content)
}

new_query_error <- function(response, ...) {
  cond <- new_error(...)
  class(cond) <- c("pkgsearch_query_error", class(cond))
  cond$response <- response
  cond
}

#' @export

print.pkgsearch_query_error <- function(x, ...) {
  # The call to the http method is quite tedious and not very useful,
  # so we remove it
  x$parent$call <- NULL

  # default print method for the error itself
  err$print_this(x, ...)

  # error message from Elastic, if any
  tryCatch({
    rsp <- x$response
    cnt <- jsonlite::fromJSON(rawToChar(rsp$content), simplifyVector = FALSE)
    if ("error" %in% names(cnt) &&
        "root_cause" %in% names(cnt$error) &&
        "reason" %in% names(cnt$error$root_cause[[1]])) {
      cat("", cnt$error$root_cause[[1]]$reason, "", sep = "\n")
    }
  }, error = function(x) NULL)

  # parent error(s)
  err$print_parents(x, ...)

  invisible(x)
}

format_result <- function(result, query, format, from, size, server, ...) {
  result <- jsonlite::fromJSON(result, simplifyVector = FALSE)

  meta <- list(
    query = query,
    format = format,
    from = from,
    size = size,
    server = server,
    total = result$hits$total,
    max_score = result$hits$max_score,
    took = result$took,
    timed_out = result$timed_out,
    ...
  )

  sources <- map(result$hits$hits, "[[", "_source")
  maintainer <- map_chr(sources, "[[", "Maintainer")

  df <- data_frame(
    score = map_dbl(result$hits$hits, "[[", "_score"),
    package = map_chr(result$hits$hits, "[[", "_id"),
    version = package_version(map_chr(sources, "[[", "Version")),
    title = map_chr(sources, "[[", "Title"),
    description = map_chr(sources, "[[", "Description"),
    date = parse_iso_8601(map_chr(sources, "[[", "date")),
    maintainer_name = gsub("\\s+<.*$", "", maintainer),
    maintainer_email = gsub("^.*<([^>]+)>.*$", "\\1", maintainer, perl = TRUE),
    revdeps = map_int(sources, "[[", "revdeps"),
    downloads_last_month = map_int(sources, function(x) x$downloads %||% 1L),
    license = map_chr(sources, "[[", "License"),
    url = map_chr(sources, function(x) x$URL %||% NA_character_),
    bugreports = map_chr(sources, function(x) x$BugReports %||% NA_character_),
    package_data = I(sources)
  )

  attr(df, "metadata") <- meta

  class(df) <- unique(c("pkg_search_result", class(df)))

  df
}

#' @export

`[.pkg_search_result` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pkg_search_result")
  NextMethod("[")
}

pkg_search_again <- function() {
  if (is.null(s_data$prev_q)) {
    throw(new_error("No query given, and no previous query"))
  }
  format <- meta(s_data$prev_q$result)$format
  meta(s_data$prev_q$result)$format <- if (format == "short") "long" else "short"
  s_data$prev_q$result
}
