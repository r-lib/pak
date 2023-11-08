#' Interpolate strings with SQL escaping
#'
#' SQL databases often have custom quotation syntax for identifiers and strings
#' which make writing SQL queries error prone and cumbersome to do. `glue_sql()` and
#' `glue_data_sql()` are analogs to [glue()] and [glue_data()] which handle the
#' SQL quoting. `glue_sql_collapse()` can be used to collapse [DBI::SQL()] objects.
#'
#' They automatically quote character results, quote identifiers if the glue
#' expression is surrounded by backticks '\verb{`}' and do not quote
#' non-characters such as numbers. If numeric data is stored in a character
#' column (which should be quoted) pass the data to `glue_sql()` as a
#' character.
#'
#' Returning the result with [DBI::SQL()] will suppress quoting if desired for
#' a given value.
#'
#' Note [parameterized queries](https://db.rstudio.com/best-practices/run-queries-safely#parameterized-queries)
#' are generally the safest and most efficient way to pass user defined
#' values in a query, however not every database driver supports them.
#'
#' If you place a `*` at the end of a glue expression the values will be
#' collapsed with commas. This is useful for the [SQL IN Operator](https://www.w3schools.com/sql/sql_in.asp)
#' for instance.
#' @inheritParams glue
#' @seealso [glue_sql_collapse()] to collapse [DBI::SQL()] objects.
#' @param .con \[`DBIConnection`]: A DBI connection object obtained from
#'   [DBI::dbConnect()].
#' @return A [DBI::SQL()] object with the given query.
#' @examplesIf requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' iris2 <- iris
#' colnames(iris2) <- gsub("[.]", "_", tolower(colnames(iris)))
#' DBI::dbWriteTable(con, "iris", iris2)
#' var <- "sepal_width"
#' tbl <- "iris"
#' num <- 2
#' val <- "setosa"
#' glue_sql("
#'   SELECT {`var`}
#'   FROM {`tbl`}
#'   WHERE {`tbl`}.sepal_length > {num}
#'     AND {`tbl`}.species = {val}
#'   ", .con = con)
#'
#' # If sepal_length is store on the database as a character explicitly convert
#' # the data to character to quote appropriately.
#' glue_sql("
#'   SELECT {`var`}
#'   FROM {`tbl`}
#'   WHERE {`tbl`}.sepal_length > {as.character(num)}
#'     AND {`tbl`}.species = {val}
#'   ", .con = con)
#'
#'
#' # `glue_sql()` can be used in conjuction with parameterized queries using
#' # `DBI::dbBind()` to provide protection for SQL Injection attacks
#'  sql <- glue_sql("
#'     SELECT {`var`}
#'     FROM {`tbl`}
#'     WHERE {`tbl`}.sepal_length > ?
#'   ", .con = con)
#' query <- DBI::dbSendQuery(con, sql)
#' DBI::dbBind(query, list(num))
#' DBI::dbFetch(query, n = 4)
#' DBI::dbClearResult(query)
#'
#' # `glue_sql()` can be used to build up more complex queries with
#' # interchangeable sub queries. It returns `DBI::SQL()` objects which are
#' # properly protected from quoting.
#' sub_query <- glue_sql("
#'   SELECT *
#'   FROM {`tbl`}
#'   ", .con = con)
#'
#' glue_sql("
#'   SELECT s.{`var`}
#'   FROM ({sub_query}) AS s
#'   ", .con = con)
#'
#' # If you want to input multiple values for use in SQL IN statements put `*`
#' # at the end of the value and the values will be collapsed and quoted appropriately.
#' glue_sql("SELECT * FROM {`tbl`} WHERE sepal_length IN ({vals*})",
#'   vals = 1, .con = con)
#'
#' glue_sql("SELECT * FROM {`tbl`} WHERE sepal_length IN ({vals*})",
#'   vals = 1:5, .con = con)
#'
#' glue_sql("SELECT * FROM {`tbl`} WHERE species IN ({vals*})",
#'   vals = "setosa", .con = con)
#'
#' glue_sql("SELECT * FROM {`tbl`} WHERE species IN ({vals*})",
#'   vals = c("setosa", "versicolor"), .con = con)
#'
#' # If you need to reference variables from multiple tables use `DBI::Id()`.
#' # Here we create a new table of nicknames, join the two tables together and
#' # select columns from both tables. Using `DBI::Id()` and the special
#' # `glue_sql()` syntax ensures all the table and column identifiers are quoted
#' # appropriately.
#'
#' iris_db <- "iris"
#' nicknames_db <- "nicknames"
#'
#' nicknames <- data.frame(
#'   species = c("setosa", "versicolor", "virginica"),
#'   nickname = c("Beachhead Iris", "Harlequin Blueflag", "Virginia Iris"),
#'   stringsAsFactors = FALSE
#' )
#'
#' DBI::dbWriteTable(con, nicknames_db, nicknames)
#'
#' cols <- list(
#'   DBI::Id(table = iris_db, column = "sepal_length"),
#'   DBI::Id(table = iris_db, column = "sepal_width"),
#'   DBI::Id(table = nicknames_db, column = "nickname")
#' )
#'
#' iris_species <- DBI::Id(table = iris_db, column = "species")
#' nicknames_species <- DBI::Id(table = nicknames_db, column = "species")
#'
#' query <- glue_sql("
#'   SELECT {`cols`*}
#'   FROM {`iris_db`}
#'   JOIN {`nicknames_db`}
#'   ON {`iris_species`}={`nicknames_species`}",
#'   .con = con
#' )
#' query
#'
#' DBI::dbGetQuery(con, query, n = 5)
#'
#' DBI::dbDisconnect(con)
#' @export
glue_sql <- function(..., .con, .envir = parent.frame(), .na = DBI::SQL("NULL")) {
  DBI::SQL(glue(..., .envir = .envir, .na = .na, .transformer = sql_quote_transformer(.con, .na)))
}

#' @rdname glue_sql
#' @export
glue_data_sql <- function(.x, ..., .con, .envir = parent.frame(), .na = DBI::SQL("NULL")) {
  DBI::SQL(glue_data(.x, ..., .envir = .envir, .na = .na, .transformer = sql_quote_transformer(.con, .na)))
}

#' @rdname glue_collapse
#' @export
glue_sql_collapse <- function(x, sep = "", width = Inf, last = "") {
  DBI::SQL(glue_collapse(x, sep = sep, width = width, last = last))
}

sql_quote_transformer <- function(connection, .na) {
  if (is.null(.na)) {
    .na <- DBI::SQL(NA)
  }

  function(text, envir) {
    should_collapse <- grepl("[*][[:space:]]*$", text)
    if (should_collapse) {
      text <- sub("[*][[:space:]]*$", "", text)
    }
    m <- gregexpr("^`|`$", text)
    is_quoted <- any(m[[1]] != -1)
    if (is_quoted) {
      regmatches(text, m) <- ""
      res <- eval(parse(text = text, keep.source = FALSE), envir)

      if (length(res) == 1) {
        res <- DBI::dbQuoteIdentifier(conn = connection, res)
      } else {

        # Support lists as well
        res[] <- lapply(res, DBI::dbQuoteIdentifier, conn = connection)
      }
    } else {
      res <- eval(parse(text = text, keep.source = FALSE), envir)
      if (inherits(res, "SQL")) {
        if (should_collapse) {
          res <- glue_collapse(res, ", ")
        }
        if (length(res) == 0L) {
          res <- DBI::SQL("NULL")
        }
        return(res)
      }

      # convert objects to characters
      is_object <- is.object(res)
      if (is_object) {
        res <- as.character(res)
      }

      is_na <- is.na(res)
      if (any(is_na)) {
        res[is_na] <- rep(list(.na), sum(is_na))
      }

      is_char <- vapply(res, function(x) !is.na(x) && is.character(x), logical(1))
      res[is_char] <- lapply(res[is_char], function(x) DBI::dbQuoteLiteral(conn = connection, x))
      res[!is_char] <- lapply(res[!is_char], function(x) DBI::SQL(conn = connection, x))
    }
    if (should_collapse) {
      res <- glue_collapse(res, ", ")
    }
    if (length(res) == 0L) {
      res <- DBI::SQL("NULL")
    }
    res
  }
}
