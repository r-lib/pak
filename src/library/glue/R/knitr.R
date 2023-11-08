# nocov start
eng_glue <- function(options) {
  glue_options <- options[names(options) %in% names(formals(glue))]
  glue_options$.envir <- glue_options$.envir %||% knitr::knit_global()

  out <- do.call(glue, c(list(options$code), glue_options))
  knitr::engine_output(options, options$code, out)
}

eng_glue_sql <- function(options) {
  glue_sql_options <- options[names(options) %in% names(formals(glue_sql))]
  glue_sql_options$.con <- glue_sql_options$.con %||% options$connection
  glue_sql_options$.envir <- glue_sql_options$.envir %||% knitr::knit_global()
  con <- glue_sql_options$.con
  if (is.character(con)) {
    con <- get(con, envir = knitr::knit_global())
  }

  if (is.null(con)) {
    stop(.call = FALSE,
    "The 'connection' option (DBI connection) is required for glue_sql chunks.")
  }
  glue_sql_options$.con <- con
  options$code <- do.call(glue_sql, c(list(paste0(options$code, collapse = "\n")), glue_sql_options))
  options$engine <- "sql"
  knitr::knit_engines$get("sql")(options)
}

# nocov end
