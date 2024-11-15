# match any library(), require(), base::library(), base::require(), etc. calls
# we use these as fallbacks. If a call is not identified some other way
# we parse it with R and match the call.
q_library_0 <- function() {
  structure(c(
    '((call function: (identifier) @fn-name) @dep-code
      (#any-of? @fn-name
       "library" "require" "loadNamespace" "requireNamespace"
       "pkg_attach" "pkg_attach2"
       "p_load"
       "module"
       "tar_option_set"
       "glue"
       "ggsave"
       "set_engine"
       "R6Class" "test_package" "test_dir" "test_file"))',
    '((call function:
       (namespace_operator
        lhs: (identifier) @ns-name
        rhs: (identifier) @fn-name
       )
      ) @dep-code
      (#any-of? @ns-name
       "base" "xfun" "pacman" "modules" "import" "box" "targets" "glue"
       "ggplot2" "parsnip" "R6" "testthat")
      (#any-of? @fn-name
       "library" "require" "loadNamespace" "requireNamespace"
       "pkg_attach" "pkg_attach2"
       "p_load"
       "module" "import"
       "from" "here" "into"
       "use"
       "tar_option_set"
       "glue"
       "ggsave"
       "set_engine"
       "R6Class" "test_package" "test_dir" "test_file"))'
  ), names = rep("q_library_0", 2))
}

q_module_import <- function() {
  c(
    '((call function: (identifier) @fn-name) @dep-code
      (#any-of? @fn-name "import"))',
    '((call function:
       (namespace_operator
        lhs: (identifier) @ns-name
        rhs: (identifier) @fn-name
       )
      ) @dep-code
      (#any-of? @ns-name "modules")
      (#any-of? @fn-name "import"))'
  )
}

# pkg::fun, pkg:::fun
q_colon <- function() {
  '((namespace_operator lhs: (identifier) @pkg-name) @dep-code
    (#not-eq? @pkg-name "base")
  )'
}

q_methods <- function() {
  structure(
    '((call function: (identifier) @fn-name) @dep-code
      (#any-of? @fn-name "setClass" "setGeneric"))',
    names = "methods"
  )
}

q_junit_reporter <- function() {
  structure(c(
    '((call function:
      (extract_operator
       lhs: (identifier) @class-name
       rhs: (identifier) @method-name
      )
     ) @dep-code
     (#eq? @class-name "JunitReporter")
     (#eq? @method-name "new"))',
    '((call function:
      (extract_operator
       lhs: (namespace_operator
             lhs: (identifier) @pkg-name
             rhs: (identifier) @class-name)
       rhs: (identifier) @method-name
      )
     ) @dep-code
     (#eq? @pkg-name "testthat")
     (#eq? @class-name "JunitReporter")
     (#eq? @method-name "new"))'
  ), names = rep("junit_reporter", 2))
}

q_knitr_dev <- function() {
  structure(c(
    '((call function:
      (extract_operator
       lhs: (identifier) @object-name
       rhs: (identifier) @method-name
      )
     ) @dep-code
     (#eq? @object-name "opts_chunk")
     (#eq? @method-name "set"))',
    '((call function:
      (extract_operator
       lhs: (namespace_operator
             lhs: (identifier) @pkg-name
             rhs: (identifier) @object-name)
       rhs: (identifier) @method-name
      )
     ) @dep-code
     (#eq? @pkg-name "knitr")
     (#eq? @object-name "opts_chunk")
     (#eq? @method-name "set"))'
  ), names = rep("knitr_dev", 2))
}

renv_dependencies_database <- function() {
  db <- getOption("renv.dependencies.database", default = list())
  db$ggplot2$geom_hex <- "hexbin"
  db$testthat$JunitReporter <- "xml2"
  db
}

q_database <- function() {
  db <- renv_dependencies_database()
  fns <- unlist(lapply(db, names))
  if (length(fns) == 0) return(NULL)
  structure(sprintf(
    '((identifier) @id
      (#any-of? @id %s))',
    paste0('"', fns, '"', collapse = " ")
  ), names = "database")
}

q_deps <- function() {
  c(
    q_library_0(),
    q_colon(),
    q_methods(),
    q_junit_reporter(),
    q_knitr_dev(),
    q_database(),
    NULL
  )
}

q_deps_rmd <- function() {
  c(block =
    '(fenced_code_block
      (fenced_code_block_delimiter)
      (info_string (language) @language) @header
      (code_fence_content) @content
      (#any-of? @language "r" "R" "rscript" "Rscript")
      (#match? @header "^[{]")
    )',
    inline =
      '(inline) @inline'
  )
}

q_deps_rmd_inline <- function() {
  '(code_span
    (code_span_delimiter) @csd1
    (code_span_delimiter) @csd2
  ) @code'
}
