
ps_env <- new.env(parent = emptyenv())

Internal <- NULL

## nocov start
.onLoad <- function(libname, pkgname) {
  ps_env$constants <- new.env(parent  = emptyenv())
  .Call(ps__init, asNamespace("ps"), ps_env$constants)
  if (!is.null(ps_env$constants$signals)) {
    ps_env$constants$signals <- as.list(ps_env$constants$signals)
  }
  if (!is.null(ps_env$constants$errno)) {
    ps_env$constants$errno <- as.list(ps_env$constants$errno)
  }
  if (!is.null(ps_env$constants$address_families)) {
    ps_env$constants$address_families <-
      as.list(ps_env$constants$address_families)
  }
  if (!is.null(ps_env$constants$socket_types)) {
    ps_env$constants$socket_types <-
      as.list(ps_env$constants$socket_types)
  }

  Internal <<- get(".Internal", asNamespace("base"))

  ps_boot_time <<- memoize(ps_boot_time)
  ps_cpu_count_logical <<- memoize(ps_cpu_count_logical)  
  ps_cpu_count_physical <<- memoize(ps_cpu_count_physical)  
  get_terminal_map <<- memoize(get_terminal_map)
  NA_time <<- memoize(NA_time)
}
## nocov end

utils::globalVariables(c("self", "super"))
