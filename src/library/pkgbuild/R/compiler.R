#' Is a compiler available?
#'
#' @description
#' These functions check if a small C file can be compiled, linked, loaded
#' and executed.
#'
#' `has_compiler()` and `has_devel()` return `TRUE` or `FALSE`.
#' `check_compiler()` and `check_devel()`
#' throw an error if you don't have developer tools installed.
#' If the `"pkgbuild.has_compiler"` option is set to `TRUE` or `FALSE`,
#' no check is carried out, and the value of the option is used.
#'
#' The implementation is  based on a suggestion by Simon Urbanek.
#' End-users (particularly those on Windows) should generally run
#' [check_build_tools()] rather than [check_compiler()].
#'
#'
#' @export
#' @inheritParams has_rtools
#' @seealso [check_build_tools()]
#' @examples
#' has_compiler()
#' check_compiler()
#'
#' with_build_tools(has_compiler())
has_compiler <- function(debug = FALSE) {
  res <- getOption("pkgbuild.has_compiler")
  if (!is.null(res)) {
    if (is_flag(res)) return(res)
    stop(cli::format_error(c(
      "",
      "!" = "Invalid {.code pkgbuild.has_compiler} option.",
      "i" = "It must be {.code TRUE} or {.code FALSE}, not {.type {res}}."
    )))
  }

  if (!debug && cache_exists("has_compiler")) {
    return(cache_get("has_compiler"))
  }

  foo_path <- file.path(tempdir(), "foo.c")
  cat("void foo(int *bar) { *bar=1; }\n", file = foo_path)
  on.exit(unlink(foo_path))

  res <- tryCatch(
    {
      if (debug) {
        message("Trying to compile a simple C file")
      }

      callr::rcmd_safe(
        "SHLIB",
        "foo.c",
        wd = tempdir(),
        show = debug,
        echo = debug,
        fail_on_status = TRUE,
        stderr = "2>&1"
      )

      if (debug) {
        message("")
      }
      dylib <- file.path(tempdir(), paste0("foo", .Platform$dynlib.ext))
      on.exit(unlink(dylib), add = TRUE)

      dll <- dyn.load(dylib)
      on.exit(dyn.unload(dylib), add = TRUE)

      .C(dll$foo, 0L)[[1]] == 1L
    },
    error = function(e) {
      FALSE
    }
  )

  cache_set("has_compiler", res)
  res
}

#' @export
#' @rdname has_compiler
check_compiler <- function(debug = FALSE) {
  if (!has_compiler(debug)) {
    stop("Failed to compile C code", call. = FALSE)
  }

  TRUE
}

#' @export
#' @rdname has_compiler
#' @usage NULL
has_devel <- check_build_tools

# The checking code looks for the objects in the package namespace, so defining
# dll here removes the following NOTE
# Registration problem:
#   Evaluating 'dll$foo' during check gives error
# 'object 'dll' not found':
#    .C(dll$foo, 0L)
# See https://github.com/wch/r-source/blob/d4e8fc9832f35f3c63f2201e7a35fbded5b5e14c/src/library/tools/R/QC.R#L1950-L1980
# Setting the class is needed to avoid a note about returning the wrong class.
# The local object is found first in the actual call, so current behavior is
# unchanged.
dll <- list(foo = structure(list(), class = "NativeSymbolInfo"))
