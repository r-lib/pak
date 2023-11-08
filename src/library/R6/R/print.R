#' @export
format.R6 <- function(x, ...) {
  if (is.function(.subset2(x, "format"))) {
    .subset2(x, "format")(...)
  } else {
    ret <- paste0("<", class(x)[1], ">")

    # If there's another class besides first class and R6
    classes <- setdiff(class(x), "R6")
    if (length(classes) >= 2) {
      ret <- c(ret, paste0("  Inherits from: <", classes[2], ">"))
    }

    ret <- c(ret,
      "  Public:",
      indent(object_summaries(x, exclude = c(".__active__", ".__enclos_env__")), 4)
    )

    private <- .subset2(.subset2(x, ".__enclos_env__"), "private")
    if (!is.null(private)) {
      ret <- c(ret,
        "  Private:",
        indent(object_summaries(private), 4)
      )
    }
    paste(ret, collapse = "\n")
  }
}

#' @export
print.R6 <- function(x, ...) {
  if (is.function(.subset2(x, "print"))) {
    .subset2(x, "print")(...)
  } else {
    cat(format(x, ...), sep = "\n")
  }

  invisible(x)
}

#' @export
format.R6ClassGenerator <- function(x, ...) {
  classname <- x$classname
  if (is.null(classname)) classname <- "unnamed"
  ret <- paste0("<", classname, "> object generator")

  if (!is.null(x$inherit)) {
    ret <- c(ret, paste0("  Inherits from: <", deparse(x$inherit), ">"))
  }

  ret <- c(ret,
    "  Public:",
    indent(object_summaries(x$public_fields), 4),
    indent(object_summaries(x$public_methods), 4)
  )

  if (!is.null(x$active)) {
    ret <- c(ret,
      "  Active bindings:",
      indent(object_summaries(x$active), 4)
    )
  }

  if (!(is.null(x$private_fields) && is.null(x$private_methods))) {
    ret <- c(ret,
      "  Private:",
      indent(object_summaries(x$private_fields), 4),
      indent(object_summaries(x$private_methods), 4)
    )
  }
  ret <- c(ret, paste("  Parent env:", format(x$parent_env)))
  # R6 generators created by versions <2.1 could be used with this version of
  # print. They had x$lock instead of x$lock_objects, and they didn't have
  # x$lock_class at all. Make sure we don't error in that case. Eventually we'll
  # be able to remove this check.
  if (!is.null(x$lock) && is.logical(x$lock))
    ret <- c(ret, paste("  Locked objects:", x$lock))
  if (!is.null(x$lock_objects))
    ret <- c(ret, paste("  Locked objects:", x$lock_objects))
  if (!is.null(x$lock_class))
    ret <- c(ret, paste("  Locked class:", x$lock_class))
  ret <- c(ret, paste("  Portable:", x$portable))

  paste(ret, collapse = "\n")
}

#' @export
print.R6ClassGenerator <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

# Return a summary string of the items of a list or environment
# x must be a list or environment
object_summaries <- function(x, exclude = NULL) {
  if (length(x) == 0)
    return(NULL)

  if (is.list(x))
    obj_names <- names(x)
  else if (is.environment(x))
    obj_names <- ls(x, all.names = TRUE)

  obj_names <- setdiff(obj_names, exclude)

  values <- vapply(obj_names, function(name) {
    if (is.environment(x) && bindingIsActive(name, x)) {
      "active binding"
    } else {
      obj <- .subset2(x, name)
      if (is.function(obj)) deparse(args(obj))[[1L]]
      # Plain environments (not envs with classes, like R6 or RefClass objects)
      else if (is.environment(obj) && identical(class(obj), "environment")) "environment"
      else if (is.null(obj)) "NULL"
      else if (is.atomic(obj)) {
        # If obj has many elements, paste() can be very slow, so we'll just
        # use just a subset of it. https://github.com/r-lib/R6/issues/159
        txt <- as.character(utils::head(obj, 60))
        txt <- paste(txt, collapse = " ")
        trim(txt)
      }
      else paste(class(obj), collapse = ", ")
    }
  }, FUN.VALUE = character(1))

  paste0(obj_names, ": ", values, sep = "")
}

# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(^|\\n)(?!$)",
    paste0("\\1", paste(rep(" ", indent), collapse = "")),
    str,
    perl = TRUE
  )
}

# Trim a string to n characters; if it's longer than n, add " ..." to the end
trim <- function(str, n = 60) {
  if (nchar(str) > n) paste(substr(str, 1, n-4), "...")
  else str
}


#' @export
plot.R6 <- function(x, ...) {
  if (is.function(x$plot)) {
    x$plot(...)
  } else {
    stop(paste0("No plot method defined for R6 class ", class(x)[1]))
  }
}
