
`%||%` <- function(l, r) if (is.null(l)) r else l

`%|NA|%` <- function(l, r) ifelse(is.na(l), r, l)

check_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || as.integer(x) != x ||
      is.na(x) || x < 0) {
    throw(new_error(x, " is not a count", call. = FALSE))
  }
}

check_string <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    throw(new_error(x, " is not a string", call. = FALSE))
  }
}

`%+%` <- function(lhs, rhs) {
  check_string(lhs)
  check_string(rhs)
  paste0(lhs, rhs)
}

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map_mold <- function(.x, .f, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

map_int <- function(.x, .f, ...) {
  map_mold(.x, .f, integer(1), ...)
}

map_dbl <- function(.x, .f, ...) {
  map_mold(.x, .f, double(1), ...)
}

map_chr <- function(.x, .f, ...) {
  map_mold(.x, .f, character(1), ...)
}

map_lgl <- function(.x, .f, ...) {
  map_mold(.x, .f, logical(1), ...)
}

meta <- function(x) {
  attr(x, "metadata")
}

`meta<-` <- function(x, value) {
  attr(x, "metadata") <- value
  x
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

couchdb_uri <- function() {
  "https://crandb.r-pkg.org/"
}

add_class <- function(x, class_name) {
  if (! inherits(x, class_name)) {
    class(x) <- c(class_name, attr(x, "class"))
  }
  x
}

add_attr <- function(object, key, value) {
  attr(object, key) <- value
  object
}

contains <- function(x, y) y %in% x

isin <- function(x, y) x %in% y

remove_special <- function(list, level = 1) {
  
  assert_that(is_positive_count(level))
  
  if (level == 1) {
    replace(
      grepl(pattern = "^_", names(list)),
      x = list,
      values = NULL
    )
  } else {
    lapply(list, remove_special, level = level - 1)
  }
  
}

pluck <- function(list, idx) list[[idx]]

needs_packages <- function(pkgs) {
  has <- map_lgl(pkgs, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })

  if (!all(has)) {
    not_installed_pkgs <- pkgs[!has]
    
    if (length(not_installed_pkgs) == 1) {
      
      throw(new_error(
        "The ",
        sQuote(not_installed_pkgs),
        " package is needed for this addin.",
        call. = FALSE
      ))
    } else {
      
      throw(new_error(
        "The ",
        paste(sQuote(not_installed_pkgs), collapse = ", "),
        " packages are needed for this addin.",
        call. = FALSE
      ))
    }
    
  }
}

clean_description <- function(txt) {
  gsub("<U+000a>", " ", txt, fixed = TRUE)
}

zap_null <- function(x) {
  x[! map_lgl(x, is.null)]
}
