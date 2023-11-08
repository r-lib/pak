
parse_urls <- function(urls) {
  out <- str_trim(strsplit(str_trim(urls), "[,\\s]+", perl = TRUE)[[1]])
  grep("^http", out, value = TRUE)
}

deparse_urls <- function(urls) {
  paste(urls, collapse = ",\n    ")
}

idesc_get_urls <- function(self, private) {
  urls <- self$get("URL")
  if (is.na(urls)) {
    character()
  } else {
    parse_urls(urls)
  }
}

idesc_set_urls <- function(self, private, urls) {
  stopifnot(is.character(urls))
  self$set(URL = deparse_urls(urls))
  invisible(self)
}

idesc_add_urls <- function(self, private, urls) {
  stopifnot(is.character(urls))
  urls <- unique(c(self$get_urls(), urls))
  self$set(URL = deparse_urls(urls))
  invisible(self)
}

idesc_del_urls <- function(self, private, pattern) {
  stopifnot(is_string(pattern))
  urls <- self$get_urls()
  filt <- grep(pattern, urls, invert = TRUE, value = TRUE, perl = TRUE)
  if (length(filt) > 0) {
    self$set(URL = deparse_urls(filt))
  } else {
    self$del("URL")
  }
  invisible(self)
}

idesc_clear_urls <- function(self, private) {
  self$del("URL")
  invisible(self)
}
