
parse_remotes <- function(remotes) {
  str_trim(strsplit(remotes, "\\s*,\\s*", perl = TRUE)[[1]])
}

deparse_remotes <- function(remotes) {
  paste0(" \n    ", paste(str_trim(remotes), collapse = ",\n    "))
}

idesc_get_remotes <- function(self, private) {
  remotes <- self$get("Remotes")
  if (is.na(remotes)) {
    character()
  } else {
    parse_remotes(remotes)
  }
}

idesc_set_remotes <- function(self, private, remotes) {
  stopifnot(is.character(remotes))
  self$set(Remotes = deparse_remotes(remotes))
  invisible(self)
}

idesc_add_remotes <- function(self, private, remotes) {
  stopifnot(is.character(remotes))
  remotes <- unique(c(self$get_remotes(), remotes))
  self$set(Remotes = deparse_remotes(remotes))
  invisible(self)
}

idesc_del_remotes <- function(self, private, pattern) {
  stopifnot(is_string(pattern))
  remotes <- self$get_remotes()
  if (length(remotes) == 0) return(invisible(self))

  filt <- grep(pattern, remotes, invert = TRUE, value = TRUE, perl = TRUE)
  if (length(filt) > 0) {
    self$set(Remotes = deparse_remotes(filt))
  } else {
    self$del("Remotes")
  }
  invisible(self)
}

idesc_clear_remotes <- function(self, private) {
  self$del("Remotes")
  invisible(self)
}
