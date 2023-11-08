
which_collate <- function(x) {
  collate_fields[x]
}


idesc_set_collate <- function(self, private, files, which) {
  stopifnot(is.character(files), is_collate_field(which))
  if (length(files) == 0) warning("No files in 'Collate' field")

  idesc_really_set_collate(self, private, files, which_collate(which))
}


idesc_really_set_collate <- function(self, private, files, field) {
  if (!identical(self$get_collate(), files)) {
    self$set(field, deparse_collate(files))
  }
}


idesc_get_collate <- function(self, private, which) {
  stopifnot(is_collate_field(which))
  coll <- unname(self$get(which_collate(which)))
  if (identical(coll, NA_character_)) character() else parse_collate(coll)
}


idesc_del_collate <- function(self, private, which) {
  stopifnot(is_collate_field_or_all(which))

  if (which == "all") {
    self$del(collate_fields)

  } else {
    self$del(collate_fields[which])
  }

  invisible(self)
}


idesc_add_to_collate <- function(self, private, files, which) {
  stopifnot(is.character(files), is_collate_field_or_all_or_default(which))

  if (which == "default") {
    ex_coll <- intersect(collate_fields, self$fields())
    if (length(ex_coll) == 0) {
      real_add_to_collate(self, private, which_collate("main"), files)
    } else {
      for (ex in ex_coll) real_add_to_collate(self, private, ex, files)
    }

  } else if (which == "all") {
    for (coll in collate_fields) {
      real_add_to_collate(self, private, coll, files)
    }

  } else {
    real_add_to_collate(self, private, which_collate(which), files)
  }
  
}

## TODO: better order, and support dependencies

real_add_to_collate <- function(self, private, field, files) {
  ex <- if (!self$has_fields(field)) {
    character()
  } else {
    parse_collate(self$get(field))
  }

  files <- unique(c(ex, files))
  idesc_really_set_collate(self, private, files, field)
}


idesc_del_from_collate <- function(self, private, files, which) {
  stopifnot(is.character(files), is_collate_field_or_all(which))

  if (which == "all") {
    for (coll in collate_fields) {
      real_del_from_collate(self, private, coll, files)
    }

  } else {
    real_del_from_collate(self, private, which_collate(which), files)
  }
}

real_del_from_collate <- function(self, private, field, files) {
  if (self$has_fields(field)) {
    coll <- setdiff(parse_collate(self$get(field)), files)
    idesc_really_set_collate(self, private, coll, field)
  } else {
    invisible(self)
  }
}


parse_collate <- function(str) {
  scan(
    text = gsub("\n", " ", str),
    what = "",
    strip.white = TRUE,
    quiet = TRUE
  )
}


deparse_collate <- function(list) {
  paste0(
    "\n",
    paste0(
      "    '",
      list,
      "'",
      collapse = "\n"
    )
  )
}
