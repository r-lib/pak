
as_string <- function(x) {
  x <- as.character(x)
  if (length(x) != 1) stop("Value must be a scalar")
  x
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && ! is.na(x)
}

# Comment can be either a single string
# or a named vector of strings
is_character <- function(x) {
  is.character(x) && all(! is.na(x))
}

is_character_or_null <- function(x){
  is.null(x) || is_character(x)
}

is_named_character_or_null <- function(x){
  is.null(x) ||
    is.character(x) && length(x) == 1 && !is.na(x) ||
    is.character(x) && length(names(x)) == length(x) && all(!is.na(x))

}

is_constructor_cmd <- function(x) {
  is_string(x) && substring(x, 1, 1) == "!"
}

is_path <- function(x) {
  is_string(x)
}

is_existing_file <- function(x) {
  is_path(x) && file.exists(x)
}

has_no_na <- function(x) {
  !any(is.na(x))
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_authors <- function(x) {
  inherits(x, "person")
}

is_string_or_null <- function(x) {
  is_string(x) || is.null(x)
}


is_collate_field <- function(x) {
  is_string(x) && x %in% names(collate_fields)
}


is_collate_field_or_all <- function(x) {
  is_string(x) && (x %in% names(collate_fields) || x == "all")
}

is_collate_field_or_all_or_default <- function(x) {
  is_string(x) && (x %in% names(collate_fields) || x == "all" || x == "default")
}

is_deps_df <- function(x) {
  is.data.frame(x) &&
    identical(sort(names(x)), sort(c("type", "package", "version"))) &&
    all(sapply(x, is.character) | sapply(x, is.factor))
}

is_package_version <- function(x) {
  tryCatch(
    {
      length(package_version(x)) == 1
    },
    error = function(e) FALSE
  )
}

is_version_component <- function(x) {
  (is_string(x) && x %in% c("major", "minor", "patch", "dev")) ||
    (is_count(x) && x <= 5)
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x
}
