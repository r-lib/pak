
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_border_style <- function(x) {
  is_string(x) && x %in% rownames(box_styles())
}

is_padding_or_margin <- function(x) {
  is.numeric(x) && length(x) %in% c(1, 4) && !anyNA(x) &&
    all(as.integer(x) == x)
}

is_col <- function(x) {
  is.null(x) || is_string(x) || is.function(x)
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x &&
    x >= 0
}

is_tree_style <- function(x) {
  is.list(x) &&
    length(x) == 4 &&
    !is.null(names(x)) &&
    all(sort(names(x)) == sort(c("h", "v", "l", "j"))) &&
    all(sapply(x, is_string))
}

is_named <- function(x) {
  !is.null(names(x))
}
