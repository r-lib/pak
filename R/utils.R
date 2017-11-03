is_uri <- function(x) {
  # TODO
  FALSE
}

# Adapted from withr:::merge_new
merge_new <- function(old, new, action = match.arg(action, c("replace", "prepend", "append"))) {
  action <- match.arg(action, c("replace", "prepend", "append"))

  switch(action,
    prepend = c(new, old),
    append = c(old, new),
    replace = new
  )
}
