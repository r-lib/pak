# This function returns the superclass object
generator_funs$get_inherit <- function() {
  # The NULL arg speeds up eval a tiny bit
  eval(inherit, parent_env, NULL)
}

# This is the $has_private function for a R6ClassGenerator. This copy of it
# won't run properly; it needs to be copied, and its parent environment set to
# the generator object environment.
# Returns TRUE if this class or one of its ancestor superclasses has private
# members; FALSE otherwise.
generator_funs$has_private <- function() {
  inherit <- get_inherit()
  if (!is.null(private_fields) || !is.null(private_methods))
    TRUE
  else if (is.null(inherit))
    FALSE
  else
    inherit$has_private()
}

# This is the $set function for a R6ClassGenerator. This copy of it won't run
# properly; it needs to be copied, and its parent environment set to the
# generator object environment.
generator_funs$set <- function(which = NULL, name = NULL, value, overwrite = FALSE) {
  if (lock_class)
    stop("Can't modify a locked R6 class.")

  if (is.null(which) || !(which %in% c("public", "private", "active")))
    stop("`which` must be 'public', 'private', or 'active'.")

  if (is.null(name) || !is.character(name))
    stop("`name` must be a string.")

  if (missing(value))
    stop("`value` must be provided.")

  # Find which group this object should go in.
  if (which == "public") {
    group <- if (is.function(value)) "public_methods" else "public_fields"
  } else if (which == "private") {
    group <- if (is.function(value)) "private_methods" else "private_fields"
  } else if (which == "active") {
    if (is.function(value))
      group <- "active"
    else
      stop("Can't add non-function to active")
  }

  # Check that it's not already present
  all_groups <- c("public_methods", "public_fields", "private_methods",
                  "private_fields", "active")

  # If we're allowed to overwrite, don't check the group that this object
  # would go in.
  if (overwrite)
    all_groups <- setdiff(all_groups, group)

  all_names <- unlist(lapply(all_groups, function(g) names(get(g))))

  if (name %in% all_names) {
    stop("Can't add ", name, " because it already present in ", classname,
         " generator.")
  }

  # Assign in correct group. Create group if it doesn't exist.
  if (is.null(self[[group]]))
    self[[group]] <- list()

  if (is.null(value)) {
    # If it's NULL, the item should get a NULL value. The `[[<-` assignment
    # would instead delete the item; this method gives it a NULL value.
    self[[group]][name] <- list(NULL)
  } else {
    self[[group]][[name]] <- value
  }

  invisible()
}


# Enable debugging for one or more methods. This will apply to all objects
# instantiated after this is called.
generator_funs$debug <- function(name) {
  debug_names <<- union(debug_names, name)
}

# Disable debugging for one or more methods.
generator_funs$undebug <- function(name) {
  debug_names <<- setdiff(debug_names, name)
}

generator_funs$lock <- function() {
  lock_class <<- TRUE
}

generator_funs$unlock <- function() {
  lock_class <<- FALSE
}

generator_funs$is_locked <- function() {
  lock_class
}
