encapsulate({
  # Search a list for all function objects, change the environment for those
  # functions to a target environment, and return the modified list.
  assign_func_envs <- function(objs, target_env) {
    if (is.null(target_env)) return(objs)

    lapply(objs, function(x) {
      if (is.function(x)) environment(x) <- target_env
      x
    })
  }

  # Get names of all superclasses
  get_superclassnames <- function(inherit) {
    if (is.null(inherit)) return(NULL)
    c(inherit$classname, get_superclassnames(inherit$get_inherit()))
  }

  # Wrapper around list2env with a NULL check. In R <3.2.0, if an empty unnamed
  # list is passed to list2env(), it errors. But an empty named list is OK. For
  # R >=3.2.0, this wrapper is not necessary.
  # @param empty_to_null Controls what to do when x is NULL or empty list.
  #   If TRUE, return NULL. If FALSE, return an empty list.
  list2env2 <- function(x, envir = NULL, parent = emptyenv(),
                        hash = (length(x) >  100),
                        size = max(29L, length(x)),
                        empty_to_null = TRUE) {
    if (is.null(envir)) {
      envir <- new.env(hash = hash, parent = parent, size = size)
    }
    if (length(x) == 0) {
      if (empty_to_null)
        return(NULL)
      else
        return(envir)
    }
    list2env(x, envir)
  }
})
