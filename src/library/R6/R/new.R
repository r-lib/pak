# This is the $new function for a R6ClassGenerator. This copy of it won't run
# properly; it needs to be copied, and its parent environment set to the
# generator object environment.
generator_funs$new <- function(...) {
  # Get superclass object -------------------------------------------
  inherit <- get_inherit()

  # Some checks on superclass ---------------------------------------
  if (!is.null(inherit)) {
    if (!inherits(inherit, "R6ClassGenerator"))
      stop("`inherit` must be a R6ClassGenerator.")

    if (!identical(portable, inherit$portable))
      stop("Sub and superclass must both be portable or non-portable.")

    # Merge fields over superclass fields, recursively --------------
    recursive_merge <- function(obj, which) {
      if (is.null(obj)) return(NULL)
      merge_vectors(recursive_merge(obj$get_inherit(), which), obj[[which]])
    }
    public_fields  <- merge_vectors(recursive_merge(inherit, "public_fields"),
                                    public_fields)
    private_fields <- merge_vectors(recursive_merge(inherit, "private_fields"),
                                    private_fields)
  }

  if (class) {
    classes <- c(classname, get_superclassnames(inherit), "R6")
  } else {
    classes <- NULL
  }

  # Precompute some things ------------------------------------------
  has_priv <- has_private()


  # Create binding and enclosing environments -----------------------
  if (portable) {
    # When portable==TRUE, the public binding environment is separate from the
    # enclosing environment.

    # Binding environment for private objects (where private objects are found)
    if (has_priv)
      private_bind_env <- new.env(parent = emptyenv(), hash = FALSE)
    else
      private_bind_env <- NULL

    # Binding environment for public objects (where public objects are found)
    public_bind_env <- new.env(parent = emptyenv(), hash = FALSE)

    # The enclosing environment for methods
    enclos_env <- new.env(parent = parent_env, hash = FALSE)

  } else {
    # When portable==FALSE, the public binding environment is the same as the
    # enclosing environment.
    # If present, the private binding env is the parent of the public binding
    # env.
    if (has_priv) {
      private_bind_env <- new.env(parent = parent_env, hash = FALSE)
      public_bind_env <- new.env(parent = private_bind_env, hash = FALSE)
    } else {
      private_bind_env <- NULL
      public_bind_env <- new.env(parent = parent_env, hash = FALSE)
    }

    enclos_env <- public_bind_env
  }

  # Add self and private pointer ------------------------------------
  enclos_env$self <- public_bind_env
  if (has_priv)
    enclos_env$private <- private_bind_env

  # Fix environment for methods -------------------------------------
  public_methods <- assign_func_envs(public_methods, enclos_env)
  if (has_priv)
    private_methods <- assign_func_envs(private_methods, enclos_env)
  if (!is.null(active))
    active <- assign_func_envs(active, enclos_env)

  # Enable debugging ------------------------------------------------
  if (length(debug_names) > 0) {
    lapply(public_methods[names(public_methods) %in% debug_names], base::debug)
    lapply(private_methods[names(private_methods) %in% debug_names], base::debug)
    lapply(active[names(active) %in% debug_names], base::debug)
  }

  # Set up superclass objects ---------------------------------------
  if (!is.null(inherit)) {
    if (portable) {
      # Set up the superclass objects
      super_struct <- create_super_env(inherit, public_bind_env,
                                       private_bind_env, portable = TRUE,
                                       cloneable = cloneable)
    } else {
      # Set up the superclass objects
      super_struct <- create_super_env(inherit, public_bind_env, portable = FALSE,
                                       cloneable = cloneable)
    }

    enclos_env$super <- super_struct$bind_env

    # Merge this level's methods over the superclass methods
    public_methods  <- merge_vectors(super_struct$public_methods, public_methods)
    private_methods <- merge_vectors(super_struct$private_methods, private_methods)
    active          <- merge_vectors(super_struct$active, active)
  }

  # Copy objects to public bind environment -------------------------
  list2env2(public_methods, envir = public_bind_env)
  list2env2(public_fields, envir = public_bind_env)

  # Copy objects to private bind environment ------------------------
  if (has_priv) {
    list2env2(private_methods, envir = private_bind_env)
    list2env2(private_fields, envir = private_bind_env)
  }

  # Set up active bindings ------------------------------------------
  if (!is.null(active)) {
    for (name in names(active)) {
      makeActiveBinding(name, active[[name]], public_bind_env)
    }

    # If there are active bindings, then we need to store a copy of the active
    # bindings in case the object is cloned. This is because as of R 4.0,
    # there's no way to get the function associated with an active binding;
    # you can only get the return value.
    enclos_env$`.__active__` <- active
  }


  # Add refs to other environments in the object --------------------
  public_bind_env$`.__enclos_env__` <- enclos_env

  # Lock ------------------------------------------------------------
  if (lock_objects) {
    if (has_priv) lockEnvironment(private_bind_env)
    lockEnvironment(public_bind_env)
  }

  # Always lock methods
  if (has_priv) {
    for (name in names(private_methods))
      lockBinding(name, private_bind_env)
  }
  for (name in names(public_methods))
    lockBinding(name, public_bind_env)

  class(public_bind_env) <- classes

  # Initialize ------------------------------------------------------
  initialize <- .subset2(public_bind_env, "initialize")
  if (is.function(initialize)) {
    initialize(...)
  } else if (length(list(...)) != 0 ) {
    stop("Called new() with arguments, but there is no initialize method.")
  }

  # Finalizer -------------------------------------------------------
  if (is.function(.subset2(public_bind_env, "finalize"))) {
    # This wraps the user's `finalize` method. The user's finalize method
    # typically does not have an `e` argument, so the wrapper needs to consume
    # the `e` argument.
    finalizer_wrapper <- function(e) {
      .subset2(e, "finalize")()
    }
    # Reassign the wrapper's environment so that it does not capture the current
    # environment and prevent objects from getting GC'd.
    environment(finalizer_wrapper) <- baseenv()

    reg.finalizer(
      public_bind_env,
      finalizer_wrapper,
      onexit = TRUE
    )
  }

  if (has_priv) {
    if (is.function(.subset2(private_bind_env, "finalize"))) {
      finalizer_wrapper <- function(e) {
        .subset2(e, ".__enclos_env__")$private$finalize()
      }
      environment(finalizer_wrapper) <- baseenv()
      reg.finalizer(
        public_bind_env,
        finalizer_wrapper,
        onexit = TRUE
      )
    }
  }

  public_bind_env
}


encapsulate({
  # Create and populate the self$super environment, for non-portable case.
  # In this function, we "climb to the top" of the superclass hierarchy by
  # recursing early on in the function, and then fill the methods downward by
  # doing the work for each level and passing the needed information down.
  create_super_env <- function(inherit, public_bind_env, private_bind_env = NULL,
                               portable = TRUE, cloneable = TRUE) {
    public_methods  <- inherit$public_methods
    private_methods <- inherit$private_methods
    active          <- inherit$active

    # Set up super enclosing and binding environments -------------------

    # The environment in which functions run is a child of the public bind env
    # (AKA self).
    # For portable classes, this is a child of the superclass's parent env.
    # For non-portable classes, this is a child of self; however, self has no
    # bindings that point to it. The only reason this environment is needed is so
    # that if a function super$foo in turn calls super$bar, it will be able to
    # find bar from the next superclass up.
    if (portable)
      enclos_parent <- inherit$parent_env
    else
      enclos_parent <- public_bind_env

    super_enclos_env <- new.env(parent = enclos_parent, hash = FALSE)

    # The binding environment is a new environment. Its parent doesn't matter
    # because it's not the enclosing environment for any functions.
    super_bind_env <- new.env(parent = emptyenv(), hash = FALSE)

    # Need to store the enclosing environment for cloning.
    super_bind_env$.__enclos_env__ <- super_enclos_env

    # Add self/private pointers -----------------------------------------
    if (portable) {
      super_enclos_env$self <- public_bind_env
      if (!is.null(private_bind_env))
        super_enclos_env$private <- private_bind_env
    }

    # Set up method environments ----------------------------------------
    # All the methods can be found in self$super (the binding env).
    # Their enclosing env is a different environment.
    public_methods  <- assign_func_envs(public_methods, super_enclos_env)
    private_methods <- assign_func_envs(private_methods, super_enclos_env)
    active          <- assign_func_envs(active, super_enclos_env)

    # Recurse if there are more superclasses ----------------------------
    inherit_inherit <- inherit$get_inherit()
    if (!is.null(inherit_inherit)) {
      super_struct <- create_super_env(inherit_inherit, public_bind_env,
                                       private_bind_env, portable, cloneable)
      super_enclos_env$super <- super_struct$bind_env

      # Merge this level's methods over the superclass methods
      public_methods  <- merge_vectors(super_struct$public_methods, public_methods)
      private_methods <- merge_vectors(super_struct$private_methods, private_methods)
      active          <- merge_vectors(super_struct$active, active)
    }

    # Copy the methods into the binding environment ---------------------
    list2env2(public_methods, envir = super_bind_env)
    list2env2(private_methods, envir = super_bind_env)
    if (!is.null(active)) {
      for (name in names(active)) {
        makeActiveBinding(name, active[[name]], super_bind_env)
      }
      # If there are active bindings, then we need to store a copy of the
      # active bindings in case the object is cloned.
      super_enclos_env$`.__active__` <- active
    }

    # Return an object with all the information needed to merge down
    list(
      bind_env = super_bind_env,
      public_methods = public_methods,
      private_methods = private_methods,
      active = active
    )
  }
})
