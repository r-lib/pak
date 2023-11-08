# This function will be added as a method to R6 objects, with the name 'clone',
# and with the environment changed.
generator_funs$clone_method <- function(deep = FALSE) {
  # Need to embed these utility functions inside this closure because the
  # environment of this function will change.

  # This takes a list of objects and a list of pairs of environments. For each
  # object, if it is a function, this checks if that function's environment is
  # the same as any of the `old` members of the pairs; if so, it will change
  # the function's environment to the matching `new` member. If the function's
  # environment is not found in the list, then it will not be touched.
  remap_func_envs <- function(objs, old_new_env_pairs) {
    lapply(objs, function(x) {
      if (is.function(x)) {
        func_env <- environment(x)

        for (i in seq_along(old_new_env_pairs)) {
          if (identical(func_env, old_new_env_pairs[[i]]$old)) {
            environment(x) <- old_new_env_pairs[[i]]$new
            break
          }
        }
      }

      x
    })
  }

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

  # ---------------------------------------------------------------------------
  # Create representation of the old object
  # ---------------------------------------------------------------------------
  old <- list(
    list(
      enclosing = .subset2(self, ".__enclos_env__"),
      binding   = self,      # AKA the public binding environment
      private   = NULL
    )
  )

  if (!is.environment(old[[1]]$enclosing)) {
    stop("clone() must be called from an R6 object.")
  }

  old[[1]]$private <- old[[1]]$enclosing$private
  has_private <- !is.null(old[[1]]$private)

  # Figure out if we're in a portable class object
  portable <- !identical(old[[1]]$binding, old[[1]]$enclosing)

  # Traverse the super binding and enclosing environments, and add them to the
  # list.
  i <- 1
  while (TRUE) {
    if (is.null(old[[i]]$enclosing$super)) {
      break
    }

    old[[i+1]] <- list(
      binding   = old[[i]]$enclosing$super,
      enclosing = old[[i]]$enclosing$super$.__enclos_env__
    )

    i <- i + 1
  }

  # Set up stuff for deep clones
  if (deep) {
    if (has_private && is.function(old[[1]]$private$deep_clone)) {
      # Get private$deep_clone, if available.
      deep_clone <- old[[1]]$private$deep_clone
    } else {
      # If there's no private$deep_clone, then this default function will copy
      # fields that are R6 objects.
      deep_clone <- function(name, value) {
        # Check if it's an R6 object.
        if (is.environment(value) && !is.null(value$`.__enclos_env__`)) {
          return(value$clone(deep = TRUE))
        }
        value
      }
    }
  }

  # We'll use these a lot later, and it's faster to refer to them directly.
  old_1_binding <- old[[1]]$binding
  old_1_private <- old[[1]]$private

  # ---------------------------------------------------------------------------
  # Create representation of the new object
  # ---------------------------------------------------------------------------

  # The object representation is made up of a list of "slices". Each slice
  # represents one level of inheritance. The first slice is somewhat different
  # from subsequent ones. The later ones are superclass slices. They do not
  # have a separate `private` environment.

  # Create the first slice. This one is different from the others.
  make_first_new_slice <- function(old_slice, portable) {
    new_slice <- list(
      enclosing = NULL,
      binding   = NULL
    )

    has_private <- !is.null(old_slice$private)

    if (portable) {
      enclosing_parent <- parent.env(old_slice$enclosing)
      binding_parent   <- emptyenv()

      if (has_private) {
        private_parent    <- emptyenv()
        new_slice$private <- new.env(private_parent, hash = FALSE)
      }
      new_slice$binding   <- new.env(binding_parent,   hash = FALSE)
      new_slice$enclosing <- new.env(enclosing_parent, hash = FALSE)

    } else {
      if (has_private) {
        private_parent    <- parent.env(old_slice$private)
        new_slice$private <- new.env(private_parent, hash = FALSE)

        binding_parent    <- new_slice$private
        new_slice$binding <- new.env(binding_parent, hash = FALSE)

      } else {
        binding_parent    <- parent.env(old_slice$binding)
        new_slice$binding <- new.env(binding_parent, hash = FALSE)
      }

      new_slice$enclosing <- new_slice$binding
    }

    # Set up self, private, and .__enclos_env__
    new_slice$enclosing$self <- new_slice$binding
    if (has_private) {
      new_slice$enclosing$private <- new_slice$private
    }
    new_slice$binding$.__enclos_env__ <- new_slice$enclosing

    new_slice
  }


  # This creates a slice other than the first one.
  make_new_slice <- function(old_slice, self, private, enclosing_parent) {
    enclosing <- new.env(enclosing_parent, hash = FALSE)
    binding   <- new.env(emptyenv(), hash = FALSE)

    enclosing$self <- self
    if (!is.null(private)) {
      enclosing$private <- private
    }

    binding$.__enclos_env__ <- enclosing

    list(
      enclosing = enclosing,
      binding = binding
    )
  }

  new <- list(
    make_first_new_slice(old[[1]], portable)
  )

  # We'll use these a lot, and it's faster to refer to them directly.
  new_1_binding   <- new[[1]]$binding
  new_1_private   <- new[[1]]$private
  new_1_enclosing <- new[[1]]$enclosing

  # Mirror the super environments from the old object
  if (length(old) > 1) {
    for (i in seq.int(2, length(old))) {
      if (portable) {
        enclosing_parent <- parent.env(old[[i]]$enclosing)
      } else {
        enclosing_parent <- new_1_enclosing
      }

      new[[i]] <- make_new_slice(
        old[[i]],
        new_1_binding,
        new_1_private,
        enclosing_parent
      )
    }

    # A second pass to add in the `super` to each enclosing environment.
    for (i in seq.int(1, length(old)-1)) {
      new[[i]]$enclosing$super <- new[[i+1]]$binding
    }
  }

  # ---------------------------------------------------------------------------
  # Copy members from old to new
  # ---------------------------------------------------------------------------
  copy_slice <- function(old_slice, new_slice, old_new_enclosing_pairs, first_slice = FALSE) {

    # Copy the old objects, fix up method environments, and put them into the
    # new binding environment.

    # Separate active and non-active bindings. We'll copy over just the
    # non-active bindings now; the active bindings need to be copied over with
    # a different method later.
    binding_names <- ls(old_slice$binding, all.names = TRUE)
    if (!is.null(old_slice$enclosing$`.__active__`)) {
      binding_names <- setdiff(binding_names, names(old_slice$enclosing$`.__active__`))
    }

    binding_copies <- mget(binding_names, envir = old_slice$binding)

    # Don't copy self, private, super, or .__enclos_env__. Note that using
    # %in% is significantly faster than setdiff() here.
    keep_idx <- !(names(binding_copies) %in% c("self", "private", "super", ".__enclos_env__"))
    binding_copies <- binding_copies[keep_idx]

    binding_copies <- remap_func_envs(binding_copies, old_new_enclosing_pairs)

    if (deep) {
      binding_copies <- mapply(
        deep_clone,
        names(binding_copies),
        binding_copies,
        SIMPLIFY = FALSE
      )
    }

    # Copy in public bindings
    list2env2(binding_copies, new_slice$binding)


    # Now copy over active bindings, if present
    if (!is.null(old_slice$enclosing$`.__active__`)) {
      active_copies <- remap_func_envs(old_slice$enclosing$`.__active__`, old_new_enclosing_pairs)

      for (name in names(active_copies)) {
        makeActiveBinding(name, active_copies[[name]], new_slice$binding)
      }
      new_slice$enclosing$`.__active__` <- active_copies
    }

    # Copy private members
    if (!is.null(old_slice$private)) {
      private_copies <- as.list.environment(old_slice$private, all.names = TRUE)
      if (deep) {
        private_copies <- mapply(
          deep_clone,
          names(private_copies),
          private_copies,
          SIMPLIFY = FALSE
        )
      }
      private_copies <- remap_func_envs(private_copies, old_new_enclosing_pairs)
      list2env2(private_copies, new_slice$private)
    }

    # With the first slice, lock the methods. For other slices, there's no
    # need to lock lock methods because they're not publicly accessible.
    if (first_slice) {
      # A list of the possible environments for methods.
      method_envs <- lapply(old_new_enclosing_pairs, `[[`, "new")

      # Returns TRUE if the object is a method (or active binding), FALSE
      # otherwise. Functions that are not methods result in FALSE.
      is_method <- function(f, method_envs) {
        env <- environment(f)

        for (i in seq_along(method_envs)) {
          if (identical(env, method_envs[[i]])) {
            return(TRUE)
          }
        }
        FALSE
      }

      for (name in names(binding_copies)) {
        if (is_method(new_slice$binding[[name]], method_envs))
          lockBinding(name, new_slice$binding)
      }
      if (has_private) {
        for (name in names(private_copies)) {
          if (is_method(new_slice$private[[name]], method_envs))
            lockBinding(name, new_slice$private)
        }
      }
    }
  }


  old_new_enclosing_pairs <- list()
  for (i in seq_along(old)) {
    old_new_enclosing_pairs[[i]] <- list(
      old = old[[i]]$enclosing,
      new = new[[i]]$enclosing
    )
  }

  for (i in seq_along(old)) {
    # Only need to pass along the old/new pairs from i and above, because a
    # superclass's function will never have an enclosing environment from a
    # subclass.
    copy_slice(
      old[[i]],
      new[[i]],
      old_new_enclosing_pairs[seq.int(i, length(old))],
      (i == 1)
    )
  }

  # Lock --------------------------------------------------------------
  # Copy locked state of environment
  if (environmentIsLocked(old_1_binding)) {
    lockEnvironment(new_1_binding)
  }
  if (has_private && environmentIsLocked(old_1_private)) {
    lockEnvironment(new_1_private)
  }

  # Finalizer -------------------------------------------------------
  if (is.function(.subset2(new_1_binding, "finalize"))) {
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
      new_1_binding,
      finalizer_wrapper,
      onexit = TRUE
    )
  }

  if (has_private) {
    if (is.function(.subset2(new_1_private, "finalize"))) {
      finalizer_wrapper <- function(e) {
        .subset2(e, ".__enclos_env__")$private$finalize()
      }
      environment(finalizer_wrapper) <- baseenv()
      reg.finalizer(
        new_1_binding,
        finalizer_wrapper,
        onexit = TRUE
      )
    }
  }

  class(new_1_binding) <- class(old_1_binding)

  new_1_binding
}
