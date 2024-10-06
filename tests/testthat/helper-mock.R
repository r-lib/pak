fake <- local({
  fake_through_tree <- function(tree, what, how) {
    for (d in tree) {
      for (parent in d) {
        parent_env <- parent[["parent_env"]]
        func_dict <- parent[["funcs"]]
        for (func_name in ls(func_dict, all.names = TRUE)) {
          func <- func_dict[[func_name]]
          func_env <- new.env(parent = environment(func))

          what <- override_seperators(what, func_env)
          where_name <- override_seperators(func_name, parent_env)

          if (!is.function(how)) {
            assign(what, function(...) how, func_env)
          } else {
            assign(what, how, func_env)
          }

          environment(func) <- func_env
          locked <- exists(where_name, parent_env, inherits = FALSE) &&
            bindingIsLocked(where_name, parent_env)
          if (locked) {
            baseenv()$unlockBinding(where_name, parent_env)
          }
          assign(where_name, func, parent_env)
          if (locked) {
            lockBinding(where_name, parent_env)
          }
        }
      }
    }
  }

  override_seperators <- function(name, env) {
    mangled_name <- NULL
    for (sep in c("::", "$")) {
      if (grepl(sep, name, fixed = TRUE)) {
        elements <- strsplit(name, sep, fixed = TRUE)
        mangled_name <- paste(
          elements[[1L]][1L],
          elements[[1L]][2L],
          sep = "XXX"
        )

        stub_list <- c(mangled_name)
        if ("stub_list" %in% names(attributes(get(sep, env)))) {
          stub_list <- c(stub_list, attributes(get(sep, env))[["stub_list"]])
        }

        create_new_name <- create_create_new_name_function(
          stub_list,
          env,
          sep
        )
        assign(sep, create_new_name, env)
      }
    }
    mangled_name %||% name
  }

  backtick <- function(x) {
    encodeString(x, quote = "`", na.encode = FALSE)
  }

  create_create_new_name_function <- function(stub_list, env, sep) {
    force(stub_list)
    force(env)
    force(sep)

    create_new_name <- function(pkg, func) {
      pkg_name <- deparse(substitute(pkg))
      func_name <- deparse(substitute(func))
      for (stub in stub_list) {
        if (paste(pkg_name, func_name, sep = "XXX") == stub) {
          return(eval(parse(text = backtick(stub)), env))
        }
      }

      # used to avoid recursively calling the replacement function
      eval_env <- new.env(parent = parent.frame())
      assign(sep, eval(parse(text = paste0("`", sep, "`"))), eval_env)

      code <- paste(pkg_name, backtick(func_name), sep = sep)
      return(eval(parse(text = code), eval_env))
    }
    attributes(create_new_name) <- list(stub_list = stub_list)
    create_new_name
  }

  build_function_tree <- function(test_env, where, where_name) {
    func_dict <- new.env()
    func_dict[[where_name]] <- where
    tree <- list(
      list(
        list(parent_env = test_env, funcs = func_dict)
      )
    )

    tree
  }

  fake <- function(where, what, how) {
    where_name <- deparse(substitute(where))
    stopifnot(is.character(what), length(what) == 1)
    test_env <- parent.frame()
    tree <- build_function_tree(test_env, where, where_name)
    fake_through_tree(tree, what, how)
  }
})
