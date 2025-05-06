# Note: For S4, the value is the class defintion. The slots (data) are in the
# attributes.
pack <- function(obj, ...) {
  # encode by storage mode
  encoding.mode <- typeof(obj)

  # null may not have attributes
  if (encoding.mode == "NULL" || identical(obj, as.name('\001NULL\001'))) {
    return(list(type = as.scalar("NULL")))
  }

  # needed because formals become attributes, etc
  if (encoding.mode == "closure") {
    obj <- as.list(obj)
  }

  # special exception
  if (encoding.mode == "environment" && isNamespace(obj)) {
    encoding.mode <- "namespace"
  }

  # Strip off 'class' from S4 attributes
  attrib <- attributes(obj)
  if (isS4(obj)) {
    encoding.mode <- "S4"
    if (".Data" %in% slotNames(obj)) {
      attrib[[".Data"]] = obj@.Data
    }
    attrib <- attrib[slotNames(obj)]
    names(attrib) <- slotNames(obj)
  }

  # encode recursively
  list(
    type = as.scalar(encoding.mode),
    attributes = givename(lapply(attrib, pack, ...)),
    value = switch(
      encoding.mode,
      environment = NULL,
      externalptr = NULL,
      namespace = lapply(as.list(getNamespaceInfo(obj, "spec")), as.scalar),
      S4 = list(class = as.scalar(class(obj)), package = as.scalar(attr(class(obj), "package"))),
      raw = as.scalar(base64_enc(unclass(obj))),
      logical = as.vector(unclass(obj), mode = "logical"),
      integer = as.vector(unclass(obj), mode = "integer"),
      numeric = as.vector(unclass(obj), mode = "numeric"),
      double = as.vector(unclass(obj), mode = "double"),
      character = as.vector(unclass(obj), mode = "character"),
      complex = as.vector(unclass(obj), mode = "complex"),
      list = unname(lapply(unclass(obj), pack, ...)),
      pairlist = unname(lapply(as.vector(obj, mode = "list"), pack, ...)),
      closure = unname(lapply(obj, pack, ...)),
      builtin = as.scalar(base64_enc(serialize(unclass(obj), NULL))),
      special = as.scalar(base64_enc(serialize(unclass(obj), NULL))),
      language = deparse(unclass(obj)),
      name = deparse(unclass(obj)),
      symbol = deparse(unclass(obj)),
      expression = deparse(obj[[1]]),
      warning("No encoding has been defined for objects with storage mode ", encoding.mode, " and will be skipped.")
    )
  )
}

unpack <- function(obj) {
  encoding.mode <- obj$type

  # functions are special
  if (encoding.mode == "NULL") {
    return(NULL)
  }

  if (identical(encoding.mode, "S4")) {
    obj_class <- load_s4_class(obj$value$class, package = obj$value$package)
    obj_data <- lapply(obj$attributes, unpack)
    if (!length(obj_class)) obj_class <- obj$value$class
    return(do.call(new, c(Class = obj_class, obj_data)))
  }

  newdata <- c(
    list(
      .Data = switch(
        encoding.mode,
        environment = new.env(parent = emptyenv()),
        namespace = getNamespace(obj$value$name),
        externalptr = NULL,
        raw = base64_dec(obj$value),
        logical = as.logical(list_to_vec(obj$value)),
        integer = as.integer(list_to_vec(obj$value)),
        numeric = as.numeric(list_to_vec(obj$value)),
        double = as.double(list_to_vec(obj$value)),
        character = as.character(list_to_vec(obj$value)),
        complex = as.complex(list_to_vec(obj$value)),
        list = lapply(obj$value, unpack),
        pairlist = lapply(obj$value, unpack),
        symbol = makesymbol(x = unlist(obj$value)),
        name = makesymbol(x = unlist(obj$value)),
        expression = parse(text = obj$value),
        language = as.call(parse(text = unlist(obj$value)))[[1]],
        special = unserialize(base64_dec(obj$value)),
        builtin = unserialize(base64_dec(obj$value)),
        closure = lapply(obj$value, unpack),
        stop("Switch falling through for encode.mode: ", encoding.mode)
      )
    ),
    lapply(obj$attributes, unpack)
  )

  # this is for serializing functions arguments: as.list(lm)$data
  if (identical(newdata[[1]], substitute())) {
    return(substitute())
  }

  # build the output object
  output <- do.call("structure", newdata, quote = TRUE)

  # functions are special
  if (encoding.mode == "closure") {
    myfn <- as.function(output)
    environment(myfn) <- globalenv()
    return(myfn)
  }

  # functions are special
  if (encoding.mode == "pairlist") {
    return(as.pairlist(output))
  }

  # try to fix native symbols
  if (is(output, "NativeSymbolInfo")) {
    try(output <- fixNativeSymbol(output))
  }

  # return
  return(output)
}

load_s4_class <- function(name, package) {
  cls <- tryCatch(
    {
      getClassDef(name, package = package)
    },
    error = function(e) {
      stop(sprintf("Failed to load S4 class definition '%s' from '%s' (%s)", name, package, e$message), call. = FALSE)
    }
  )
}
