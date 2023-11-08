
repoman_data <- new.env(parent = emptyenv())

`%||%` <- function(l, r) if (is.null(l)) r else l

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = character(1), ...)
}

viapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = integer(1), ...)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}

vdapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = double(1), ...)
}

# Like mapply, but better recycling rules, and no simplifying

mapx <- function(...) {
  args <- list(...)
  if (length(args) == 0) stop("No arguments to `mapx()`")
  fun <- args[[length(args)]]
  if (!is.function(fun)) stop("Last `mapx()` argument not a function")
  if (length(args) == 1) stop("No data to `mapx()`")
  data <- args[-length(args)]

  lens <- setdiff(unique(viapply(data, length)), 1L)
  if (any(lens == 0)) {
    data <- lapply(data, function(x) { length(x) <- 0; x })
    lens <- 0
  }
  if (length(lens) > 1) {
    stop(
      "Incompatible data lengths in `mapx()`: ",
       paste(lens, collapse = ", ")
    )
  }

  do.call(
    mapply,
    c(list(FUN = fun, SIMPLIFY = FALSE, USE.NAMES = FALSE), data)
  )
}

lapply_rows <-  function(df, fun, ...) {
  lapply(seq_len(nrow(df)), function(i) fun(df[i,], ...))
}

zip_vecs <- function(...) {
  mapply(c, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

add_attr <- function(x, attr, value) {
  attr(x, attr) <- value
  x
}

# Why are we using this?
# AFAICT the only difference is that if `getOption("encoding")` is set,
# then it is observed in `file()` and then `readLines()` converts the
# file to UTF-8. (At least on R 4.1, earlier versions are probably
# different.)
#
# OTOH it seems that we only use it to read the etag from a file, plus
# in some test cases, so it should not matter much.

read_lines <- function(con, ...) {
  if (is.character(con)) {
    con <- file(con)
    on.exit(close(con))
  }
  readLines(con, ...)
}

dep_types_hard <- function() c("Depends", "Imports", "LinkingTo")
dep_types_soft <- function() c("Suggests", "Enhances")
dep_types <- function() c(dep_types_hard(), dep_types_soft())

interpret_dependencies <- function(dp) {
  hard <- dep_types_hard()

  res <- if (isTRUE(dp)) {
    list(c(hard, "Suggests"), hard)

  } else if (identical(dp, FALSE)) {
    list(character(), character())

  } else if (is_na_scalar(dp)) {
    list(hard, hard)

  } else if (is.list(dp) && all(names(dp) == c("direct", "indirect"))) {
    dp

  } else {
    list(dp, dp)
  }

  names(res) <- c("direct", "indirect")
  res
}

## TODO: in theory the set of base packages can change over time,
## so we would need an R version specific vector here.
## Not an issue currently, might be in the future.

base_packages <- function() {
  if (is.null(repoman_data$base_packages)) {
    repoman_data$base_packages <-
      parse_installed(.Library, priority="base")$Package
  }
  repoman_data$base_packages
}

recommended_packages <- function() {
  if (is.null(repoman_data$recommended_packages)) {
    repoman_data$recommended_packages <- c(
      "boot", "class", "cluster", "codetools", "foreign", "KernSmooth",
      "lattice", "MASS", "Matrix", "mgcv", "nlme", "nnet", "rpart",
      "spatial", "survival"
    )
  }
  repoman_data$recommended_packages
}

is_na_scalar <- function(x) {
  identical(x, NA_character_) ||
    identical(x, NA_integer_) ||
    identical(x, NA_real_) ||
    identical(x, NA_complex_) ||
    identical(x, NA)
}

drop_nulls <- function(x)  {
  x[! vlapply(x, is.null)]
}

null2na <- function(x) {
  x %||% NA_character_
}

na_omit <- function(x) {
  x[!is.na(x)]
}

shasum256 <- function(x) {
  cli::hash_file_sha256(x)
}

file.size <- function (...) {
  file.info(...)$size
}

msg_wrap <- function(..., .space = TRUE) {
  ret <- paste(strwrap(paste0(...)), collapse = "\n")
  if (.space) ret <- paste0("\n", ret, "\n")
  ret
}

try_catch_null <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

run_examples <- function() {
  if (Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") == "") {
    # If this is not a check, then OK
    TRUE
  } else if (identical(Sys.getenv("NOT_CRAN"), "true") &&
      isTRUE(as.logical(Sys.getenv("CI")))) {
    # If NOT_CRAN is set and we are on the CI, then we run examples
    TRUE
  } else {
    FALSE
  }
}

modify_vec <- function(old, new) {
  old <- as.list(old)
  new <- as.list(new)
  unlist(utils::modifyList(old, new))
}

last <- function(x) {
  x[[length(x)]]
}

get_os_type <- function() {
  .Platform$OS.type
}

encode_path <- function(path) {
  if (get_os_type() == "windows") {
    enc2utf8(path)
  } else {
    enc2native(path)
  }
}

gzip_decompress <- function(from, chunk_size = 5 * 1000 * 1000) {
  con <- gzcon(rawConnection(from))
  on.exit(close(con), add = TRUE)
  pieces <- list()
  while (1) {
    pieces[[length(pieces) + 1]] <- readBin(con, what = "raw", n = chunk_size)
    if (length(pieces[[length(pieces)]]) == 0) break;
  }
  do.call("c", pieces)
}

save_rds <- function(data, path) {
  saveRDS(data, file = path, version = 2, compress = FALSE)
}

nullfile <- function() {
  if (get_os_type() == "windows") "nul:" else "/dev/null"
}

is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}
