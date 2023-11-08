
#' SHA-256 hash
#'
#' Calculate the SHA-256 hash of each element of a character vector.
#'
#' @param x Character vector. If not a character vector, then
#' [as.character()] is used to try to coerce it into one. `NA` entries
#' will have an `NA` hash.
#' @return `hash_sha256()` returns aharacter vector of hexadecimal
#' SHA-256 hashes.

#' @family hash functions
#'
#' @export
#' @examples
#' hash_sha256(c("foo", NA, "bar", ""))

hash_sha256 <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  na <- is.na(x)
  x[na] <- NA_character_
  x[!na] <- .Call(clic_sha256, x[!na])
  x
}

#' @export
#' @rdname hash_sha256
#' @details `hash_raw_sha256()` calculates the SHA-256 hash of the bytes
#' of a raw vector.
#'
#' @return `hash_raw_sha256()` returns a character scalar.

hash_raw_sha256 <- function(x) {
  stopifnot(is.raw(x))
  .Call(clic_sha256_raw, x)
}

#' @export
#' @rdname hash_sha256
#' @param serialize_version Workspace format version to use, see
#' [base::serialize()].
#' @details `hash_obj_sha256()` calculates the SHA-256 hash of an R
#' object. The object is serialized into a binary vector first.
#'
#' @return `hash_obj_sha256()` returns a character scalar.

hash_obj_sha256 <- function(x, serialize_version = 2) {
  sr <- serialize(x, NULL, version = serialize_version)[-(1:14)]
  hash_raw_sha256(sr)
}

#' @export
#' @rdname hash_sha256
#' @param paths Character vector of file names.
#' @details `hash_file_sha256()` calculates the SHA-256 hash of one or
#' more files.
#'
#' @return `hash_file_sha256()` returns a character vector of SHA-256
#' hashes.

hash_file_sha256 <- function(paths) {
  if (!is.character(paths)) paths <- as.character(paths)
  paths <- normalizePath(paths, mustWork = FALSE)
  if (is_windows()) {
    paths <- enc2utf8(paths)
  } else {
    paths <- enc2native(paths)
  }
  .Call(clic_sha256_file, paths)
}

#' SHA-1 hash
#'
#' Calculate the SHA-1 hash of each element of a character vector.
#'
#' @param x Character vector. If not a character vector, then
#' [as.character()] is used to try to coerce it into one. `NA` entries
#' will have an `NA` hash.
#' @return `hash_sha1()` returns aharacter vector of hexadecimal
#' SHA-1 hashes.

#' @family hash functions
#'
#' @export
#' @examples
#' hash_sha1(c("foo", NA, "bar", ""))

hash_sha1 <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  na <- is.na(x)
  x[na] <- NA_character_
  x[!na] <- .Call(clic_sha1, x[!na])
  x
}

#' @export
#' @rdname hash_sha1
#' @details `hash_raw_sha1()` calculates the SHA-1 hash of the bytes
#' of a raw vector.
#'
#' @return `hash_raw_sha1()` returns a character scalar.

hash_raw_sha1 <- function(x) {
  stopifnot(is.raw(x))
  .Call(clic_sha1_raw, x)
}

#' @export
#' @rdname hash_sha1
#' @param serialize_version Workspace format version to use, see
#' [base::serialize()].
#' @details `hash_obj_sha1()` calculates the SHA-1 hash of an R
#' object. The object is serialized into a binary vector first.
#'
#' @return `hash_obj_sha1()` returns a character scalar.

hash_obj_sha1 <- function(x, serialize_version = 2) {
  sr <- serialize(x, NULL, version = serialize_version)[-(1:14)]
  hash_raw_sha1(sr)
}

#' @export
#' @rdname hash_sha1
#' @param paths Character vector of file names.
#' @details `hash_file_sha1()` calculates the SHA-1 hash of one or
#' more files.
#'
#' @return `hash_file_sha1()` returns a character vector of SHA-1
#' hashes.

hash_file_sha1 <- function(paths) {
  if (!is.character(paths)) paths <- as.character(paths)
  paths <- normalizePath(paths, mustWork = FALSE)
  if (is_windows()) {
    paths <- enc2utf8(paths)
  } else {
    paths <- enc2native(paths)
  }
  .Call(clic_sha1_file, paths)
}

#' MD5 hash
#'
#' Calculate the MD5 hash of each element of a character vector.
#'
#' @param x Character vector. If not a character vector, then
#' [as.character()] is used to try to coerce it into one. `NA` entries
#' will have an `NA` hash.
#' @return `hash_md5()` returns a character vector of hexadecimal MD5
#' hashes.
#'
#' @family hash functions
#' @seealso [tools::md5sum()] for a base R MD5 function that works on
#' files.
#'
#' @export
#' @examples
#' hash_md5(c("foo", NA, "bar", ""))

hash_md5 <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  na <- is.na(x)
  x[na] <- NA_character_
  x[!na] <- .Call(clic_md5, x[!na])
  x
}

#' @export
#' @rdname hash_md5
#' @details `hash_raw_md5()` calculates the MD5 hash of the bytes
#' of a raw vector.
#'
#' @return `hash_raw_md5()` returns a character scalar.

hash_raw_md5 <- function(x) {
  stopifnot(is.raw(x))
  .Call(clic_md5_raw, x)
}

#' @export
#' @rdname hash_md5
#' @param serialize_version Workspace format version to use, see
#' [base::serialize()].
#' @details `hash_obj_md5()` calculates the MD5 hash of an R
#' object. The object is serialized into a binary vector first.
#'
#' @return `hash_obj_md5()` returns a character scalar.

hash_obj_md5 <- function(x, serialize_version = 2) {
  sr <- serialize(x, NULL, version = serialize_version)[-(1:14)]
  hash_raw_md5(sr)
}

#' @export
#' @rdname hash_md5
#' @param paths Character vector of file names.
#' @details `hash_file_md5()` calcultaes the MD5 hash of one of more
#' files.

hash_file_md5 <- function(paths) {
  if (!is.character(paths)) paths <- as.character(paths)
  paths <- normalizePath(paths, mustWork = FALSE)
  if (is_windows()) {
    paths <- enc2utf8(paths)
  } else {
    paths <- enc2native(paths)
  }
  .Call(clic_md5_file, paths)
}

#' Emoji hash
#'
#' @details
#' It uses the first 13 hexadecimal characters (out of the 32) of the MD5
#' hash of the input, and converts them into an emoji representation.
#' It uses a manually selected subset of all emojis, that tend to be
#' displayed correctly.
#'
#' ## Number of possible hash values
#'
#' ```{r include = FALSE}
#' hf <- function(size) {
#'   format(nrow(emojis)**size, big.mark = ",", scientific = FALSE)
#' }
#' ```
#'
#' cli uses `r nrow(emojis)` possible emojis. This is the number of
#' different hashes you can get for different values of `size`:
#'
#' | `size` | size of hash table space |
#' | -----: | -----------------------: |
#' | 1      | `r hf(1)`                |
#' | 2      | `r hf(2)`                |
#' | 3      | `r hf(3)`                |
#' | 4      | `r hf(4)`                |
#'
#' @param x Character vector. `NA` entries will have an `NA` hash.
#' @param size Number of emojis to use in a hash. Currently it has to
#'   be between 1 and 4.
#' @return `hash_emoji()` returns a data frame with columns
#'   * `hash`: the emoji hash, a string of the requested size.
#'   * `emojis`: list column with the emoji characters in character
#'     vectors. Note that an emoji might have multiple code points.
#'   * `text`: text representation of `hash`, comma separated.
#'   * `names`: list column with the text representations of `emojis`, in
#'     character vectors.
#'
#' @family hash functions
#' @seealso the emoji package for a comprehensive list of emojis
#' @export
#' @examples
#' hash_emoji(c("foo", NA, "bar", ""))$text
#'
#' # if you increase `size`, the shorter hash is a prefix of the longer:
#' hash_emoji("foobar", 1)$text
#' hash_emoji("foobar", 2)$text
#' hash_emoji("foobar", 3)$text
#' hash_emoji("foobar", 4)$text

hash_emoji <- function(x, size = 3) {
  # our integer arithmetic does not work if size > 4
  if (!is.character(x)) x <- as.character(x)
  stopifnot(
    is.character(x),
    is_count(size),
    size >= 1 && size <= 4
  )

  hashes <- lapply(x, hash_emoji1, size = size)
  emojis <- lapply(hashes, "[[", "emoji")
  names <- lapply(hashes, "[[", "names")

  data.frame(
    stringsAsFactors = FALSE,
    hash = vapply(emojis, hash_collapse, character(1)),
    emojis = I(emojis),
    text = vapply(names, hash_collapse, character(1), sep = ", "),
    names = I(names)
  )
}

hash_collapse <- function(x, sep = "") {
  if (anyNA(x)) {
    NA_character_
  } else {
    paste(x, collapse = sep)
  }
}

hash_emoji1 <- function(x, size = 3) {
  if (is.na(x)) {
    return(list(
      emoji = rep(NA_character_, size),
      names = rep(NA_character_, size)
    ))
  }

  md5 <- hash_md5(x)
  hash_emoji1_transform(md5, size)
}

hash_emoji1_transform <- function(md5, size) {
  md513 <- substr(md5, 1, 13)
  mdint <- as.integer(as.hexmode(strsplit(md513, "")[[1]]))
  hash <- sum(mdint * 16^(0:12))

  base <- nrow(emojis)
  ehash <- hash %% (base ** size)
  digits <- integer()
  while (ehash > 0) {
    digits <-  c(digits, ehash %% base)
    ehash <- ehash %/% base
  }
  digits <- c(digits, rep(0, 10))[1:size]

  nms <- emojis$name[digits + 1]
  emo <- emojis$emoji[digits + 1]

  list(
    emoji = emo,
    names = nms
  )
}

#' @export
#' @rdname hash_emoji
#' @details `hash_raw_emoji()` calculates the emoji hash of the bytes
#' of a raw vector.
#'
#' @return `hash_raw_emoji()` and `hash_obj_emoji()` return a list with
#' entries:
#' * `hash`: the emoji hash, a string of requested size,
#' * `emojis`: the individual emoji characters in a character vector,
#' * `text`: text representation of `hash`, comma separated,
#' * `names`: names of the emojis, in a character vector.

hash_raw_emoji <- function(x, size = 3) {
  stopifnot(is.raw(x))
  md5 <- hash_raw_md5(x)
  emo <- hash_emoji1_transform(md5, size)

  list(
    hash = hash_collapse(emo$emoji),
    emojis = emo$emoji,
    text = hash_collapse(emo$emoji, sep = ", "),
    names = emo$names
  )
}

#' @export
#' @rdname hash_emoji
#' @param serialize_version Workspace format version to use, see
#' [base::serialize()].
#' @details `hash_obj_emoji()` calculates the emoji hash of an R
#' object. The object is serialized into a binary vector first.

hash_obj_emoji <- function(x, size = 3, serialize_version = 2) {
  sr <- serialize(x, NULL, version = serialize_version)[-(1:14)]
  hash_raw_emoji(sr, size = size)
}

#' Adjective-animal hash
#'
#' @details
#' It uses the first 13 hexadecimal characters (out of the 32) of the MD5
#' hash of the input, and converts them into an adjective-animal form to
#' create a human readable hash.
#'
#' ## Number of possible hash values
#'
#' ```{r include = FALSE}
#' hf <- function(n_adj) {
#'   format(
#'     length(gfycat_adjectives) ** n_adj * length(gfycat_animals),
#'     big.mark = ",",
#'     scientific = FALSE
#'   )
#' }
#' ```
#'
#' `hash_animals()` uses `r length(gfycat_animals)` animal names and
#' `r length(gfycat_adjectives)` different adjectives. The number of
#' different hashes you can get for different values of `n_adj`:
#'
#' | `n_adj` | size of the hash table space |
#' | ------: | ---------------------------: |
#' | 0       | `r hf(0)`                    |
#' | 1       | `r hf(1)`                    |
#' | 2       | `r hf(2)`                    |
#' | 3       | `r hf(3)`                    |
#'
#' ## Source
#'
#' The list of adjectives and animals comes from the ids package,
#' and in turn from
#' <https://github.com/a-type/adjective-adjective-animal>, and
#' from <https://gfycat.com>.
#'
#' @param x Character vector. `NA` entries will have an `NA` hash.
#' @param n_adj Number of adjectives to use. It must be between 0 and 3.
#' @return A data frame with columns
#'   * `hash`: the hash value, a string.
#'   * `words`: list column with the adjectives and the animal name in a
#'     character vector.
#'
#' @family hash functions
#' @seealso the ids package for generating random adjective-animal ids
#'
#' @export
#' @examples
#' hash_animal(c("foo", "bar"))
#'
#' # if you increase `n_adj`, the shorter hash is a suffix of the longer:
#' hash_animal("cli package", 0)$hash
#' hash_animal("cli package", 1)$hash
#' hash_animal("cli package", 2)$hash
#' hash_animal("cli package", 3)$hash

hash_animal <- function(x, n_adj = 2) {
  if (!is.character(x)) x <- as.character(x)
  stopifnot(
    is.character(x),
    is_count(n_adj),
    n_adj >= 0 && n_adj <= 3
  )

  hashes <- lapply(x, hash_animal1, n_adj = n_adj)

  data.frame(
    stringsAsFactors = FALSE,
    hash = vapply(hashes, hash_collapse, character(1), sep = " "),
    words = I(hashes)
  )
}

hash_animal1 <- function(x, n_adj = 2) {
  if (is.na(x)) {
    return(rep(NA_character_, n_adj + 1))
  }

  md5 <- hash_md5(x)
  hash_animal1_transform(md5, n_adj)
}

hash_animal1_transform <- function(md5, n_adj) {
  md513 <- substr(md5, 1, 13)
  mdint <- as.integer(as.hexmode(strsplit(md513, "")[[1]]))
  hash <- sum(mdint * 16^(0:12))

  len_ani <- length(gfycat_animals)
  len_adj <- length(gfycat_adjectives)
  ehash <- hash %% (len_adj ** n_adj * len_ani)
  digits <- ehash %% len_ani
  ehash <- ehash %/% len_ani

  while (ehash > 0) {
    digits <- c(digits, ehash %% len_adj)
    ehash <- ehash %/% len_adj
  }
  digits <- c(digits, rep(0, 10))[1:(n_adj + 1)]
  digits <- rev(digits)

  c(
    gfycat_adjectives[digits[-length(digits)] + 1],
    gfycat_animals[digits[length(digits)] + 1]
  )
}

#' @export
#' @rdname hash_animal
#' @details `hash_raw_anima()` calculates the adjective-animal hash of
#' the bytes of a raw vector.
#'
#' @return `hash_raw_animal()` and `hash_obj_animal()` return a list
#' with entries:
#' * `hash`: the hash value, a string,
#' * `words: the adjectives and the animal name in a character vector.

hash_raw_animal <- function(x, n_adj = 2) {
  stopifnot(is.raw(x))
  md5 <- hash_raw_md5(x)
  hash <- hash_animal1_transform(md5, n_adj)

  list(
    hash = hash_collapse(hash, sep = ", "),
    words = hash
  )
}

#' @export
#' @rdname hash_animal
#' @param serialize_version Workspace format version to use, see
#' [base::serialize()].
#' @details `hash_obj_animal()` calculates the adjective-animal hash of
#' an R object. The object is serialized into a binary vector first.

hash_obj_animal <- function(x, n_adj = 2, serialize_version = 2) {
  sr <- serialize(x, NULL, version = serialize_version)[-(1:14)]
  hash_raw_animal(sr, n_adj = n_adj)
}
