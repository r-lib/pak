#' @useDynLib zip, .registration = TRUE, .fixes = "c_"
NULL

call_with_cleanup <- function(ptr, ...) {
  # R_ExecWithCleanup (inside cleancall) pushes a CTXT_CCODE context with
  # call=R_NilValue, so Rf_error() loses the calling function name. Capture
  # it here and restore it on errors that arrive without a call.
  caller <- sys.call(sys.nframe() - 1)
  withCallingHandlers(
    .Call(c_cleancall_call, pairlist(ptr, ...), parent.frame()),
    error = function(e) {
      if (is.null(conditionCall(e))) {
        e$call <- caller
        stop(e)
      }
    }
  )
}

#' Compress Files into 'zip' Archives
#'
#' `zip()` creates a new zip archive file.
#'
#' `zip_append()` appends compressed files to an existing 'zip' file.
#'
#' ## Relative paths
#'
#' `zip()` and `zip_append()` can run in two different modes: mirror
#' mode and cherry picking mode. They handle the specified `files`
#' differently.
#'
#' ### Mirror mode
#'
#' Mirror mode is for creating the zip archive of a directory structure,
#' exactly as it is on the disk. The current working directory will
#' be the root of the archive, and the paths will be fully kept.
#' zip changes the current directory to `root` before creating the
#' archive.
#'
#' E.g. consider the following directory structure:
#'
#' ```{r echo = FALSE, comment = ""}
#' dir.create(tmp <- tempfile())
#' oldwd <- getwd()
#' setwd(tmp)
#' dir.create("foo/bar", recursive = TRUE)
#' dir.create("foo/bar2")
#' dir.create("foo2")
#' cat("this is file1", file = "foo/bar/file1")
#' cat("this is file2", file = "foo/bar/file2")
#' cat("this is file3", file = "foo2/file3")
#' out <- processx::run("tree", c("--noreport", "--charset=ascii"))
#' cat(crayon::strip_style(out$stdout))
#' setwd(oldwd)
#' ```
#'
#' Assuming the current working directory is `foo`, the following zip
#' entries are created by `zip`:
#' ```{r, echo = 2:4}
#' setwd(tmp)
#' setwd("foo")
#' zip::zip("../test.zip", c("bar/file1", "bar2", "../foo2"))
#' zip_list("../test.zip")[, "filename", drop = FALSE]
#' setwd(oldwd)
#' ```
#'
#' Note that zip refuses to store files with absolute paths, and chops
#' off the leading `/` character from these file names. This is because
#' only relative paths are allowed in zip files.
#'
#' ### Cherry picking mode
#'
#' In cherry picking mode, the selected files and directories
#' will be at the root of the archive. This mode is handy if you
#' want to select a subset of files and directories, possibly from
#' different paths and put all of them in the archive, at the top
#' level.
#'
#' Here is an example with the same directory structure as above:
#'
#' ```{r, echo = 3:4}
#' setwd(tmp)
#' setwd("foo")
#' zip::zip(
#'   "../test2.zip",
#'   c("bar/file1", "bar2", "../foo2"),
#'   mode = "cherry-pick"
#')
#' zip_list("../test2.zip")[, "filename", drop = FALSE]
#' setwd(oldwd)
#' ```
#'
#' From zip version 2.3.0, `"."` has a special meaning in the `files`
#' argument: it will include the files (and possibly directories) within
#' the current working directory, but **not** the working directory itself.
#' Note that this only applies to cherry picking mode.
#'
#' ## Permissions:
#'
#' `zip()` (and `zip_append()`, etc.) add the permissions of
#' the archived files and directories to the ZIP archive, on Unix systems.
#' Most zip and unzip implementations support these, so they will be
#' recovered after extracting the archive.
#'
#' Note, however that the owner and group (uid and gid) are currently
#' omitted, even on Unix.
#'
#' ## `zipr()` and `zipr_append()`
#'
#' These functions exist for historical reasons. They are identical
#' to `zip()` and `zip_append()` with a different default for the
#' `mode` argument.
#'
#' @param zipfile The zip file to create. If the file exists, `zip`
#'   overwrites it, but `zip_append` appends to it. If it is a directory
#'   an error is thrown.
#' @param files Character vector of paths to files to add to the archive.
#'   See details below about absolute and relative path names.
#' @param recurse Whether to add the contents of directories recursively.
#' @param compression_level A number between 1 and 9. 9 compresses best,
#'   but it also takes the longest.
#' @param include_directories Whether to explicitly include directories
#'   in the archive. Including directories might confuse MS Office when
#'   reading docx files, so set this to `FALSE` for creating them.
#' @param root Change to this working directory before creating the
#'   archive.
#' @param mode Selects how files and directories are stored in
#'   the archive. It can be `"mirror"` or `"cherry-pick"`.
#'   See "Relative Paths" below for details.
#' @param keys An optional character vector of the same length as `files`,
#'   specifying the paths of the corresponding entries inside the zip
#'   archive. For a file, the key is the exact archive path. For a
#'   directory, the key becomes the directory prefix under which all
#'   contents are stored. If `NULL` (default), paths are determined by
#'   `mode`. `"."` may not appear in `files` when `keys` is specified.
#' @param password Password for encrypting the archive entries. It can be a
#'   string, a raw vector of bytes, or a zero-argument function that returns
#'   one of these. If `NULL` (the default), the `zip_password` option is
#'   consulted; if that is also `NULL`, entries are stored unencrypted.
#'   The password is interpreted as UTF-8 bytes regardless of the current
#'   locale, which matches the WinZip/7-Zip convention and ensures
#'   interoperability across platforms.
#' @param encryption Encryption scheme to use when `password` is not `NULL`.
#'   `"aes256"` (the default) and `"aes128"` use WinZip AES encryption
#'   (AES-256 or AES-128 in CTR mode, key derived via PBKDF2-HMAC-SHA1,
#'   with an HMAC-SHA1 authentication tag). This scheme is supported by
#'   7-Zip, WinZip, and macOS Archive Utility. `"zipcrypto"` uses the
#'   legacy PKWARE ZipCrypto stream cipher, which is **cryptographically
#'   weak** and should only be used for compatibility with tools that do
#'   not support AES encryption.
#' @return The name of the created zip file, invisibly.
#'
#' @export
#' @examples
#' ## Some files to zip up. We will run all this in the R session's
#' ## temporary directory, to avoid messing up the user's workspace.
#' dir.create(tmp <- tempfile())
#' dir.create(file.path(tmp, "mydir"))
#' cat("first file", file = file.path(tmp, "mydir", "file1"))
#' cat("second file", file = file.path(tmp, "mydir", "file2"))
#'
#' zipfile <- tempfile(fileext = ".zip")
#' zip::zip(zipfile, "mydir", root = tmp)
#'
#' ## List contents
#' zip_list(zipfile)
#'
#' ## Add another file
#' cat("third file", file = file.path(tmp, "mydir", "file3"))
#' zip_append(zipfile, file.path("mydir", "file3"), root = tmp)
#' zip_list(zipfile)

zip <- function(
  zipfile,
  files,
  recurse = TRUE,
  compression_level = 9,
  include_directories = TRUE,
  root = ".",
  mode = c("mirror", "cherry-pick"),
  keys = NULL,
  password = NULL,
  encryption = c("aes256", "aes128", "zipcrypto")
) {
  mode <- match.arg(mode)
  encryption <- match.arg(encryption)
  zip_internal(
    zipfile,
    files,
    recurse,
    compression_level,
    append = FALSE,
    root = root,
    keep_path = (mode == "mirror"),
    include_directories = include_directories,
    keys = keys,
    password = password,
    encryption = encryption
  )
}

#' @rdname zip
#' @export

zipr <- function(
  zipfile,
  files,
  recurse = TRUE,
  compression_level = 9,
  include_directories = TRUE,
  root = ".",
  mode = c("cherry-pick", "mirror"),
  keys = NULL,
  password = NULL,
  encryption = c("aes256", "aes128", "zipcrypto")
) {
  mode <- match.arg(mode)
  encryption <- match.arg(encryption)
  zip_internal(
    zipfile,
    files,
    recurse,
    compression_level,
    append = FALSE,
    root = root,
    keep_path = (mode == "mirror"),
    include_directories = include_directories,
    keys = keys,
    password = password,
    encryption = encryption
  )
}

#' @rdname zip
#' @export

zip_append <- function(
  zipfile,
  files,
  recurse = TRUE,
  compression_level = 9,
  include_directories = TRUE,
  root = ".",
  mode = c("mirror", "cherry-pick"),
  keys = NULL,
  password = NULL,
  encryption = c("aes256", "aes128", "zipcrypto")
) {
  mode <- match.arg(mode)
  encryption <- match.arg(encryption)
  zip_internal(
    zipfile,
    files,
    recurse,
    compression_level,
    append = TRUE,
    root = root,
    keep_path = (mode == "mirror"),
    include_directories = include_directories,
    keys = keys,
    password = password,
    encryption = encryption
  )
}

#' @rdname zip
#' @export

zipr_append <- function(
  zipfile,
  files,
  recurse = TRUE,
  compression_level = 9,
  include_directories = TRUE,
  root = ".",
  mode = c("cherry-pick", "mirror"),
  keys = NULL,
  password = NULL,
  encryption = c("aes256", "aes128", "zipcrypto")
) {
  mode <- match.arg(mode)
  encryption <- match.arg(encryption)
  zip_internal(
    zipfile,
    files,
    recurse,
    compression_level,
    append = TRUE,
    root = root,
    keep_path = (mode == "mirror"),
    include_directories = include_directories,
    keys = keys,
    password = password,
    encryption = encryption
  )
}

zip_internal <- function(
  zipfile,
  files,
  recurse,
  compression_level,
  append,
  root,
  keep_path,
  include_directories,
  keys = NULL,
  password = NULL,
  encryption = "aes256"
) {
  if (!is.null(keys)) {
    if (length(keys) != length(files)) {
      stop("`keys` must have the same length as `files`")
    }
  }

  zipfile <- path.expand(zipfile)
  if (dir.exists(zipfile)) {
    stop("zip file at `", zipfile, "` already exists and it is a directory")
  }
  oldwd <- setwd(root)
  on.exit(setwd(oldwd), add = TRUE)

  if (!all(file.exists(files))) {
    stop("Some files do not exist")
  }

  data <- get_zip_data(
    files,
    recurse,
    keep_path,
    include_directories,
    keys = keys
  )
  data$key <- fix_absolute_paths(data$key)
  warn_for_colon(data$key)
  warn_for_dotdot(data$key)

  fi <- file.info(data$file, extra_cols = FALSE)
  show_progress <- is_progress_enabled()
  total_bytes <- if (show_progress) {
    sum(fi$size[!data$dir], na.rm = TRUE)
  } else {
    NA_real_
  }

  pw <- resolve_password(password)
  enc <- if (is.null(pw)) 0L else encryption_code(encryption)

  call_with_cleanup(
    c_R_zip_zip,
    enc2c(zipfile),
    enc2c(data$key),
    enc2c(data$file),
    data$dir,
    fi$mtime,
    as.integer(compression_level),
    append,
    total_bytes,
    pw,
    enc
  )

  invisible(zipfile)
}

#' List Files in a 'zip' Archive
#'
#' @details Note that `crc32` is formatted using `as.hexmode()`. `offset` refers
#'   to the start of the local zip header for each entry. Following the approach
#'   of `seek()` it is stored as a `numeric` rather than an `integer` vector and
#'   can therefore represent values up to `2^53-1` (9 PB).
#' @param zipfile Path to an existing ZIP file.
#' @param encoding Encoding to use for entry filenames. ZIP files signal
#'   UTF-8 filenames via a flag in each entry; those are always decoded as
#'   UTF-8 regardless of `encoding`. For entries without that flag, `encoding`
#'   is used; `NULL` (the default) falls back to IBM CP437, which is what the
#'   ZIP specification prescribes for legacy entries. The value is passed
#'   to [iconv()].
#' @return A data frame with columns: `filename`, `compressed_size`,
#'   `uncompressed_size`, `timestamp`, `permissions`, `crc32`, `offset`,
#'   `type` and `encryption`. `type` is one of `file`, `block_device`,
#'   `character_device`, `directory`, `FIFO`, `symlink` or `socket`.
#'   `encryption` is one of `none`, `aes128`, `aes192`, `aes256`,
#'   `zipcrypto`, or `NA` if encrypted but the scheme cannot be determined.
#'
#' @family zip/unzip functions
#' @export

zip_list <- function(zipfile, encoding = NULL) {
  if (startsWith(zipfile, "http://") || startsWith(zipfile, "https://")) {
    return(zip_list_url(zipfile, encoding))
  }
  zipfile <- enc2c(normalizePath(zipfile))
  res <- .Call(c_R_zip_list, zipfile, encoding)
  if (Sys.getenv("PKGCACHE_NO_PILLAR") == "") {
    requireNamespace("pillar", quietly = TRUE)
  }
  df <- data_frame(
    filename = res[[1]],
    compressed_size = res[[2]],
    uncompressed_size = res[[3]],
    timestamp = as.POSIXct(res[[4]], tz = "UTC", origin = "1970-01-01")
  )
  Encoding(df$filename) <- "UTF-8"
  df$permissions <- as.octmode(res[[5]])
  df$crc32 <- as.hexmode(res[[6]])
  df$offset <- res[[7]]
  # names are the same as in `fs::file_info()`
  df$type <- file_types[res[[8]] + 1L]
  df$encryption <- encryption_types[res[[9]] + 2L]
  df
}

file_types <- c(
  "file",
  "block_device",
  "character_device",
  "directory",
  "FIFO",
  "symlink",
  "socket"
)

# C returns: -1=unknown, 0=none, 1=aes128, 2=aes192, 3=aes256, 4=zipcrypto
# + 2L shifts to 1-based index
encryption_types <- c(
  NA_character_,
  "none",
  "aes128",
  "aes192",
  "aes256",
  "zipcrypto"
)

#' Uncompress 'zip' Archives
#'
#' `unzip()` always restores modification times of the extracted files and
#' directories.
#'
#' @section Permissions:
#'
#' If the zip archive stores permissions and was created on Unix,
#' the permissions will be restored.
#'
#' @param zipfile Path to the zip file to uncompress, or a character vector of
#'   paths. When multiple paths are given and all other arguments are at their
#'   defaults, the files are unzipped concurrently in a thread pool.
#'   Set the `zip_threads` option or the `ZIP_THREADS` environment variable
#'   to control the number of threads used. If neither is set, the `Ncpus`
#'   option is used, if set. By default zip uses two threads.
#' @param files Character vector of files to extract from the archive.
#'   Files within directories can be specified, but they must use a forward
#'   slash as path separator, as this is what zip files use internally.
#'   If `NULL`, all files will be extracted.
#' @param overwrite Whether to overwrite existing files. If `FALSE` and
#'   a file already exists, then an error is thrown.
#' @param junkpaths Whether to ignore all directory paths when creating
#'   files. If `TRUE`, all files will be created in `exdir`.
#' @param exdir Directory to uncompress the archive to. If it does not
#'   exist, it will be created.
#' @inheritParams zip_list
#' @param password Password for decrypting encrypted entries. It can be a
#'   string, a raw vector, or a function that returns one of these. If `NULL`
#'   (the default), the `zip_password` option is used, or no password if that
#'   is also `NULL`. The password is silently ignored for entries that are not
#'   encrypted.
#' @return A data frame with one row per extracted entry and columns,
#'   invisibly: `filename` (path within the archive), `compressed_size`,
#'   `uncompressed_size`, `timestamp`, `permissions`, `crc32`, `offset`,
#'   `type` (same as in [zip_list()]), and `path` (absolute path to the
#'   extracted file on disk).
#'
#' @family zip/unzip functions
#' @export
#' @examples
#' ## temporary directory, to avoid messing up the user's workspace.
#' dir.create(tmp <- tempfile())
#' dir.create(file.path(tmp, "mydir"))
#' cat("first file", file = file.path(tmp, "mydir", "file1"))
#' cat("second file", file = file.path(tmp, "mydir", "file2"))
#'
#' zipfile <- tempfile(fileext = ".zip")
#' zip::zip(zipfile, "mydir", root = tmp)
#'
#' ## List contents
#' zip_list(zipfile)
#'
#' ## Extract and inspect result
#' tmp2 <- tempfile()
#' result <- unzip(zipfile, exdir = tmp2)
#' result[, c("filename", "path")]

unzip <- function(
  zipfile,
  files = NULL,
  overwrite = TRUE,
  junkpaths = FALSE,
  exdir = ".",
  encoding = NULL,
  password = NULL
) {
  if (
    length(zipfile) == 1 &&
      (startsWith(zipfile, "http://") || startsWith(zipfile, "https://"))
  ) {
    return(unzip_url(zipfile, files, overwrite, junkpaths, exdir, encoding))
  }
  stopifnot(
    is_character(zipfile),
    length(zipfile) >= 1,
    is_character_or_null(files),
    is_flag(overwrite),
    is_flag(junkpaths),
    is_string(exdir)
  )

  if (
    length(zipfile) > 1 &&
      is.null(files) &&
      isTRUE(overwrite) &&
      !isTRUE(junkpaths) &&
      is.null(encoding) &&
      !any(startsWith(zipfile, "http://") | startsWith(zipfile, "https://"))
  ) {
    pw <- resolve_password(password)
    passwords <- if (!is.null(pw)) rawToChar(pw) else NULL
    return(threaded_unzip(zipfile, exdirs = exdir, passwords = passwords))
  }

  if (length(zipfile) > 1) {
    results <- lapply(zipfile, function(zf) {
      unzip(
        zf,
        files = files,
        overwrite = overwrite,
        junkpaths = junkpaths,
        exdir = exdir,
        encoding = encoding,
        password = password
      )
    })
    return(invisible(do.call(rbind, results)))
  }

  zipfile <- enc2c(normalizePath(zipfile))
  if (!is.null(files)) {
    files <- enc2c(files)
  }
  exdir <- sub("/+$", "", exdir)
  mkdirp(exdir)
  exdir <- enc2c(normalizePath(exdir))

  pw <- resolve_password(password)

  res <- call_with_cleanup(
    c_R_zip_unzip,
    zipfile,
    files,
    overwrite,
    junkpaths,
    exdir,
    encoding,
    is_progress_enabled(),
    pw
  )

  if (Sys.getenv("PKGCACHE_NO_PILLAR") == "") {
    requireNamespace("pillar", quietly = TRUE)
  }
  df <- data_frame(
    filename = res[[1]],
    compressed_size = res[[2]],
    uncompressed_size = res[[3]],
    timestamp = as.POSIXct(res[[4]], tz = "UTC", origin = "1970-01-01")
  )
  Encoding(df$filename) <- "UTF-8"
  df$permissions <- as.octmode(res[[5]])
  df$crc32 <- as.hexmode(res[[6]])
  df$offset <- res[[7]]
  df$type <- file_types[res[[8]] + 1L]
  df$path <- res[[9]]
  Encoding(df$path) <- "UTF-8"

  invisible(df)
}
