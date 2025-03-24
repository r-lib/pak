
#' @useDynLib zip, .registration = TRUE, .fixes = "c_"
NULL

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
#' different paths and put all of the in the archive, at the top
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
#' These function exist for historical reasons. They are identical
#' to `zip()` and `zipr_append()` with a different default for the
#' `mode` argument.
#'
#' @param zipfile The zip file to create. If the file exists, `zip`
#'   overwrites it, but `zip_append` appends to it. If it is a directory
#'   an error is thrown.
#' @param files List of file to add to the archive. See details below
#'    about absolute and relative path names.
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

zip <- function(zipfile, files, recurse = TRUE, compression_level = 9,
                include_directories = TRUE, root = ".",
                mode = c("mirror", "cherry-pick")) {
  mode <- match.arg(mode)
  zip_internal(zipfile, files, recurse, compression_level, append = FALSE,
               root = root, keep_path = (mode == "mirror"),
               include_directories = include_directories)
}

#' @rdname zip
#' @export

zipr <- function(zipfile, files, recurse = TRUE, compression_level = 9,
                 include_directories = TRUE, root = ".",
                 mode = c("cherry-pick", "mirror")) {
  mode <- match.arg(mode)
  zip_internal(zipfile, files, recurse, compression_level, append = FALSE,
               root = root, keep_path = (mode == "mirror"),
               include_directories = include_directories)
}

#' @rdname zip
#' @export

zip_append <- function(zipfile, files, recurse = TRUE,
                       compression_level = 9, include_directories = TRUE,
                       root = ".", mode = c("mirror", "cherry-pick")) {
  mode <- match.arg(mode)
  zip_internal(zipfile, files, recurse, compression_level, append = TRUE,
               root = root, keep_path = (mode == "mirror"),
               include_directories = include_directories)
}

#' @rdname zip
#' @export

zipr_append <- function(zipfile, files, recurse = TRUE,
                        compression_level = 9, include_directories = TRUE,
                        root = ".", mode = c("cherry-pick", "mirror")) {
  mode <- match.arg(mode)
  zip_internal(zipfile, files, recurse, compression_level, append = TRUE,
               root = root, keep_path = (mode == "mirror"),
               include_directories = include_directories)
}

zip_internal <- function(zipfile, files, recurse, compression_level,
                         append, root, keep_path, include_directories) {
  zipfile <- path.expand(zipfile)
  if (dir.exists(zipfile)) {
    stop("zip file at `", zipfile, "` already exists and it is a directory")
  }
  oldwd <- setwd(root)
  on.exit(setwd(oldwd), add = TRUE)

  if (any(! file.exists(files))) stop("Some files do not exist")

  data <- get_zip_data(files, recurse, keep_path, include_directories)
  data$key <- fix_absolute_paths(data$key)
  warn_for_colon(data$key)
  warn_for_dotdot(data$key)

  .Call(c_R_zip_zip, enc2c(zipfile), enc2c(data$key),
        enc2c(data$file), data$dir, file.info(data$file)$mtime,
        as.integer(compression_level), append)

  invisible(zipfile)
}

#' List Files in a 'zip' Archive
#'
#' @details Note that `crc32` is formatted using `as.hexmode()`. `offset` refers
#'   to the start of the local zip header for each entry. Following the approach
#'   of `seek()` it is stored as a `numeric` rather than an `integer` vector and
#'   can therefore represent values up to `2^53-1` (9 PB).
#' @param zipfile Path to an existing ZIP file.
#' @return A data frame with columns: `filename`, `compressed_size`,
#'   `uncompressed_size`, `timestamp`, `permissions`, `crc32` and `offset`.
#'
#' @family zip/unzip functions
#' @export

zip_list <- function(zipfile) {
  zipfile <- enc2c(normalizePath(zipfile))
  res <- .Call(c_R_zip_list, zipfile)
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
  df
}

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
#' @param zipfile Path to the zip file to uncompress.
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
#'
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
#' ## Extract
#' tmp2 <- tempfile()
#' unzip(zipfile, exdir = tmp2)
#' dir(tmp2, recursive = TRUE)

unzip <- function(zipfile, files = NULL, overwrite = TRUE,
                      junkpaths = FALSE, exdir = ".") {

  stopifnot(
    is_string(zipfile),
    is_character_or_null(files),
    is_flag(overwrite),
    is_flag(junkpaths),
    is_string(exdir))

  zipfile <- enc2c(normalizePath(zipfile))
  if (!is.null(files)) files <- enc2c(files)
  exdir <- sub("/+$", "", exdir)
  mkdirp(exdir)
  exdir <- enc2c(normalizePath(exdir))

  .Call(c_R_zip_unzip, zipfile, files, overwrite, junkpaths, exdir)

  invisible()
}
