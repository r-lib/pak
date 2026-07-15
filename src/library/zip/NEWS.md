# zip (development version)

* `unzip()` no longer errors on Unix when extracting a symlink whose target
  is missing or is extracted later in the archive (#157).

# zip 3.0.1

* The progress bar option is now named `zip_progress`, to match the other
  options. The old `zip.progress` name still works as a fallback (#150).

* Boolean environment variables (`R_ZIP_PROCESS_FALLBACK` and
  `ZIP_PROGRESS` currently) are now matched case-insensitively, so `TRUE`
  and `FALSE` are accepted as documented (#154).

* The number of threads used by `unzip()` now respects the `Ncpus` option,
  if neither the `zip_threads` option nor the `ZIP_THREADS` environment
  variable is set (#151).

* Archives created by `zip()` can now always be extracted with the Windows
  Explorer "Extract All" tool, including archives that contain many small
  files (#153).

# zip 3.0.0

* `zip()`, `zipr()`, `zip_append()`, `zipr_append()`, `zip_process()`, and
  `unzip()` / `unzip_process()` now support password-protected archives
  using WinZIP AES-256, and other encryption schemes (#38).

* `zip_list()` now reports an `encryption` column indicating the encryption
  scheme used for each entry (#38).

* `unzip()` is now vectorized. If all arguments apart from `zipfile`,
  `exdir` and `password` are the default, then it unprocesses all files
  concurrently, using a thread pool. The size of the thread pool can be
  set with the `zip_threads` option or the `ZIP_THREADS` environment
  variable (#147).

* `zip_list()` and `unzip()` now work directly on `http://` and `https://`
  URLs. They use HTTP range requests to download only the central directory
  and the requested entries, so listing or extracting a few files from a
  large remote archive does not downloads the whole file. If the server
  does not support range requests, they fall back to downloading the entire
  archive (with a warning). This requires the curl package (#39).

* `zip_list()` and `unzip()` now report the Unix permission bits stored in an
  archive on Windows as well. Previously they always reported `700`/`600` on
  Windows, regardless of the permissions recorded in the ZIP file.

* `zip_list()` and `unzip()` now report `type == "directory"` for directory
  entries whose Unix mode bits lack `S_IFDIR` but that are marked as
  directories by a trailing slash or the DOS directory attribute (e.g.
  archives created by `zip()` itself). Previously these were reported as
  `"file"`.

* `zip()` and `unzip()` now show a progress bar when the `cli` package is
  installed. For `zip()`, progress is byte-level, so large single files are
  tracked smoothly. For `unzip()`, progress advances once per extracted
  entry. Progress bars are (for now) opt-in via the `ZIP_PROGRESS=true`
  environment variable or the `zip.progress` option (#48).

* `unzip()` now returns a data frame (invisibly) with one row per extracted
  entry, containing the same columns as `zip_list()` (`filename`,
  `compressed_size`, `uncompressed_size`, `timestamp`, `permissions`,
  `crc32`, `offset`, `type`) plus a `path` column with the absolute path
  to each extracted file on disk (#35).

* `zip_list()` and `unzip()` now have an `encoding` argument for ZIP files
  with non-UTF-8, non-CP437 filenames (e.g. CP932/Shift-JIS on Japanese
  Windows). When `encoding` is set, filenames without the UTF-8 flag are
  decoded from the specified code page instead of CP437 (#101).

* `zip_append()` and `zipr_append()` now replace existing entries when
  appending a file whose archive path already exists in the zip file,
  instead of creating duplicate entries (#111).

* `unzip()` and `zip_list()` now correctly handle ZIP files with
  non-UTF-8 filenames (e.g. filenames encoded in IBM CP437, as created
  by many Windows tools). The filenames are converted to UTF-8 using the
  CP437 character map when the UTF-8 flag is not set in the ZIP entry
  (#103).

* New `keys` argument to `zip()`, `zipr()`, `zip_append()`, and
  `zipr_append()`. It allows specifying custom paths for entries inside
  the archive, independently of their paths on disk (#50).

* `unzip_process()` now probes falls back to using an R subprocess when
  the bundled `cmdunzip` executable cannot be started (#135).

* Updated embedded miniz to version 3.1.1 (#122).

# zip 2.3.3

* `zip_list()` now has a `type` column, for the file type.

* `unzip()` now correctly creates symbolic links on Unix (#127).

# zip 2.3.2

* `zip_list()` now returns a `tbl` object, and loads the pillar package,
  if installed, to produce the nicer output for long data frames.

# zip 2.3.1

* The zip shared library now hides its symbols (on platforms that support
  this), to avoid name clashes with other libraries (#98).

# zip 2.3.0

* zip now handles large zip files on Windows (#65, #75, #79, @weshinsley).

* zip now behaves better for absolute paths in mirror mode, and when the
  paths contain a `:` character (#69, #70).

* `zip::unzip()` now uses the process's umask value (see `umask(2)`) on Unix
  if the zip file does not contain Unix permissions (#67).

* Fix segmentation fault when zip file can't be created (#91, @zeehio)

* Fix delayed evaluation error on zipfile when `zip::zip()`
  is used (#92, @zeehio)

* New `deflate()` and `inflate()` functions to compress and uncompress
  GZIP streams in memory.

# zip 2.2.2

* No user visible changes.

# zip 2.2.1

* No user visible changes.

# 2.2.0

* Header values (of version made by and external attributes) are now
  correctly read and written on big-endian systems (#68).

* `zip_list()` now also returns `crc32` and `offset` (#74, @jefferis).

# 2.1.1

This version has no user visible changes.

# 2.1.0

* `unzip_process()` now does not fail randomly on Windows (#60).

* Now all functions handle Unicode paths correctly, on Windows
  as well (#42, #53).

* `unzip_process()` now works when R library is on different drive
  than `exdir` on Windows (#45)

* zip functions now have a `mode` argument to choose how files and
  directories are assembled into the archive. See the docs for
  details.

* zip functions now have a `root` argument, zip changes the working
  directory to this before creating the archive, so all files are
  relative to `root`.

* `zip()` and `zip_append()` are not deprecated any more, as it was
  hard to achieve the same functionality with the other zip functions.

# 2.0.4

* `unzip_process()` prints better error messages to the standard error,
  and exits with a non-zero status, on error.

# 2.0.3

* `zipr()` and `zipr_append()` get an `include_directories = TRUE`
  argument, that can be used to omit directory entries from the zip
  archive. These entries may cause problems in MS Office docx files (#34).

# 2.0.2

* `zip_process()` and `unzip_process()` can now pass extra arguments to
  `processx::process` (#32).

* `unzip_process()` now makes sure the `exdir` path is created with
  forward slashes on Windows, mixing forward and backward slashes can
  cause errors.

# 2.0.1

* `zip()` and `zip_append()` are now soft-deprecated, please use
  `zipr()` and `zipr_append()` instead.

# 2.0.0

* New `zipr()` and `zipr_append()`, they always store relative file names
  in the archive.

* New `unzip()` function for uncompressing zip archives.

* New `zip_process()` and `unzip_process()` functions to create or
  uncompress an archive in a background process.

* `zip()`, `zipr()`, `zip_append()` and `zipr_append()` all include
  directories in the archives, empty ones as well.

* `zip()`, `zipr()`, `zip_append()` and `zipr_append()` all add time stamps
  to the archive and `zip_list()` returns then in the `timestamp` column.

* `zip()`, `zipr()`, `zip_append()` and `zipr_append()` all add file
  and directory permissions to the archive on Unix systems, and
  `zip_list()` returns them in the `permissions` column.

* `zip_list()` now correctly reports the size of large files in the archive.

* Use miniz 2.0.8 internally.

# 1.0.0

First public release.
