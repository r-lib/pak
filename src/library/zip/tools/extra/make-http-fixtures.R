#!/usr/bin/env Rscript
# Regenerate the HTTP range-request test fixtures in this directory.
#
# The HTTP tests serve a ZIP over a fake server and snapshot zip_list()'s
# output, including the `permissions` column. Building the archive during the
# test makes that column platform-dependent: zipr() only stamps Unix mode bits
# (755/644) on Unix; on Windows the entries carry no Unix permissions and
# zip_list() reports a fixed 700/600 instead. Baking the archive once, here on
# Unix, freezes the permission bits into the file so the snapshot matches on
# every platform.
#
#   http.zip         - root "ziptest/" with directory entries, mirroring
#                      make_a_zip(include_directories = TRUE).
#   http-nodirs.zip  - the same files without directory entries, mirroring
#                      make_a_zip(include_directories = FALSE). Used by the
#                      junkpaths test and as the base for the ZIP64 EOCD tests.
#
# Run from anywhere; writes alongside this script.
stopifnot(.Platform$OS.type == "unix")
here <- dirname(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(), value = TRUE)
))
if (length(here) == 0) {
  here <- "."
}

work <- tempfile("http-fixture-")
root <- file.path(work, "ziptest")
dir.create(file.path(root, "dir"), recursive = TRUE)
cat("file1\n", file = file.path(root, "file1"))
cat("file11\n", file = file.path(root, "file11"))
cat("file2\n", file = file.path(root, "dir", "file2"))
cat("file3\n", file = file.path(root, "dir", "file3"))

zip::zipr(file.path(here, "http.zip"), root, include_directories = TRUE)
zip::zipr(file.path(here, "http-nodirs.zip"), root, include_directories = FALSE)

unlink(work, recursive = TRUE)
