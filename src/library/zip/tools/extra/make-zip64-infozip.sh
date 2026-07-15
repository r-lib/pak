#!/bin/sh
# Regenerate tests/testthat/fixtures/zip64.zip
#
# A real ZIP64 archive produced by Info-ZIP's `zip -fz` (force ZIP64). This
# encoding sets the uncompressed-size field to the 0xFFFFFFFF sentinel for
# *every* entry and stores the real value in a ZIP64 extended information extra
# field -- including zero-length STORED directory entries (comp_size=0). That
# combination used to make the bundled miniz reader reject the directory
# entries with "invalid header or archive is corrupted" (see the comp_size=0
# handling in mz_zip_file_stat_internal, fixed to run after the ZIP64 fields
# are resolved). Used as a regression fixture for the local (miniz) reader.
#
# Requires Info-ZIP `zip`. Run from the repo root:
#   sh tools/extra/make-zip64-infozip.sh
set -e
dest="tests/testthat/fixtures/zip64.zip"

work=$(mktemp -d)
mkdir -p "$work/src/dir"
printf 'file1\n'  > "$work/src/file1"
printf 'file11\n' > "$work/src/file11"
printf 'file2\n'  > "$work/src/dir/file2"
printf 'file3\n'  > "$work/src/dir/file3"
( cd "$work" && zip -fz -r -X zip64.zip src >/dev/null )
cp "$work/zip64.zip" "$dest"
rm -rf "$work"
echo "wrote $dest"
