
#' List all mounted partitions
#'
#' The output is similar the Unix `mount` and `df` commands.
#'
#' @param all Whether to list virtual devices as well. If `FALSE`, on
#' Linux it will still list `overlay` and `grpcfuse` file systems, to
#' provide some useful information in Docker containers.
#' @return A data frame with columns `device`, `mountpoint`,
#' `fstype` and `options`.
#'
#' @family disk functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_disk_partitions(all = TRUE)
#' ps_disk_partitions()

ps_disk_partitions <- function(all = FALSE) {
  assert_flag(all)
  l <- not_null(.Call(ps__disk_partitions, all))

  d <- data_frame(
    device = vapply(l, "[[", character(1), 1),
    mountpoint = vapply(l, "[[", character(1), 2),
    fstype = vapply(l, "[[", character(1), 3),
    options = vapply(l, "[[", character(1), 4)
  )

  if (!all) d <- ps__disk_partitions_filter(d)

  d
}

#' @importFrom utils read.delim

ps__disk_partitions_filter <- function(pt) {
  os <- ps_os_name()

  if (os == "LINUX") {
    fs <- read.delim("/proc/filesystems", header = FALSE, sep = "\t")
    goodfs <- c(fs[[2]][fs[[1]] != "nodev"], "zfs")
    ok <- pt$device != "none" & file.exists(pt$device) & pt$fstype %in% goodfs
    ok <- ok | pt$device %in% c("overlay", "grpcfuse")
    pt <- pt[ok, , drop = FALSE]

  } else if (os == "MACOS") {
    ok <- substr(pt$device, 1, 1) == "/" & file.exists(pt$device)
    pt <- pt[ok, , drop = FALSE]
  }

  pt
}

#' Disk usage statistics, per partition
#'
#' The output is similar to the Unix `df` command.
#'
#'
#' Note that on Unix a small percentage of the disk space (5% typically)
#' is reserved for the superuser. `ps_disk_usage()` returns the space
#' available to the calling user.
#'
#' @param paths The mounted file systems to list. By default all file
#' systems returned by [ps_disk_partitions()] is listed.
#' @return A data frame with columns `mountpoint`, `total`, `used`,
#' `available` and `capacity`.
#'
#' @family disk functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_disk_usage()


ps_disk_usage <- function(paths = ps_disk_partitions()$mountpoint) {
  assert_character(paths)
  l <- .Call(ps__disk_usage, paths)
  os <- ps_os_name()
  if (os == "WINDOWS") {
    ps__disk_usage_format_windows(paths, l)
  } else {
    ps__disk_usage_format_posix(paths, l)
  }
}

ps__disk_usage_format_windows <- function(paths, l) {
  total <- vapply(l, "[[", double(1), 1)
  free <- vapply(l, "[[", double(1), 2)
  freeuser <- vapply(l, "[[", double(1), 3)
  used <- total - free

  d <- data_frame(
    mountpoint = paths,
    total = total,
    used = used,
    available = freeuser,
    capacity = used / total
  )

  d
}

ps__disk_usage_format_posix <- function(paths, l) {
  l2 <- lapply(l, function(fs) {
    total <- fs[[5]] * fs[[1]]
    avail_to_root <- fs[[6]] * fs[[1]]
    avail = fs[[7]] * fs[[1]]
    used  <-  total - avail_to_root
    total_user <- used + avail
    usage_percent <- used / total_user
    list(total = total, used = used, free = avail, percent = usage_percent)
  })

  d <- data_frame(
    mountpoint = paths,
    total = vapply(l2, "[[", double(1), "total"),
    used = vapply(l2, "[[", double(1), "used"),
    available = vapply(l2, "[[", double(1), "free"),
    capacity = vapply(l2, "[[", double(1), "percent")
  )

  d
}

#' System-wide disk I/O counters
#'
#' Returns a data.frame of system-wide disk I/O counters.
#'
#' Includes the following non-NA fields for all supported platforms:
#' * `read_count`: number of reads
#' * `write_count`: number of writes
#' * `read_bytes`: number of bytes read
#' * `write_bytes`: number of bytes written
#'
#' And for only some platforms:
#' * `read_time`: time spent reading from disk (in milliseconds)
#' * `write_time`: time spent writing to disk (in milliseconds)
#' * `busy_time`: time spent doing actual I/Os (in milliseconds)
#' * `read_merged_count`: number of merged reads (see iostats doc)
#' * `write_merged_count`: number of merged writes (see iostats doc)
#'
#' @return A data frame of one row per disk of I/O stats, with columns
#' `name`, `read_count` `read_merged_count` `read_bytes`, `read_time`,
#' `write_count`, `write_merged_count`, `write_bytes` `write_time`, and
#' `busy_time`.
#'
#' @family disk functions
#' @export
#' @examplesIf ps::ps_is_supported() && ps:::ps_os_name() %in% c("LINUX", "WINDOWS") && !ps:::is_cran_check()
#' ps_disk_io_counters()
ps_disk_io_counters <- function() {
  os <- ps_os_name()
  tab <- if (os == "LINUX") {
    ps__disk_io_counters_linux()
  } else if (os == "WINDOWS") {
    ps__disk_io_counters_windows()
  } else {
    ps__disk_io_counters_macos()
  }

  class(tab) <- c("tbl", "data.frame")
  tab
}

ps__disk_io_counters_windows <- function() {
  l <- .Call(ps__disk_io_counters)
  disk_info <- data_frame(
    name = l[[1]],
    read_count = l[[2]],
    read_merged_count = NA,
    read_bytes = l[[3]],
    read_time = l[[4]],
    write_count = l[[5]],
    write_merged_count = NA,
    write_bytes = l[[6]],
    write_time = l[[7]],
    busy_time = NA
  )

  disk_info[disk_info$name != "",]
}

ps__disk_io_counters_macos <- function() {
  tab <- not_null(.Call(ps__disk_io_counters))
  data_frame(
    name = names(tab),
    read_count = map_dbl(tab, "[[", 1),
    read_merged_count = NA,
    read_bytes = map_dbl(tab, "[[", 3),
    read_time = map_dbl(tab, "[[", 5),
    write_count = map_dbl(tab, "[[", 2),
    write_merged_count = NA,
    write_bytes = map_dbl(tab, "[[", 4),
    write_time = map_dbl(tab, "[[", 6),
    busy_time = NA
  )
}

#' File system information for files
#'
#' @param paths A path or a vector of paths. `ps_fs_info()` returns
#'   information about the file systems of all paths. `path` may contain
#'   direcories as well.
#' @return Data frame with file system information for each
#'   path in `paths`, one row per path. Common columns for all
#'   operating systems:
#'   * `path`: The input paths, i.e. the `paths` argument.
#'   * `mountpoint`: Directory where the file system is mounted.
#'     On Linux there is a small chance that it was not possible to
#'     look this up, and it is `NA_character_`. This is the drive letter
#'     or the mount directory on Windows, with a trailing `\`.
#'   * `name`: Device name.
#'     On Linux there is a small chance that it was not possible to
#'     look this up, and it is `NA_character_`. On Windows this is the
#'     volume GUID path of the form `\\?\Volume{GUID}\`.
#'   * `type`: File system type (character).
#'     On Linux there is a tiny chance that it was not possible to
#'     look this up, and it is `NA_character_`.
#'   * `block_size`: File system block size. This is the sector size on
#'     Windows, in bytes.
#'   * `transfer_block_size`: Pptimal transfer block size. On Linux it is
#'     currently always the same as `block_size`. This is the cluster size
#'     on Windows, in bytes.
#'   * `total_data_blocks`: Total data blocks in file system. On Windows
#'     this is the number of sectors.
#'   * `free_blocks`: Free blocks in file system. On Windows this is the
#'     number of free sectors.
#'   * `free_blocks_non_superuser`: Free blocks for a non-superuser, which
#'     might be different on Unix. On Windows this is the number of free
#'     sectors for the calling user.
#'   * `id`: File system id. This is a raw vector. On Linux it is
#'     often all zeros. It is always `NULL` on Windows.
#'   * `owner`: User that mounted the file system. On Linux and Windows
#'     this is currently always `NA_real_`.
#'   * `type_code`: Type of file system, a numeric code. On Windows this
#'     this is `NA_real_`.
#'   * `subtype_code`: File system subtype (flavor). On Linux and Windows
#'     this is always `NA_real_`.
#'
#'   The rest of the columns are flags, and they are operating system
#'   dependent.
#'
#'   macOS:
#'
#'   * `RDONLY`: A read-only filesystem.
#'   * `SYNCHRONOUS`: File system is written to synchronously.
#'   * `NOEXEC`: Can't exec from filesystem.
#'   * `NOSUID`: Setuid bits are not honored on this filesystem.
#'   * `NODEV`: Don't interpret special files.
#'   * `UNION`: Union with underlying filesysten.
#'   * `ASYNC`: File system written to asynchronously.
#'   * `EXPORTED`: File system is exported.
#'   * `LOCAL`: File system is stored locally.
#'   * `QUOTA`: Quotas are enabled on this file system.
#'   * `ROOTFS`: This file system is the root of the file system.
#'   * `DOVOLFS`: File system supports volfs.
#'   * `DONTBROWSE`: File system is not appropriate path to user data.
#'   * `UNKNOWNPERMISSIONS`:  VFS will ignore ownership information on
#'     filesystem filesystemtem objects.
#'   * `AUTOMOUNTED`: File system was mounted by automounter.
#'   * `JOURNALED`: File system is journaled.
#'   * `DEFWRITE`: File system should defer writes.
#'   * `MULTILABEL`: MAC support for individual labels.
#'   * `CPROTECT`: File system supports per-file encrypted data protection.
#'
#'   Linux:
#'
#'   * `MANDLOCK`: Mandatory locking is permitted on the filesystem
#'     (see `fcntl(2)`).
#'   * `NOATIME`: Do not update access times; see `mount(2)`.
#'   * `NODEV`: Disallow access to device special files on this filesystem.
#'   * `NODIRATIME`: Do not update directory access times; see mount(2).
#'   * `NOEXEC`: Execution of programs is disallowed on this filesystem.
#'   * `NOSUID`: The set-user-ID and set-group-ID bits are ignored by
#'      `exec(3)` for executable files on this filesystem
#'   * `RDONLY`: This filesystem is mounted read-only.
#'   * `RELATIME`: Update atime relative to mtime/ctime; see `mount(2)`.
#'   * `SYNCHRONOUS`: Writes are synched to the filesystem immediately
#'     (see the description of `O_SYNC` in `open(2)``).
#'   * `NOSYMFOLLOW`: Symbolic links are not followed when resolving paths;
#'     see `mount(2)``.
#'
#'   Windows:
#'
#'   * `CASE_SENSITIVE_SEARCH`: Supports case-sensitive file names.
#'   * `CASE_PRESERVED_NAMES`: Supports preserved case of file names when
#'      it places a name on disk.
#'   * `UNICODE_ON_DISK`: Supports Unicode in file names as they appear on
#'      disk.
#'   * `PERSISTENT_ACLS`: Preserves and enforces access control lists
#'      (ACL). For example, the NTFS file system preserves and enforces
#'      ACLs, and the FAT file system does not.
#'   * `FILE_COMPRESSION`: Supports file-based compression.
#'   * `VOLUME_QUOTAS`: Supports disk quotas.
#'   * `SUPPORTS_SPARSE_FILES`: Supports sparse files.
#'   * `SUPPORTS_REPARSE_POINTS`: Supports reparse points.
#'   * `SUPPORTS_REMOTE_STORAGE`: Supports remote storage.
#'   * `RETURNS_CLEANUP_RESULT_INFO`: On a successful cleanup operation,
#'      the file system returns information that describes additional
#'      actions taken during cleanup, such as deleting the file. File
#'      system filters can examine this information in their post-cleanup
#'      callback.
#'   * `SUPPORTS_POSIX_UNLINK_RENAME`: Supports POSIX-style delete and
#'      rename operations.
#'   * `VOLUME_IS_COMPRESSED`: It is a compressed volume, for example, a
#'      DoubleSpace volume.
#'   * `SUPPORTS_OBJECT_IDS`: Supports object identifiers.
#'   * `SUPPORTS_ENCRYPTION`: Supports the Encrypted File System (EFS).
#'   * `NAMED_STREAMS`: Supports named streams.
#'   * `READ_ONLY_VOLUME`: It is read-only.
#'   * `SEQUENTIAL_WRITE_ONCE`: Supports a single sequential write.
#'   * `SUPPORTS_TRANSACTIONS`: Supports transactions.
#'   * `SUPPORTS_HARD_LINKS`: The volume supports hard links.
#'   * `SUPPORTS_EXTENDED_ATTRIBUTES`: Supports extended attributes.
#'   * `SUPPORTS_OPEN_BY_FILE_ID`: Supports open by FileID.
#'   * `SUPPORTS_USN_JOURNAL`: Supports update sequence number (USN)
#'      journals.
#'   * `SUPPORTS_INTEGRITY_STREAMS`: Supports integrity streams.
#'   * `SUPPORTS_BLOCK_REFCOUNTING`: The volume supports sharing logical
#'      clusters between files on the same volume.
#'   * `SUPPORTS_SPARSE_VDL`: The file system tracks whether each cluster
#'      of a file contains valid data (either from explicit file writes or
#'      automatic zeros) or invalid data (has not yet been written to or
#'      zeroed).
#'   * `DAX_VOLUME`: The volume is a direct access (DAX) volume.
#'   * `SUPPORTS_GHOSTING`: Supports ghosting.
#'
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_fs_info(c("/", "~", "."))

ps_fs_info <- function(paths = "/") {
  assert_character(paths)
  abspaths <- normalizePath(paths, mustWork = TRUE)
  mps <- ps_fs_mount_point(paths)
  res <- .Call(ps__fs_info, paths, abspaths, mps)
  df <- as_data_frame(res)

  # this should not happen in practice, but just in case
  if (ps_os_type()[["LINUX"]] && any(is.na(df$type))) {
    miss <- which(is.na(df$type))
    df$type[miss] <- linux_fs_types$name[match(df$type_code[miss], linux_fs_types$id)]
  }

  df
}

linux_fs_types <- utils::read.table(
  "tools/linux-fs-types.txt",
  header = TRUE,
  stringsAsFactors = FALSE
)

posix_stat_types <- c(
  "regular file",
  "directory",
  "character device",
  "block device",
  "FIFO",
  "symbolic link",
  "socket"
)

#' File status
#'
#' This function is currently not implemented on Windows.
#'
#' @param paths Paths to files, directories, devices, etc. They must
#'   exist. They are expanded using [base::path.expand()].
#' @param follow Whether to follow symbolic links. If `FALSE` it returns
#'   information on the links themselves.
#' @return Data frame with one row for each path in `paths`. Columns:
#'   * `path`: Expanded `paths`.
#'   * `dev_major`: Major device ID of the device the path resides on.
#'   * `dev_minor`: Minor device ID of the device the path resodes on.
#'   * `inode`: Inode number.
#'   * `mode`: File type and mode (permissions). It is easier to use the
#'     `type` and `permissions` columns.
#'   * `type`: File type, character. One of
#'     `r paste(posix_stat_types, collapse = ", ")`.
#'   * `permissions`: Permissions, numeric code in an integer column.
#'   * `nlink`: Number of hard links.
#'   * `uid`: User id of owner.
#'   * `gid`: Group id of owner.
#'   * `rdev_major`: If the path is a device, its major device id,
#'     otherwise `NA_integer_`.
#'   * `rdev_minor`: IF the path is a device, its minor device id,
#'     otherwise `NA_integer_`.
#'   * `size`: File size in bytes.
#'   * `block_size`: Block size for filesystem I/O.
#'   * `blocks`: Number of 512B blocks allocated.
#'   * `access_time`: Time of last access.
#'   * `modification_time`: Time of last modification.
#'   * `change_time`: Time of last status change.
#'
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check() && ps_os_type()[["POSIX"]]
#' ps_fs_stat(c(".", tempdir()))

ps_fs_stat <- function(paths, follow = TRUE) {
  assert_character(paths)
  paths <- path.expand(paths)
  res <- .Call(ps__stat, paths, follow)
  res[["type"]] <- posix_stat_types[res[["type"]]]
  res[["access_time"]] <- .POSIXct(res[["access_time"]], "UTC")
  res[["modification_time"]] <- .POSIXct(res[["modification_time"]], "UTC")
  res[["change_time"]] <- .POSIXct(res[["change_time"]], "UTC")
  as_data_frame(res)
}

#' Find the mount point of a file or directory
#'
#' @param paths Paths to files, directories, devices, etc. They must
#'   exist. They are normalized using [base::normalizePath()].
#' @return Character vector, paths to the mount points of the input
#'   `paths`.
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_fs_mount_point(".")

ps_fs_mount_point <- function(paths) {
  assert_character(paths)
  paths <- normalizePath(paths, mustWork = TRUE)
  call_with_cleanup(ps__mount_point, paths)
}
