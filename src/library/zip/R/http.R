cp437_to_utf8 <- function(raw_bytes) {
  .Call(c_R_zip_cp437_to_utf8, raw_bytes)
}

read_u16 <- function(con) {
  b <- readBin(con, "raw", n = 2L)
  as.integer(b[1L]) + as.integer(b[2L]) * 256L
}

read_u32 <- function(con) {
  b <- readBin(con, "raw", n = 4L)
  as.numeric(b[1L]) +
    as.numeric(b[2L]) * 256 +
    as.numeric(b[3L]) * 65536 +
    as.numeric(b[4L]) * 16777216
}

# might lose precision for values > 2^53, but that's not a problem for
# ZIP64 sizes/offsets, it'd happen with an ZIP file larger than 8PB...
read_u64 <- function(con) {
  b <- readBin(con, "raw", n = 8L)
  as.numeric(b[1L]) +
    as.numeric(b[2L]) * 256 +
    as.numeric(b[3L]) * 65536 +
    as.numeric(b[4L]) * 16777216 +
    as.numeric(b[5L]) * 4294967296 +
    as.numeric(b[6L]) * 1099511627776 +
    as.numeric(b[7L]) * 281474976710656 +
    as.numeric(b[8L]) * 72057594037927936
}

dos_to_posix <- function(date_word, time_word) {
  year <- bitwShiftR(date_word, 9L) + 1980L
  month <- bitwAnd(bitwShiftR(date_word, 5L), 15L)
  day <- bitwAnd(date_word, 31L)
  hour <- bitwShiftR(time_word, 11L)
  min <- bitwAnd(bitwShiftR(time_word, 5L), 63L)
  sec <- bitwAnd(time_word, 31L) * 2L
  ISOdatetime(year, month, day, hour, min, sec, tz = "UTC")
}

find_eocd <- function(data) {
  sig <- as.raw(c(0x50, 0x4b, 0x05, 0x06))
  n <- length(data)
  for (i in seq(n - 21L, max(1L, n - 65536L), by = -1L)) {
    if (
      data[i] == sig[1L] &&
        data[i + 1L] == sig[2L] &&
        data[i + 2L] == sig[3L] &&
        data[i + 3L] == sig[4L]
    ) {
      return(i)
    }
  }
  stop("Cannot find End of Central Directory record in the downloaded range")
}

parse_eocd <- function(data, pos, range_start = 0) {
  con <- rawConnection(data[pos:length(data)], "r")
  on.exit(close(con))
  readBin(con, "raw", n = 4L) # signature
  readBin(con, "raw", n = 2L) # disk number
  readBin(con, "raw", n = 2L) # disk with CD start
  readBin(con, "raw", n = 2L) # entries on this disk
  num_entries <- read_u16(con)
  cd_size <- read_u32(con)
  cd_offset <- read_u32(con)

  # When any field is saturated, this is a ZIP64 archive and the real
  # values live in the ZIP64 EOCD record, reached via the ZIP64 EOCD
  # locator that sits immediately before the classic EOCD.
  if (
    num_entries == 0xFFFFL || cd_size >= 0xFFFFFFFF || cd_offset >= 0xFFFFFFFF
  ) {
    z64 <- parse_zip64_eocd(data, pos, range_start)
    if (!is.null(z64)) {
      num_entries <- z64$num_entries
      cd_size <- z64$cd_size
      cd_offset <- z64$cd_offset
    }
  }

  list(num_entries = num_entries, cd_size = cd_size, cd_offset = cd_offset)
}

parse_zip64_eocd <- function(data, eocd_pos, range_start) {
  # ZIP64 EOCD locator: 20 bytes, immediately before the classic EOCD.
  loc_pos <- eocd_pos - 20L
  if (loc_pos < 1L) {
    return(NULL)
  }
  lcon <- rawConnection(data[loc_pos:(loc_pos + 19L)], "r")
  on.exit(close(lcon))
  if (read_u32(lcon) != 0x07064b50) {
    return(NULL)
  }
  read_u32(lcon) # disk holding the ZIP64 EOCD record
  z64_offset <- read_u64(lcon) # absolute offset of the ZIP64 EOCD record

  rec_pos <- z64_offset - range_start + 1
  if (rec_pos < 1 || rec_pos + 56L - 1L > length(data)) {
    stop("ZIP64 End of Central Directory record is outside the fetched range")
  }
  rcon <- rawConnection(data[rec_pos:length(data)], "r")
  on.exit(close(rcon), add = TRUE)
  if (read_u32(rcon) != 0x06064b50) {
    stop("Expected ZIP64 End of Central Directory signature")
  }
  read_u64(rcon) # size of remaining ZIP64 EOCD record
  read_u16(rcon) # version made by
  read_u16(rcon) # version needed
  read_u32(rcon) # number of this disk
  read_u32(rcon) # disk with CD start
  read_u64(rcon) # entries on this disk
  num_entries <- read_u64(rcon)
  cd_size <- read_u64(rcon)
  cd_offset <- read_u64(rcon)
  list(num_entries = num_entries, cd_size = cd_size, cd_offset = cd_offset)
}

parse_zip64_extra <- function(extra_raw, comp_size, uncomp_size, local_offset) {
  n <- length(extra_raw)
  if (n < 4L) {
    return(NULL)
  }
  con <- rawConnection(extra_raw, "r")
  on.exit(close(con))
  remaining <- n
  result <- list()
  while (remaining >= 4L) {
    tag <- read_u16(con)
    size <- read_u16(con)
    remaining <- remaining - 4L
    if (remaining < size) {
      break
    }
    chunk <- readBin(con, "raw", n = size)
    remaining <- remaining - size
    if (tag != 0x0001L) {
      next
    }
    ccon <- rawConnection(chunk, "r")
    on.exit(close(ccon), add = TRUE)
    if (uncomp_size >= 0xFFFFFFFF) {
      result$uncomp_size <- read_u64(ccon)
    }
    if (comp_size >= 0xFFFFFFFF) {
      result$comp_size <- read_u64(ccon)
    }
    if (local_offset >= 0xFFFFFFFF) result$local_offset <- read_u64(ccon)
  }
  if (length(result)) result else NULL
}

url_host <- function(url) {
  host <- tryCatch(curl::curl_parse_url(url)$host, error = function(e) NULL)
  if (is.null(host) || !nzchar(host)) url else host
}

warn_no_range <- function(url) {
  warning(sprintf(
    paste0(
      "Server '%s' does not support HTTP range requests, ",
      "downloading the whole file."
    ),
    url_host(url)
  ))
}

zip_range_get <- function(url, range_str, handle) {
  curl::handle_setheaders(handle, "Range" = range_str)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  list(
    status = resp$status_code,
    content = resp$content,
    headers = curl::parse_headers_list(resp$headers)
  )
}

zip_fetch_cd <- function(url, handle) {
  resp <- zip_range_get(url, "bytes=-65536", handle)

  # Some servers (e.g. GitHub's CDN) reject a suffix range that is larger
  # than the file with 416 Range Not Satisfiable instead of returning the
  # whole file. The 416 response still reports the size in its Content-Range
  # header ("bytes */SIZE"), so retry with an explicit range anchored at the
  # start of the last 64k (or the start of the file when it is smaller).
  if (resp$status == 416L) {
    cr <- resp$headers[["content-range"]]
    filesize <- if (!is.null(cr)) {
      as.numeric(sub(".*/([0-9]+)$", "\\1", cr))
    } else {
      NA_real_
    }
    range_str <- if (!is.na(filesize)) {
      sprintf("bytes=%.0f-", max(0, filesize - 65536))
    } else {
      "bytes=0-"
    }
    resp <- zip_range_get(url, range_str, handle)
  }

  if (resp$status == 200L) {
    return(list(use_range = FALSE, body = resp$content))
  }
  if (resp$status != 206L) {
    stop(sprintf("Unexpected HTTP status %d for '%s'", resp$status, url))
  }

  cr <- resp$headers[["content-range"]]
  if (is.null(cr)) {
    stop("Server returned 206 without Content-Range header")
  }
  filesize <- as.numeric(sub(".*/([0-9]+)$", "\\1", cr))
  if (is.na(filesize)) {
    stop("Cannot parse Content-Range header: ", cr)
  }

  tail_data <- resp$content
  range_start <- filesize - length(tail_data)

  eocd_pos <- find_eocd(tail_data)
  eocd <- parse_eocd(tail_data, eocd_pos, range_start)

  cd_offset <- eocd$cd_offset
  cd_size <- eocd$cd_size

  if (cd_offset >= range_start) {
    buf_start <- cd_offset - range_start + 1L
    cd_raw <- tail_data[buf_start:(buf_start + cd_size - 1L)]
  } else {
    resp2 <- zip_range_get(
      url,
      sprintf("bytes=%.0f-%.0f", cd_offset, cd_offset + cd_size - 1),
      handle
    )
    if (!resp2$status %in% c(200L, 206L)) {
      stop(sprintf(
        "Unexpected HTTP status %d fetching Central Directory",
        resp2$status
      ))
    }
    cd_raw <- resp2$content
  }

  list(
    use_range = TRUE,
    cd_raw = cd_raw,
    num_entries = eocd$num_entries,
    filesize = filesize
  )
}

zip_parse_cd <- function(cd_info, encoding = NULL) {
  cd_raw <- cd_info$cd_raw
  num_entries <- cd_info$num_entries

  con <- rawConnection(cd_raw, "r")
  on.exit(close(con))

  filename <- character(num_entries)
  compressed_size <- numeric(num_entries)
  uncompressed_size <- numeric(num_entries)
  timestamp <- numeric(num_entries)
  permissions <- integer(num_entries)
  crc32 <- numeric(num_entries)
  offset <- numeric(num_entries)
  type_idx <- integer(num_entries)
  method <- integer(num_entries)

  for (i in seq_len(num_entries)) {
    sig <- read_u32(con)
    if (sig != 0x02014b50) {
      stop(sprintf("Expected CD signature at entry %d (got 0x%08X)", i, sig))
    }
    version_by <- read_u16(con)
    read_u16(con) # version needed
    flags <- read_u16(con)
    meth <- read_u16(con)
    mod_time <- read_u16(con)
    mod_date <- read_u16(con)
    crc <- read_u32(con)
    csz <- read_u32(con)
    usz <- read_u32(con)
    fname_len <- read_u16(con)
    extra_len <- read_u16(con)
    comment_len <- read_u16(con)
    read_u16(con) # disk start
    read_u16(con) # internal attr
    ext_attr <- read_u32(con)
    lhdr_off <- read_u32(con)

    fname_raw <- readBin(con, "raw", n = fname_len)
    extra_raw <- readBin(con, "raw", n = extra_len)
    readBin(con, "raw", n = comment_len)

    z64 <- parse_zip64_extra(extra_raw, csz, usz, lhdr_off)
    if (!is.null(z64)) {
      if (!is.null(z64$comp_size)) {
        csz <- z64$comp_size
      }
      if (!is.null(z64$uncomp_size)) {
        usz <- z64$uncomp_size
      }
      if (!is.null(z64$local_offset)) lhdr_off <- z64$local_offset
    }

    utf8_flag <- bitwAnd(flags, 0x800L) != 0L
    fname <- if (utf8_flag) {
      rawToChar(fname_raw)
    } else if (!is.null(encoding)) {
      iconv(rawToChar(fname_raw), from = encoding, to = "UTF-8")
    } else {
      cp437_to_utf8(fname_raw)
    }
    Encoding(fname) <- "UTF-8"

    unix_version <- bitwAnd(bitwShiftR(version_by, 8L), 0xFFL) == 3L
    # ext_attr is a u32 read as a double; the high bit is set for regular
    # files (S_IFREG), so it can exceed .Machine$integer.max. Use double
    # arithmetic to extract the upper 16 bits before any integer coercion.
    unix_attr <- bitwAnd(as.integer(ext_attr %/% 65536), 0xFFFFL)
    perm <- if (unix_version && unix_attr != 0L) {
      bitwAnd(unix_attr, 0x1FFL)
    } else if (endsWith(fname, "/")) {
      0700L
    } else {
      0600L
    }

    fmt <- bitwAnd(unix_attr, 0xF000L)
    tidx <- if (fmt == 0x6000L || bitwAnd(unix_attr, 0x6000L) == 0x6000L) {
      1L
    } else if (fmt == 0x2000L) {
      2L
    } else if (fmt == 0x4000L || endsWith(fname, "/")) {
      3L
    } else if (fmt == 0x1000L) {
      4L
    } else if (fmt == 0xA000L) {
      5L
    } else if (fmt == 0xC000L) {
      6L
    } else {
      0L
    }

    filename[i] <- fname
    compressed_size[i] <- csz
    uncompressed_size[i] <- usz
    timestamp[i] <- as.numeric(dos_to_posix(mod_date, mod_time))
    permissions[i] <- perm
    crc32[i] <- crc
    offset[i] <- lhdr_off
    type_idx[i] <- tidx
    method[i] <- meth
  }

  df <- data_frame(
    filename = filename,
    compressed_size = compressed_size,
    uncompressed_size = uncompressed_size,
    timestamp = as.POSIXct(timestamp, tz = "UTC", origin = "1970-01-01")
  )
  df$permissions <- as.octmode(permissions)
  crc32_int <- integer(length(crc32))
  lo <- crc32 < 2^31
  crc32_int[lo] <- as.integer(crc32[lo])
  crc32_int[!lo] <- as.integer(crc32[!lo] - 2^32)
  df$crc32 <- as.hexmode(crc32_int)
  df$offset <- offset
  df$type <- file_types[type_idx + 1L]
  df$.method <- method # internal; stripped by zip_list_url before returning
  df
}

zip_list_url <- function(url, encoding) {
  need_packages("curl", "url ZIP files")
  h <- curl::new_handle()
  cd_info <- zip_fetch_cd(url, h)
  if (!cd_info$use_range) {
    warn_no_range(url)
    tmp <- tempfile(fileext = ".zip")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(cd_info$body, tmp)
    return(zip_list(tmp, encoding))
  }
  df <- zip_parse_cd(cd_info, encoding)
  df$.method <- NULL
  df
}

parse_local_header <- function(buf) {
  con <- rawConnection(buf, "r")
  on.exit(close(con))
  sig <- read_u32(con)
  if (sig != 0x04034b50) {
    stop("Expected local file header signature (0x04034b50)")
  }
  read_u16(con) # version needed
  read_u16(con) # flags
  meth <- read_u16(con)
  read_u16(con) # mod time
  read_u16(con) # mod date
  read_u32(con) # CRC-32
  read_u32(con) # compressed size (may be 0 if data descriptor used)
  read_u32(con) # uncompressed size
  fname_len <- read_u16(con)
  extra_len <- read_u16(con)
  list(method = meth, fname_len = fname_len, extra_len = extra_len)
}

unzip_url <- function(url, files, overwrite, junkpaths, exdir, encoding) {
  need_packages("curl", "url ZIP files")
  h <- curl::new_handle()
  cd_info <- zip_fetch_cd(url, h)

  if (!cd_info$use_range) {
    warn_no_range(url)
    tmp <- tempfile(fileext = ".zip")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(cd_info$body, tmp)
    return(unzip(
      tmp,
      files = files,
      overwrite = overwrite,
      junkpaths = junkpaths,
      exdir = exdir,
      encoding = encoding
    ))
  }

  entries <- zip_parse_cd(cd_info, encoding)
  methods <- entries$.method
  entries$.method <- NULL

  if (!is.null(files)) {
    missing_files <- setdiff(files, entries$filename)
    if (length(missing_files)) {
      stop(
        "Files not found in ZIP archive: ",
        paste(missing_files, collapse = ", ")
      )
    }
    keep <- entries$filename %in% files
    entries <- entries[keep, , drop = FALSE]
    methods <- methods[keep]
  }

  mkdirp(exdir)
  exdir <- normalizePath(exdir)

  is_dir <- endsWith(entries$filename, "/") | entries$type == "directory"

  for (dname in entries$filename[is_dir]) {
    outpath <- if (junkpaths) {
      file.path(exdir, basename(sub("/$", "", dname)))
    } else {
      file.path(exdir, dname)
    }
    mkdirp(outpath)
  }

  paths <- character(nrow(entries))
  paths[is_dir] <- file.path(exdir, sub("/$", "", entries$filename[is_dir]))

  file_idx <- which(!is_dir)
  for (ii in seq_along(file_idx)) {
    i <- file_idx[ii]
    entry <- entries[i, ]
    meth <- methods[i]

    fname_bytes <- nchar(entry$filename, type = "bytes")
    range_end <- entry$offset +
      29 +
      fname_bytes +
      256 +
      entry$compressed_size -
      1
    resp <- zip_range_get(
      url,
      sprintf("bytes=%.0f-%.0f", entry$offset, range_end),
      h
    )

    outpath <- if (junkpaths) {
      file.path(exdir, basename(entry$filename))
    } else {
      p <- file.path(exdir, entry$filename)
      mkdirp(dirname(p))
      p
    }

    if (!overwrite && file.exists(outpath)) {
      stop("File already exists (use overwrite = TRUE): ", outpath)
    }

    # Ranges worked for the central directory fetch but the server ignored
    # this one and sent the whole file, so it does not really support ranges.
    # Use the downloaded archive to extract this and all remaining entries at
    # once, instead of re-requesting (and re-downloading) the file per entry.
    if (resp$status == 200L) {
      warn_no_range(url)
      tmp <- tempfile(fileext = ".zip")
      on.exit(unlink(tmp), add = TRUE)
      writeBin(resp$content, tmp)
      remaining <- file_idx[ii:length(file_idx)]
      unzip(
        tmp,
        files = entries$filename[remaining],
        overwrite = overwrite,
        junkpaths = junkpaths,
        exdir = exdir,
        encoding = encoding
      )
      paths[remaining] <- if (junkpaths) {
        file.path(exdir, basename(entries$filename[remaining]))
      } else {
        file.path(exdir, entries$filename[remaining])
      }
      break
    }

    if (resp$status != 206L) {
      stop(sprintf(
        "Unexpected HTTP status %d for '%s'",
        resp$status,
        entry$filename
      ))
    }

    lhdr <- parse_local_header(resp$content)
    data_start <- 30L + lhdr$fname_len + lhdr$extra_len # 0-based
    comp_size <- entry$compressed_size

    if (data_start + comp_size > length(resp$content)) {
      data_off <- entry$offset + data_start
      resp2 <- zip_range_get(
        url,
        sprintf("bytes=%.0f-%.0f", data_off, data_off + comp_size - 1),
        h
      )
      if (!resp2$status %in% c(200L, 206L)) {
        stop(sprintf(
          "Unexpected HTTP status %d fetching data for '%s'",
          resp2$status,
          entry$filename
        ))
      }
      compressed <- resp2$content
    } else {
      compressed <- resp$content[(data_start + 1L):(data_start + comp_size)]
    }

    decompressed <- if (meth == 0L) {
      compressed
    } else if (meth == 8L) {
      inflate(compressed, raw = TRUE)$output
    } else {
      stop(sprintf(
        "Unsupported compression method %d for '%s'",
        meth,
        entry$filename
      ))
    }

    writeBin(decompressed, outpath)
    paths[i] <- outpath
  }

  entries$path <- paths
  Encoding(entries$path) <- "UTF-8"
  invisible(entries)
}
