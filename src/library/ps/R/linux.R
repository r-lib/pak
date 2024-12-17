
#' @importFrom utils read.table

psl_connections <- function(p) {
  sock_raw <- not_null(.Call(psll_connections, p))
  sock <- data_frame(
    fd = as.integer(vapply(sock_raw, "[[", character(1), 1)),
    id = vapply(sock_raw, "[[", character(1), 2)
  )

  flt <- function(x, col, values) x[x[[col]] %in% values, ]

  unix <- flt(psl__read_table("/proc/net/unix"),         "V7",  sock$id)
  tcp  <- flt(psl__read_table("/proc/net/tcp") [, 1:10], "V10", sock$id)
  tcp6 <- flt(psl__read_table("/proc/net/tcp6")[, 1:10], "V10", sock$id)
  udp  <- flt(psl__read_table("/proc/net/udp") [, 1:10], "V10", sock$id)
  udp6 <- flt(psl__read_table("/proc/net/udp6")[, 1:10], "V10", sock$id)

  ## Sockets that still existed when we queried /proc/net,
  ## because some of them might be closed already...
  sockx <- flt(sock, "id",
               c(unix$V7, tcp$V10, tcp6$V10, udp$V10, udp6$V10))

  if (length(tcp) && nrow(tcp)) {
    tcp$type <- "SOCK_STREAM"
    tcp$family <- "AF_INET"
  }
  if (length(tcp6) && nrow(tcp6)) {
    tcp6$type <- "SOCK_STREAM"
    tcp6$family <- "AF_INET6"
  }
  if (length(udp) && nrow(udp)) {
    udp$type <- "SOCK_DGRAM"
    udp$family <- "AF_INET"
  }
  if (length(udp6) && nrow(udp6)) {
    udp6$type <- "SOCK_DGRAM"
    udp6$family <- "AF_INET6"
  }
  net <- rbind(tcp, tcp6, udp, udp6)

  ## Unix socket might or might not have a path
  if (length(unix) && nrow(unix)) {
    unix$V8 <- unix$V8 %||% ""
    unix$V8[unix$V8 == ""] <- NA_character_
  }

  ## The status column is 01...09, 0A, 0B, but R might parse it as integer
  if (length(unix) && nrow(unix)) {
    unix$V6 <- str_tail(paste0('0', as.character(unix$V6)), 2)
  }
  if (length(net) && nrow(net)) {
    net$V4 <- str_tail(paste0('0', as.character(net$V4)), 2)
  }

  d <- data_frame(
    fd = integer(),
    family = character(),
    type = character(),
    laddr = character(),
    lport = integer(),
    raddr = character(),
    rport = integer(),
    state = character()
  )

  if (length(unix) &&  nrow(unix)) {
    d <- data_frame(
      fd = sockx$fd[match(unix$V7, sockx$id)],
      family = "AF_UNIX",
      type = match_names(ps_env$constants$socket_types, unix$V5),
      laddr = unix$V8,
      lport = NA_integer_,
      raddr = NA_character_,
      rport = NA_integer_,
      state = NA_character_)
  }

  if (!is.null(net)) {

    laddr <- mapply(psl__decode_address, net$V2, net$family,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)
    raddr <- mapply(psl__decode_address, net$V3, net$family,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)

    net_d <- data_frame(
      fd = sockx$fd[match(net$V10, sockx$id)],
      family = net$family,
      type = net$type,
      laddr = vapply(laddr, "[[", character(1), 1),
      lport = vapply(laddr, "[[", integer(1), 2),
      raddr = vapply(raddr, "[[", character(1), 1),
      rport = vapply(raddr, "[[", integer(1), 2),
      state = match_names(ps_env$constants$tcp_statuses, net$V4))

    d <- rbind(d, net_d)
  }

  d
}

psl__read_table <- function(file, stringsAsFactors = FALSE, header = FALSE,
                            skip = 1, fill = TRUE, ...) {
  tryCatch(
    read.table(file, stringsAsFactors = stringsAsFactors, header = header,
               skip = skip, fill = fill, ...),
    error = function(e) NULL
  )
}

psl__decode_address <- function(addr, family) {
  ipp <- strsplit(addr, ":")[[1]]
  if (length(ipp) != 2) return(list(NA_character_, NA_integer_))
  addr <- str_strip(ipp[[1]])
  port <- strtoi(ipp[[2]], 16)

  if (family == "AF_INET") {
    AF_INET <- ps_env$constants$address_families[["AF_INET"]]
    addrn <- strtoi(substring(addr, 1:4*2-1, 1:4*2), base = 16)
    if (.Platform$endian == "little") addrn <- rev(addrn)
    addrs <- .Call(ps__inet_ntop, as.raw(addrn), AF_INET) %||% NA_character_
    list(addrs, port)

  } else {
    AF_INET6 <- ps_env$constants$address_families[["AF_INET6"]]
    addrn <- strtoi(substring(addr, 1:16*2-1, 1:16*2), base = 16)
    if (.Platform$endian == "little") {
      addrn[ 1: 4] <- rev(addrn[ 1: 4])
      addrn[ 5: 8] <- rev(addrn[ 5: 8])
      addrn[ 9:12] <- rev(addrn[ 9:12])
      addrn[13:16] <- rev(addrn[13:16])
    }
    addrs <- .Call(ps__inet_ntop, as.raw(addrn), AF_INET6) %||% NA_character_
    list(addrs, port)
  }
}

psl__cpu_count_from_lscpu <- function() {
  tryCatch({
    lines <- system("lscpu -p=core", intern = TRUE)
    cores <- unique(lines[!str_starts_with(lines, "#")])
    length(cores)
  }, error = function(e) psl__cpu_count_from_cpuinfo())
}

psl__cpu_count_from_cpuinfo <- function() {
  lines <- readLines("/proc/cpuinfo")
  mapping = list()
  current = list()

  for (l in lines) {
    l <- tolower(str_strip(l))
    if (!nchar(l)) {
      if ("physical id" %in% names(current) &&
          "cpu cores" %in% names(current)) {
        mapping[[ current[["physical id"]] ]] <- current[["cpu cores"]]
      }
      current <- list()

    } else {
      if (str_starts_with(l, "physical id") ||
          str_starts_with(l, "cpu cores")) {
        kv <- strsplit(l, "\\t+:")[[1]]
        current[[ kv[[1]] ]] <- kv[[2]]
      }
    }
  }

  sum(as.integer(unlist(mapping)))
}

ps_cpu_count_physical_linux <- function() {
  if (Sys.which("lscpu") != "") {
    psl__cpu_count_from_lscpu()
  } else {
    psl__cpu_count_from_cpuinfo()
  }
}

ps__system_cpu_times_linux <- function() {
  clock_ticks <- tryCatch(
    as.numeric(system("getconf CLK_TCK", intern=TRUE)),
    error = function(e) return(250)
  )
  stat <- readLines("/proc/stat", n = 1)
  tms <- as.double(strsplit(stat, "\\s+")[[1]][-1]) / clock_ticks
  nms <- c(
    "user", "nice", "system", "idle", "iowait", "irq", "softirq", "steal",
    "guest", "guest_nice"
  )
  names(tms) <- nms[1:length(tms)]
  tms
}

ps__disk_io_counters_linux <- function() {
  # Internal disk IO counters for Linux, reads lines from diskstats if it exists
  # or /sys/block as a backup
  if (file.exists("/proc/diskstats")) {
    ps__read_procfs()
  } else if (dir.exists("/sys/block")) {
    ps__read_sysfs()
  } else {
    stop("Can't read disk IO, neither /proc/diskstats or /sys/block on this system")
  }
}

ps__is_storage_device <- function(name, including_virtual = TRUE) {
  # Whether a named drive (e.g. 'sda') is a real storage device, or a virtual one
  name <- gsub("/", "!", name, fixed=TRUE)
  if (including_virtual) {
    path <- file.path("/sys/block", name)
  } else {
    path <- file.path("/sys/block", name, "device")
  }

  return(dir.exists(path))
}

ps__read_procfs <- function() {
  # Read total disk IO stats from /proc/diskstats
  file <- readLines("/proc/diskstats")
  # Get info as list of vectors (could be different lengths)
  lines <- strsplit(trimws(file), "\\s+")
  # Pre-allocate matrix of info as NAs
  mat <- matrix(data = NA_character_, nrow = length(lines), ncol = 10)
  for (i in seq_along(lines)) {
    # For each line, check length and insert into matrix as appropriate
    line <- lines[[i]]
    flen <- length(line)
    if (flen == 15) {
      # Linux 2.4
      mat[i, 1] <- line[[4]] # name
      mat[i, 2] <- line[[3]] # reads
      mat[i, 3:9] <- line[5:11] # reads_merged, rbytes, rtime, writes, writes_merged, wbytes, wtime
      mat[i, 10] <- line[[14]] # busy_time
    } else if (flen == 14 || flen >= 18) {
      # Linux 2.6+, line referring to a disk
      mat[i, 1] <- line[[3]] # name
      mat[i, 2:9] <- line[5:12] # reads, reads_merged, rbytes, rtime, writes, writes_merged, wbytes, wtime
      mat[i, 10] <- line[[14]] # busy_time
    } else if (flen == 7) {
      # Linux 2.6+, line referring to a partition
      mat[i, 1] <- line[[2]] # name
      mat[i, 2] <- line[[4]] # reads
      mat[i, 4:5] <- line[5:6] # rbytes, writes
      mat[i, 8] <- line[7] # wbytes
    } else {
      stop("Cannot read diskstats file")
    }
  }

  # Add names and convert types as appropriate
  return(data.frame(
    name = mat[, 1],
    read_count = as.numeric(mat[, 2]),
    read_merged_count = as.numeric(mat[, 3]),
    read_bytes = as.numeric(mat[, 4]) * 512, # Multiple by disk sector size to get bytes
    read_time = as.numeric(mat[, 5]),
    write_count = as.numeric(mat[, 6]),
    write_merged_count = as.numeric(mat[, 7]),
    write_bytes = as.numeric(mat[, 8]) * 512, # Multiple by disk sector size to get bytes
    write_time = as.numeric(mat[, 9]),
    busy_time = as.numeric(mat[, 10])
  ))
}

ps__read_sysfs <- function() {
  # Read disk IO from each device folder in /sys/block

  # Get stat files for each block
  blocks <- list.dirs("/sys/block/", recursive = FALSE)
  all_files <- list.files(blocks, full.names = TRUE)
  stats <- all_files[grepl("(stat)$", all_files)]

  # Pre-allocate list for dfs
  disk_info <- vector(mode = "list", length = length(stats))
  for (i in seq_along(stats)) {
    # Read in each file, get the field
    stat <- stats[[i]]
    fields <- readLines(stat)
    fields <- unlist(strsplit(trimws(fields), "\\s+"))

    # Save all info as a dataframe
    block_info <- data.frame(
      name = strsplit(stat, "/", fixed=TRUE)[[1]][[5]], # Extract name from stat filepath
      read_count = as.numeric(fields[[1]]),
      read_merged_count = as.numeric(fields[[2]]),
      read_bytes = as.numeric(fields[[3]]) * 512, # Multiple by disk sector size to get bytes
      read_time = as.numeric(fields[[4]]),
      write_count = as.numeric(fields[[5]]),
      write_merged_count = as.numeric(fields[[6]]),
      write_bytes = as.numeric(fields[[7]]) * 512, # Multiple by disk sector size to get bytes
      write_time = as.numeric(fields[[8]]),
      busy_time = as.numeric(fields[[10]])
    )

    disk_info[[i]] <- block_info
  }
  return(do.call(rbind, disk_info))
}
