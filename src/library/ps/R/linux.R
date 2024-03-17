
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
