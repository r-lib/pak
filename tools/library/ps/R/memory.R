
#' Statistics about system memory usage
#'
#' @return Named list. All numbers are in bytes:
#' * `total`: total physical memory (exclusive swap).
#' * `avail` the memory that can be given instantly to processes without
#'   the system going into swap. This is calculated by summing different
#'   memory values depending on the platform and it is supposed to be used
#'   to monitor actual memory usage in a cross platform fashion.
#' * `percent`: Percentage of memory that is taken.
#' * `used`: memory used, calculated differently depending on
#'   the platform and designed for informational purposes only.
#'   `total` - `free` does not necessarily match `used`.
#' * `free`: memory not being used at all (zeroed) that is
#'   readily available; note that this doesn’t reflect the actual memory
#'   available (use `available` instead). `total` - `used` does not
#'   necessarily match `free`.
#' * `active`: (Unix only) memory currently in use or very recently used,
#'   and so it is in RAM.
#' * `inactive`: (Unix only) memory that is marked as not used.
#' * `wired`: (macOS only) memory that is marked to always stay in RAM. It
#'   is never moved to disk.
#' * `buffers`: (Linux only) cache for things like file system metadata.
#' * `cached`: (Linux only) cache for various things.
#' * `shared`: (Linux only) memory that may be simultaneously accessed by
#'   multiple processes.
#' * `slab`:  (Linux only) in-kernel data structures cache.
#'
#' @family memory functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_system_memory()

ps_system_memory <- function() {
  os <- ps_os_name()

  if (os == "MACOS") {
    l <- .Call(ps__system_memory)
    l$avail <- l$inactive + l$free
    l$used <- l$active + l$wired
    l$free <- l$free - l$speculative
    l$percent <- (l$total - l$avail) / l$total * 100
    l[c("total", "avail", "percent", "used", "free",
        "active", "inactive", "wired")]

  } else if (os == "LINUX") {
    ps__system_memory_linux()

  } else if (os == "WINDOWS") {
    l <- .Call(ps__system_memory)[c("total", "avail")]
    l$free <- l$avail
    l$used <- l$total - l$avail
    l$percent <- (l$total - l$avail) * 100 / l$total
    l[c("total", "avail", "percent", "used", "free")]

  } else {
    stop("ps is not supported in this platform")
  }
}

ps__system_memory_linux <- function() {
  tab <- read.table("/proc/meminfo", header = FALSE, fill = TRUE)
  mems <- structure(
    as.list(tab[[2]] * 1024),
    names = tolower(sub(":$", "", tab[[1]]))
  )

  total <- mems[["memtotal"]]
  free <- mems[["memfree"]]

  buffers <- mems[["buffers"]] %||% NA_real_
  cached <- mems[["cached"]] %||% NA_real_ + mems[["sreclaimable"]] %||% 0
  shared <- mems[["shmem"]] %||% mems[["memshared"]] %||% NA_real_
  active <- mems[["active"]] %||% NA_real_

  inactive <- mems[["inactive"]]
  if (is.null(inactive)) {
    inactive <-
      mems[["inact_dirty"]] + mems[["inact_clean"]] + mems[["inact_laundry"]]
  }
  if (length(inactive) == 0) inactive <- NA_real_

  slab <- mems[["slab"]] %||% 0

  used <- total - free - cached - buffers
  if (used < 0 || is.na(used)) {
    # May be symptomatic of running within a LCX container where such
    # values will be dramatically distorted over those of the host.
    used <- total - free
  }

  avail <- mems[["memavailable"]] %||% NA_real_

  # If avail is greater than total or our calculation overflows,
  # that's symptomatic of running within a LCX container where such
  # values will be dramatically distorted over those of the host.
  # https://gitlab.com/procps-ng/procps/blob/
  #     24fd2605c51fccc375ab0287cec33aa767f06718/proc/sysinfo.c#L764
  if (!is.na(avail) && avail > total) avail <- free

  percent <- (total - avail) / total * 100

  list(total = total, avail = avail, percent = percent, used = used,
       free = free, active = active, inactive = inactive, buffers = buffers,
       cached = cached, shared = shared, slab = slab)
}

#' System swap memory statistics
#'
#' @return Named list. All numbers are in bytes:
#' * `total`: total swap memory.
#' * `used`: used swap memory.
#' * `free`: free swap memory.
#' * `percent`: the percentage usage.
#' * `sin`: the number of bytes the system has swapped in from disk
#'   (cumulative). This is `NA` on Windows.
#' * `sout`: the number of bytes the system has swapped out from disk
#'   (cumulative). This is `NA` on Windows.
#'
#' @family memory functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_system_swap()

ps_system_swap <- function() {
  os <- ps_os_name()

  if (os == "MACOS") {
    l <- .Call(ps__system_swap)
    l$percent <- l$used / l$total * 100
    l[c("total", "used", "free", "percent", "sin", "sout")]
  } else if (os == "LINUX") {
    ps__system_swap_linux()

  } else if (os == "WINDOWS") {
    l <- .Call(ps__system_memory)
    total <- l[[3]]
    free <- l[[4]]
    used <- total - free
    percent <- used / total
    list(total = total, used = used, free = free,
         percent = percent, sin = NA_real_, sout = NA_real_)

  } else {
    stop("ps is not supported in this platform")
  }
}

ps__system_swap_linux <- function() {
  tab <- read.table("/proc/meminfo", header = FALSE, fill = TRUE)
  mems <- structure(
    as.list(tab[[2]] * 1024),
    names = tolower(sub(":$", "", tab[[1]]))
  )

  total <- mems[["swaptotal"]]
  free <- mems[["swapfree"]]
  used <- total - free
  percent <- used / total * 100
  sin <- sout <- 0

  tryCatch({
    tab2 <- read.table("/proc/vmstat", header = FALSE)
    vms <- structure(as.list(tab2[[2]] * 4 * 1024), names = tab2[[1]])
    sin <- vms[["pswpin"]] %||% 0
    sout <- vms[["pswpout"]] %||% 0
    }, error = function(e) NULL
  )

  list(total = total, used = used, free = free, percent = percent,
       sin = sin, sout = sout)
}
