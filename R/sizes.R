format_bytes <- local({
  pretty_bytes <- function(bytes, style = c("default", "nopad", "6")) {
    style <- switch(
      match.arg(style),
      "default" = pretty_bytes_default,
      "nopad" = pretty_bytes_nopad,
      "6" = pretty_bytes_6
    )

    style(bytes)
  }

  compute_bytes <- function(bytes, smallest_unit = "B") {
    units0 <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")

    stopifnot(
      is.numeric(bytes),
      is.character(smallest_unit),
      length(smallest_unit) == 1,
      !is.na(smallest_unit),
      smallest_unit %in% units0
    )

    limits <- c(1000, 999950 * 1000^(seq_len(length(units0) - 2) - 1))
    low <- match(smallest_unit, units0)
    units <- units0[low:length(units0)]
    limits <- limits[low:length(limits)]

    neg <- bytes < 0 & !is.na(bytes)
    bytes <- abs(bytes)

    mat <- matrix(
      rep(bytes, each = length(limits)),
      nrow = length(limits),
      ncol = length(bytes)
    )
    mat2 <- matrix(mat < limits, nrow = length(limits), ncol = length(bytes))
    exponent <- length(limits) - colSums(mat2) + low - 1L
    res <- bytes / 1000^exponent
    unit <- units[exponent - low + 2L]

    ## Zero bytes
    res[bytes == 0] <- 0
    unit[bytes == 0] <- units[1]

    ## NA and NaN bytes
    res[is.na(bytes)] <- NA_real_
    res[is.nan(bytes)] <- NaN
    unit[is.na(bytes)] <- units0[low] # Includes NaN as well

    data.frame(
      stringsAsFactors = FALSE,
      amount = res,
      unit = unit,
      negative = neg
    )
  }

  pretty_bytes_default <- function(bytes) {
    szs <- compute_bytes(bytes)
    amt <- szs$amount

    ## String. For fractions we always show two fraction digits
    res <- character(length(amt))
    int <- is.na(amt) | amt == as.integer(amt)
    res[int] <- format(
      ifelse(szs$negative[int], -1, 1) * amt[int],
      scientific = FALSE
    )
    res[!int] <- sprintf("%.2f", ifelse(szs$negative[!int], -1, 1) * amt[!int])

    format(paste(res, szs$unit), justify = "right")
  }

  pretty_bytes_nopad <- function(bytes) {
    sub("^\\s+", "", pretty_bytes_default(bytes))
  }

  pretty_bytes_6 <- function(bytes) {
    szs <- compute_bytes(bytes, smallest_unit = "kB")
    amt <- szs$amount

    na <- is.na(amt)
    nan <- is.nan(amt)
    neg <- !na & !nan & szs$negative
    l10 <- !na & !nan & !neg & amt < 10
    l100 <- !na & !nan & !neg & amt >= 10 & amt < 100
    b100 <- !na & !nan & !neg & amt >= 100

    szs$unit[neg] <- "kB"

    famt <- character(length(amt))
    famt[na] <- " NA"
    famt[nan] <- "NaN"
    famt[neg] <- "< 0"
    famt[l10] <- sprintf("%.1f", amt[l10])
    famt[l100] <- sprintf(" %.0f", amt[l100])
    famt[b100] <- sprintf("%.0f", amt[b100])

    paste0(famt, " ", szs$unit)
  }

  structure(
    list(
      .internal = environment(),
      pretty_bytes = pretty_bytes,
      compute_bytes = compute_bytes
    ),
    class = c("standalone_bytes", "standalone")
  )
})
