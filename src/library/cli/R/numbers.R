format_num <- local({
  pretty_num <- function(number, style = c("default", "nopad", "6")) {
    style <- switch(
      match.arg(style),
      "default" = pretty_num_default,
      "nopad" = pretty_num_nopad,
      "6" = pretty_num_6
    )

    style(number)
  }

  compute_num <- function(number, smallest_prefix = "y") {
    prefixes0 <- c(
      "y",
      "z",
      "a",
      "f",
      "p",
      "n",
      "u",
      "m",
      "",
      "k",
      "M",
      "G",
      "T",
      "P",
      "E",
      "Z",
      "Y"
    )
    zeroshif0 <- 9L

    stopifnot(
      is.numeric(number),
      is.character(smallest_prefix),
      length(smallest_prefix) == 1,
      !is.na(smallest_prefix),
      smallest_prefix %in% prefixes0
    )

    limits <- c(999950 * 1000^(seq_len(length(prefixes0)) - (zeroshif0 + 1L)))
    nrow <- length(limits)
    low <- match(smallest_prefix, prefixes0)
    zeroshift <- zeroshif0 + 1L - low
    prefixes <- prefixes0[low:length(prefixes0)]
    limits <- limits[low:nrow]
    nrow <- nrow - low + 1

    neg <- number < 0 & !is.na(number)
    number <- abs(number)
    mat <- matrix(
      rep(number, each = nrow),
      nrow = nrow,
      ncol = length(number)
    )
    mat2 <- matrix(mat < limits, nrow = nrow, ncol = length(number))
    exponent <- nrow - colSums(mat2) - (zeroshift - 1L)
    in_range <- function(exponent) {
      max(
        min(exponent, nrow - zeroshift, na.rm = FALSE),
        1L - zeroshift,
        na.rm = TRUE
      )
    }
    if (length(exponent)) {
      exponent <- sapply(exponent, in_range)
    }
    res <- number / 1000^exponent
    prefix <- prefixes[exponent + zeroshift]

    ## Zero number
    res[number == 0] <- 0
    prefix[number == 0] <- prefixes[zeroshift]

    ## NA and NaN number
    res[is.na(number)] <- NA_real_
    res[is.nan(number)] <- NaN
    prefix[is.na(number)] <- "" # prefixes0[low] is meaningless    # Includes NaN as well

    data.frame(
      stringsAsFactors = FALSE,
      amount = res,
      prefix = prefix,
      negative = neg
    )
  }

  pretty_num_default <- function(number) {
    szs <- compute_num(number)
    amt <- szs$amount
    sep <- " "

    ## String. For fractions we always show two fraction digits
    res <- character(length(amt))
    int <- is.na(amt) | abs(amt - as.integer(amt)) <= .Machine$double.eps
    res[int] <- format(
      ifelse(szs$negative[int], -1, 1) * amt[int],
      scientific = FALSE
    )
    res[!int] <- sprintf("%.2f", ifelse(szs$negative[!int], -1, 1) * amt[!int])

    format(paste(res, szs$prefix, sep = sep), justify = "right")
  }

  pretty_num_nopad <- function(number) {
    sub("^\\s+", "", pretty_num_default(number))
  }

  pretty_num_6 <- function(number) {
    szs <- compute_num(number, smallest_prefix = "y")
    amt <- round(szs$amount, 2)
    sep <- " "

    na <- is.na(amt)
    nan <- is.nan(amt)
    neg <- !na & !nan & szs$negative
    l10p <- !na & !nan & !neg & amt < 10
    l100p <- !na & !nan & !neg & amt >= 10 & amt < 100
    b100p <- !na & !nan & !neg & amt >= 100
    l10n <- !na & !nan & neg & amt < 10
    l100n <- !na & !nan & neg & amt >= 10 & amt < 100
    b100n <- !na & !nan & neg & amt >= 100

    famt <- character(length(amt))
    famt[na] <- "  NA"
    famt[nan] <- " NaN"
    famt[l10p] <- sprintf("%.2f", amt[l10p])
    famt[l100p] <- sprintf("%.1f", amt[l100p])
    famt[b100p] <- sprintf(" %.0f", amt[b100p])
    famt[l10n] <- sprintf("-%.1f", amt[l10n])
    famt[l100n] <- sprintf(" -%.0f", amt[l100n])
    famt[b100n] <- sprintf("-%.0f", amt[b100n])

    sub(" $", "  ", paste0(famt, sep, szs$prefix))
  }

  structure(
    list(
      .internal = environment(),
      pretty_num = pretty_num,
      compute_num = compute_num
    ),
    class = c("standalone_num", "standalone")
  )
})
