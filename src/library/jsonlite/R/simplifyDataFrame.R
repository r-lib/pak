simplifyDataFrame <- function(recordlist, columns, flatten, simplifyMatrix) {
  # no records at all
  if (!length(recordlist)) {
    if (!missing(columns)) {
      return(as.data.frame(matrix(ncol = length(columns), nrow = 0, dimnames = list(NULL, columns))))
    } else {
      return(data.frame())
    }
  }

  # only empty records and unknown columns
  if (!any(vapply(recordlist, length, integer(1), USE.NAMES = FALSE)) && missing(columns)) {
    return(data.frame(matrix(nrow = length(recordlist), ncol = 0)))
  }

  # find columns if not specified
  if (missing(columns)) {
    columns <- unique(unlist(lapply(recordlist, names), recursive = FALSE, use.names = FALSE))
  }

  # Convert row lists to column lists. This is the heavy lifting
  # columnlist <- lapply(columns, function(x) lapply(recordlist, "[[", x))
  # Now slighlty optimized
  columnlist <- transpose_list(recordlist, columns)

  # simplify vectors and nested data frames
  columnlist <- lapply(columnlist, simplify, simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = FALSE, simplifySubMatrix = simplifyMatrix, flatten = flatten)

  # check that all elements have equal length
  columnlengths <- unlist(vapply(
    columnlist,
    function(z) {
      ifelse(length(dim(z)) > 1, nrow(z), length(z))
    },
    integer(1)
  ))
  n <- unique(columnlengths)
  if (length(n) > 1) {
    stop("Elements not of equal length: ", paste(columnlengths, collapse = " "))
  }

  # add the column names before flattening
  names(columnlist) <- columns

  # flatten nested data frames
  if (isTRUE(flatten)) {
    dfcolumns <- vapply(columnlist, is.data.frame, logical(1))
    if (any(dfcolumns)) {
      columnlist <- c(columnlist[!dfcolumns], do.call(c, columnlist[dfcolumns]))
    }
  }

  # make into data frame
  class(columnlist) <- "data.frame"

  # set row names
  if ("_row" %in% names(columnlist)) {
    rn <- columnlist[["_row"]]
    columnlist["_row"] <- NULL

    # row.names() casts double to character which is undesired.
    if (is.double(rn)) {
      rn <- as.integer(rn)
    }

    # Replace missing values with numbers
    rn_na <- is.na(rn)
    if (sum(rn_na) > 0) {
      rn[rn_na] <- paste0("NA_", seq_len(sum(rn_na)))
    }

    # data frames MUST have row names
    if (any(duplicated(rn))) {
      warning('Duplicate names in "_row" field. Data frames must have unique row names.', call. = FALSE)
      if (is.character(rn)) {
        row.names(columnlist) <- make.unique(rn)
      } else {
        row.names(columnlist) <- seq_len(n)
      }
    } else {
      row.names(columnlist) <- rn
    }
  } else {
    row.names(columnlist) <- seq_len(n)
  }

  return(columnlist)
}
