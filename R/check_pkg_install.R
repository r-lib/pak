#' Check for package installation and install
#'
#' Determine if the one or more packages are installed in the library path and, 
#' if not, prompt the user to install them. If all packages are installed, the
#' function silently returns the package list invisibly. 
#' @inheritParams pkg_install
#' @importFrom glue glue_collapse
#' @importFrom utils menu
#' @export

check_pkg_install <-
  function(pkg,
           lib = .libPaths()[[1L]],
           upgrade = FALSE,
           num_workers = 1L,
           ask = FALSE) {
    installed <- rep(TRUE, length(pkg))
    for (i in seq(along = pkg))
      installed[i] <- nrow(pkg_status(pkg[i])) > 0
    
    if (any(!installed)) {
      pkList <- glue::glue_collapse(pkg[!installed], sep = ", ", last = " and ")
      msg <- paste(
        sum(!installed),
        ifelse(sum(!installed) > 1, " packages are", " package is"),
        " required and",
        ifelse(sum(!installed) > 1, " are", " is"),
        " not installed. (",
        pkList,
        "). Would you like to try to install",
        ifelse(sum(!installed) > 1, " them", " it"),
        " from CRAN now?",
        sep = ""
      )
      cat(msg)
      if (interactive()) {
        installChoice <- menu(c("yes", "no"))
        if (installChoice == 1) {
          pkg_install(
            pkg[!installed],
            lib = lib,
            upgrade = upgrade,
            num_workers = num_workers,
            ask = ask
          )
        } else
          stop("Required package is missing", call. = FALSE)
      } else
        stop("Required package is missing", call. = FALSE)
    }
    invisible(pkg)
  }
