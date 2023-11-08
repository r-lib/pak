
chk <- function(msg, check) {
  if (check) TRUE else msg
}

chks <- function(..., x, warn) {
  results <- list(...)
  results <- unlist(setdiff(results, TRUE))
  results <- if (length(results) == 0) TRUE else results

  if (! identical(results, TRUE) && warn) {
    warning(
      call. = FALSE,
      "'", x$key, "'",
      paste0(
        if (length(results) == 1) " " else  "\n    * ",
        strwrap(results, indent = 0, exdent = 6)
      )
    )
  }

  results
}


#' Syntactical check of a DESCRIPTION field
#'
#' @param x The field.
#' @param warn Whether to generate a warning if the syntax check fails.
#' @param ... Additional arguments, they might be used in the future.
#' @return `TRUE` if the field is syntactically correct,
#'   otherwise a character vector, containing one or multiple
#'   error messages.
#'
#' @export

check_field <- function(x, warn = FALSE, ...)
  UseMethod("check_field")

#' @export
#' @method check_field DescriptionField

check_field.DescriptionField <- function(x, warn = FALSE, ...) TRUE

##' @export
##' @method check_field DescriptionPackage

check_field.DescriptionPackage <- function(x, warn = FALSE, R = FALSE, ...) {

  ## In Depends, we can depend on certain 'R' versions
  if (R && x$value == "R") return(TRUE)

  chks(
    x = x, warn = warn,
    chk("must only contain ASCII letters, numbers, dots",
        grepl("^[a-zA-Z0-9\\.]*$", x$value)),
    chk("must be at least two characters long",
        nchar(x$value) >= 2),
    chk("must start with a letter",
        grepl("^[a-zA-Z]", x$value)),
    chk("must not end with a dot",
        !grepl("\\.$", x$value))
  )
}

valid_packagename_regexp <- "[[:alpha:]][[:alnum:].]*[[:alnum:]]"
valid_version_regexp <- "[0-9]+[-\\.][0-9]+([-\\.][0-9]+)*"
valid_package_archive_name <- paste0(
  "^",
  valid_packagename_regexp,
  "_",
  valid_version_regexp,
  "(.*)?",
  "(\\.tar\\.gz|\\.tgz|\\.zip)",
  "$"
)

##' @export
##' @method check_field DescriptionVersion

check_field.DescriptionVersion <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk(paste("must be a sequence of at least two (usually three)",
              " non-negative integers separated by a single dot or dash",
              " character"),
        grepl(paste0("^", valid_version_regexp, "$"), x$value))
  )
}

## TODO: It also must be a license R CMD check recognizes
##
##' @export
##' @method check_field DescriptionLicense

check_field.DescriptionLicense <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must contain only ASCII characters",
        is_ascii(x$value)),
    chk("must not be empty",
        str_trim(x$value) != "")
  )
}

##' @export
##' @method check_field DescriptionDescription

check_field.DescriptionDescription <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must not be empty",
        str_trim(x$value) != ""),
    chk("must contain one or more complete sentences",
        grepl("[.!?]['\")]?$", str_trim(x$value))),
    chk("must not start with 'The package', 'This Package, 'A package'",
        !grepl("^(The|This|A|In this|In the) package", x$value)),
    chk("must start with a capital letter",
        grepl("^['\"]?[[:upper:]]", x$value))
  )
}

##' @export
##' @method check_field DescriptionTitle

check_field.DescriptionTitle <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must not be empty",
         str_trim(x$value) != ""),
    chk("must not end with a period",
        !grepl("[.]$", str_trim(x$value)) ||
        grepl("[[:space:]][.][.][.]|et[[:space:]]al[.]", str_trim(x$value)))
  )
}

##' @export
##' @method check_field DescriptionMaintainer

check_field.DescriptionMaintainer <- function(x, warn = FALSE, ...) {

  re_maint <- paste0(
    "^[[:space:]]*(.*<",
    RFC_2822_email_regexp,
    ">|ORPHANED)[[:space:]]*$"
  )

  chks(
    x = x, warn = warn,
    chk("must not be empty",
        str_trim(x$value) != ""),
    chk("must contain an email address",
        grepl(re_maint, x$value))
  )
}

## TODO
##' @export
##' @method check_field DescriptionAuthorsAtR

check_field.DescriptionAuthorsAtR <- function(x, warn = FALSE, ...) {
  TRUE
}

##' @export
##' @method check_field DescriptionDependencyList

check_field.DescriptionDependencyList <- function(x, warn = FALSE, ...) {

  deps <- parse_deps(x$key, x$value)

  is_package_list <- function(xx) {
    p <- lapply(xx, function(pc)
      check_field.DescriptionPackage(
        list(key = "Package", value = pc),
        R = x$key[1] == "Depends"
      )
    )
    all_true(p)
  }

  is_version_req <- function(x) {

    x <- str_trim(x)
    if (x == "*") return(TRUE)

    re <- paste0(
      "^(<=|>=|<|>|==|!=)\\s*",
      valid_version_regexp,
      "$"
    )
    grepl(re, x)
  }

  is_version_req_list <- function(x) {
    all_true(vapply(x, is_version_req, TRUE))
  }

  chks(
    x = x, warn = warn,
    chk("must contain valid package names",
        is_package_list(deps$package)),
    chk("must contain valid version requirements",
        is_version_req_list(deps$version))
  )
}

##' @export
##' @method check_field DescriptionRemotes

check_field.DescriptionRemotes <- function(x, warn = FALSE, ...) {

  is_remote <- function(x) {
    xx <- str_trim(strsplit(x, ",", fixed = TRUE)[[1]])
    p <- grepl("^[^[:space:]]+$", xx)
    all_true(p)
  }

  chks(
    x = x, warn = warn,
    chk("must be a comma separated list of remotes",
        is_remote(x$value))
  )
}

##' @export
##' @method check_field DescriptionPackageList

check_field.DescriptionPackageList <- function(x, warn = FALSE, ...) {

  is_package_list <- function(x) {
    xx <- str_trim(strsplit(x, ",", fixed = TRUE)[[1]])
    p <- lapply(xx, function(pc)
      check_field.DescriptionPackage(list(key = "Package", value = pc)))
    all_true(p)
  }

  chks(
    x = x, warn = warn,
    chk("must be a comma separated list of package names",
        is_package_list(x$value))
  )
}

##' @export
##' @method check_field DescriptionRepoList

check_field.DescriptionRepoList <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be a comma separated list repository URLs",
        is_url_list(x$value))
  )
}

##' @export
##' @method check_field DescriptionURL

check_field.DescriptionURL <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be a http, https or ftp URL",
        is_url(x$value))
  )
}

##' @export
##' @method check_field DescriptionURLList

check_field.DescriptionURLList <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be a comma separated list of http, https or ftp URLs",
        is_url_list(x$value))
  )
}

##' @export
##' @method check_field DescriptionPriority

check_field.DescriptionPriority <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be one of 'base', 'recommended' or 'defunct-base'",
        str_trim(x$value) %in% c("base", "recommended", "defunct-base"))
  )
}

##' @export
##' @method check_field DescriptionCollate

check_field.DescriptionCollate <- function(x, warn = FALSE, ...) {

  coll <- tolower(parse_collate(x$value))

  chks(
    x = x, warn = warn,
    chk("must contain a list of .R files",
        all(grepl("[.]r$", coll)))
  )
}

##' @export
##' @method check_field DescriptionLogical

check_field.DescriptionLogical <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be one of 'true', 'false', 'yes' or 'no' (case insensitive)",
        str_trim(tolower(x$value)) %in% c("true", "false", "yes", "no"))
  )
}

##' @export
##' @method check_field DescriptionEncoding

check_field.DescriptionEncoding <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be one of 'latin1', 'latin2' and 'UTF-8'",
        x$value %in% c("latin1", "latin2", "UTF-8"))
  )
}

##' @export
##' @method check_field DescriptionOSType

check_field.DescriptionOSType <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be one of 'unix' and 'windows'",
        x$value %in% c("unix", "windows"))
  )
}

##' @export
##' @method check_field DescriptionType

check_field.DescriptionType <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be either 'Package' or 'Translation'",
        x$value %in% c("Package", "Translation"))
  )
}

##' @export
##' @method check_field DescriptionClassification

check_field.DescriptionClassification <- function(x, warn = FALSE, ...) {
  TRUE
}

##' @export
##' @method check_field DescriptionLanguage

check_field.DescriptionLanguage <- function(x, warn = FALSE, ...) {

  is_language_list <- function(x) {
    x <- str_trim(strsplit(x, ",", fixed = TRUE)[[1]])
    all(grepl("^[a-z][a-z][a-z]?(-[A-Z]+)?$", x))
  }

  chks(
    x = x, warn = warn,
    chk("must be a list of IETF language codes defined by RFC 5646",
        is_language_list(x$value))
  )
}

##' @export
##' @method check_field DescriptionDate

check_field.DescriptionDate <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk(
      paste0(
        "must be an ISO date: yyyy-mm-dd, but it is actually better\n",
        "to leave this field out completely. It is not required."),
      grepl("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$", x$value)
    )
  )
}

##' @export
##' @method check_field DescriptionCompression

check_field.DescriptionCompression <- function(x, warn = FALSE, ...) {

  chks(
    x = x, warn = warn,
    chk("must be one of 'bzip2', 'xz', 'gzip'",
        x$value %in% c("bzip2", "xz", "gzip"))
  )
}

##' @export
##' @method check_field DescriptionRepository

check_field.DescriptionRepository <- function(x, warn = FALSE, ...) {
  TRUE
}

##' @export
##' @method check_field DescriptionFreeForm

check_field.DescriptionFreeForm <- function(x, warn = FALSE, ...) {
  TRUE
}

##' @export
##' @method check_field DescriptionAddedByRCMD

check_field.DescriptionAddedByRCMD <- function(x, warn = FALSE, ...) {
  TRUE
}
