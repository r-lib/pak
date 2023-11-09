
#' DESCRIPTION fields that denote package dependencies
#'
#' Currently it has the following ones: Imports, Depends,
#' Suggests, Enhances and LinkingTo. See the _Writing R Extensions_
#' manual for when to use which.
#'
#' @family field types
#' @export

dep_types <- c("Imports", "Depends", "Suggests", "Enhances", "LinkingTo")

standard_fields <- c(
  "Additional_repositories",
  "Author",
  "Authors@R",
  "Biarch",
  "BugReports",
  "BuildKeepEmpty",
  "BuildManual",
  "BuildResaveData",
  "BuildVignettes",
  "Built",
  "ByteCompile",
  "Classification/ACM",
  "Classification/ACM-2012",
  "Classification/JEL",
  "Classification/MSC",
  "Classification/MSC-2010",
  "Collate",
  "Collate.unix",
  "Collate.windows",
  "Contact",
  "Copyright",
  "Date",
  "Depends",
  "Description",
  "Encoding",
  "Enhances",
  "Imports",
  "KeepSource",
  "Language",
  "LazyData",
  "LazyDataCompression",
  "LazyLoad",
  "License",
  "LinkingTo",
  "MailingList",
  "Maintainer",
  "Note",
  "OS_type",
  "Package",
  "Packaged",
  "Priority",
  "Suggests",
  "SysDataCompression",
  "SystemRequirements",
  "Title",
  "Type",
  "URL",
  "Version",
  "VignetteBuilder",
  "ZipData",
  "Repository",
  "Path",
  "Date/Publication",
  "LastChangedDate",
  "LastChangedRevision",
  "Revision",
  "RcmdrModels",
  "RcppModules",
  "Roxygen",
  "Acknowledgements",
  "Acknowledgments", # USA/Canadian usage.
  "biocViews"
)

#' A list of DESCRIPTION fields that are valid according to the CRAN checks
#'
#' @family field types
#' @export

cran_valid_fields <- c(
  standard_fields,
  "^(?:X-CRAN|Repository/R-Forge)",
  paste0(standard_fields, "Note")
)

#' The DESCRIPTION fields that are supposed to be in plain ASCII encoding
#'
#' @family field types
#' @export

cran_ascii_fields <- c(
  "Package",
  "Version",
  "Priority",
  "Depends",
  "Imports",
  "LinkingTo",
  "Suggests",
  "Enhances",
  "License",
  "License_is_FOSS",
  "License_restricts_use",
  "OS_type",
  "Archs",
  "MD5sum",
  "NeedsCompilation",
  "Encoding"
)
