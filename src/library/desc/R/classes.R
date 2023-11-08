
collate_fields <- c(
  main = "Collate",
  windows = "Collate.windows",
  unix = "Collate.unix"
)

field_classes <- list(

  Package = "Package",
  Version = "Version",
  License = "License",
  Description = "Description",
  Title = "Title",
  Maintainer = "Maintainer",
  AuthorsAtR = "Authors@R",

  DependencyList = c("Imports", "Suggests", "Depends", "Enhances",
    "LinkingTo"),
  PackageList = c("VignetteBuilder", "RdMacros"),
  Remotes = "Remotes",
  RepoList = "Additional_repositories",
  URL = "BugReports",
  URLList = "URL",
  Priority = "Priority",
  Collate = unname(collate_fields),
  Logical = c("LazyData", "KeepSource", "ByteCompile", "ZipData", "Biarch",
    "BuildVignettes", "NeedsCompilation", "License_is_FOSS",
    "License_restricts_use", "BuildKeepEmpty", "BuildManual",
    "BuildResaveData", "LazyLoad"),
  Encoding = "Encoding",
  OSType = "OS_type",
  Type = "Type",
  Classification = c("Classification/ACM", "Classification/ACM-2012",
    "Classification/JEL", "Classification/MSC", "Classification/MSC-2010"),
  Language = "Language",
  Date = "Date",
  Compression = c("LazyDataCompression", "SysDataCompression"),
  Repository = "Repository",

  FreeForm = c("Author", "SystemRequirements",
    "Archs", "Contact", "Copyright", "MailingList", "Note", "Path",
    "LastChangedDate", "LastChangedRevision", "Revision", "RcmdrModels",
    "RcppModules", "Roxygen", "Acknowledgements", "Acknowledgments",
    "biocViews"),

  AddedByRCMD = c("Built", "Packaged", "MD5sum", "Date/Publication")
)

field_classes$FreeForm <- c(
  field_classes$FreeForm,
  paste0(unlist(field_classes), "Note")
)

create_fields <- function(keys, values) {
  # Need to add names explicitly, because mapply() might drop them
  # https://bugs.r-project.org/show_bug.cgi?id=18201
  structure(
    mapply(keys, values, SIMPLIFY = FALSE, FUN = create_field),
    names = keys
  )
}

create_field <- function(key, value) {
  f <- structure(list(key = key, value = value), class = "DescriptionField")
  if (key %in% unlist(field_classes)) {
    cl <- paste0("Description", find_field_class(key))
    class(f) <- c(cl, class(f))
  }
  f
}

find_field_class <- function(k) {
  names(which(vapply(field_classes, `%in%`, logical(1), x = k)))
}
