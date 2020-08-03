DEFAULT_RSPM_REPO_ID <-  "1" # cran
DEFAULT_RSPM <-  "https://packagemanager.rstudio.com"

#' Query the system requirements for a dev package (and its dependencies)
#'
#' Returns a character vector of commands to run that will install system
#' requirements for the queried operating system.
#'
#' @inheritParams local_install
#' @param os,os_release The operating system and operating system release version, see
#'   <https://github.com/rstudio/r-system-requirements#operating-systems> for the
#'   list of supported operating systems.
#' @return A character vector of commands needed to install the system requirements for the package.
#' @export
local_system_requirements <- function(os, os_release, root = ".") {
  os_versions <- supported_os_versions()

  os <- match.arg(os, names(os_versions))

  os_release <- match.arg(os_release, os_versions[[os]])

  rspm <- Sys.getenv("RSPM_ROOT", DEFAULT_RSPM)
  rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", DEFAULT_RSPM_REPO_ID)
  rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)

  desc_file <- normalizePath(file.path(root, "DESCRIPTION"), mustWork = FALSE)
  if (!file.exists(desc_file)) {
    stop("`", root, "` must contain a package.", call. = FALSE)
  }

  req_url <- sprintf(
    "%s/sysreqs?distribution=%s&release=%s&suggests=true",
    rspm_repo_url,
    os,
    os_release
  )

  h <- curl::new_handle()

  desc_size <- file.size(desc_file)
  desc_data <- readBin(desc_file, "raw", desc_size)

  curl::handle_setheaders(h,
    customrequest = "POST",
    "content-type" = "text/plain"
  )

  curl::handle_setopt(h,
    postfieldsize = desc_size,
    postfields = desc_data
  )

  res <- curl::curl_fetch_memory(req_url, h)

  data <- jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)

  pre_install <- unique(unlist(c(data[["pre_install"]], lapply(data[["dependencies"]], `[[`, "pre_install"))))

  install_scripts <- unique(unlist(c(data[["install_scripts"]], lapply(data[["dependencies"]], `[[`, "install_scripts"))))

  as.character(c(pre_install, install_scripts))
}

# Adapted from https://github.com/rstudio/r-system-requirements/blob/master/systems.json
# OSs commented out are not currently supported by the API
supported_os_versions <- function() {
  list(
    #"debian" = c("8", "9"),
    "ubuntu" = c("14.04", "16.04", "18.04", "20.04"),
    "centos" = c("6", "7", "8"),
    "redhat" = c("6", "7", "8"),
    "opensuse" = c("42.3", "15.0"),
    "sle" = c("12.3", "15.0")
    #"windows" = c("")
  )
}
