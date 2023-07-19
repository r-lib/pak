DEFAULT_RSPM_REPO_ID <-  "1" # cran
DEFAULT_RSPM <-  "https://packagemanager.rstudio.com"
DEFAULT_REQ_URL_EXT <- ""

#' Query system requirements
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' Note that these functions are now *deprecated*, in favor of
#' [pkg_sysreqs()] and the `sysreqs_*`  functions, which are more
#' powerful, as they work for all package sources (packages at Github,
#' GitLab, URLs, etc.) and they have more detailed output.
#'
#' Instead of
#' ```r
#' pak::pkg_system_requirement("curl")
#' ```
#' call
#' ```r
#' pak::pkg_sysreqs("curl")$install_scripts
#' ```
#' and the equivalent of
#' ```r
#' pak::local_system_requirements()
#' ```
#' is
#' ```r
#' pak::pkg_sysreqs("local::.", dependencies = TRUE)$install_script
#' ```
#'
#' @details
#' Returns a character vector of commands to run that will install system
#' requirements for the queried operating system.
#'
#' `local_system_requirements()` queries system requirements for a dev package
#' (and its dependencies) given its `root` path.
#'
#' @inheritParams local_install
#' @param os,os_release The operating system and operating system release
#'   version, e.g. "ubuntu", "debian", "centos", "redhat". See
#'   <https://github.com/rstudio/r-system-requirements#operating-systems> for
#'   all full list of supported operating systems.
#'
#'   If `NULL`, the default, these will be looked up using [distro::distro()].
#' @param execute,sudo If `execute` is `TRUE`, pak will execute the system
#'   commands (if any). If `sudo` is `TRUE`, pak will prepend the commands with
#'   [sudo](https://en.wikipedia.org/wiki/Sudo).
#' @param echo If `echo` is `TRUE` and `execute` is `TRUE`, echo the command output.
#' @return A character vector of commands needed to install the system
#'   requirements for the package.
#' @export
#' @examplesIf FALSE
#' local_system_requirements("ubuntu", "20.04")
local_system_requirements <- function(os = NULL, os_release = NULL, root = ".", execute = FALSE, sudo = execute, echo = FALSE) {

  once_per_session(message(
    "`pak::local_system_requirements()` is deprecated since pak 0.6.0.\n",
    "Please use `pak::pkg_sysreqs()` instead.",
    call. = FALSE
  ))

  res <- remote(
    function(...) asNamespace("pak")$system_requirements_internal(...),
    list(os = os, os_release = os_release, root = root, package = NULL, execute = execute, sudo = sudo, echo = echo))
  if (execute) invisible(res) else res
}

#' @details
#' `pkg_system_requirements()` queries system requirements for existing packages
#' (and their dependencies).
#' @param package Package names to lookup system requirements for.
#' @rdname local_system_requirements
#' @export
#' @examplesIf FALSE
#' pkg_system_requirements("pak", "ubuntu", "20.04")
#' pkg_system_requirements("pak", "redhat", "7")
#' pkg_system_requirements("config", "ubuntu", "20.04") # no sys reqs
#' pkg_system_requirements("curl", "ubuntu", "20.04")
#' pkg_system_requirements("git2r", "ubuntu", "20.04")
#' pkg_system_requirements(c("config", "git2r", "curl"), "ubuntu", "20.04")
#' # queried packages must exist
#' pkg_system_requirements("iDontExist", "ubuntu", "20.04")
#' pkg_system_requirements(c("curl", "iDontExist"), "ubuntu", "20.04")
pkg_system_requirements <- function(package, os = NULL, os_release = NULL, execute = FALSE, sudo = execute, echo = FALSE) {

  once_per_session(message(
    "`pak::pkg_system_requirements()` is deprecated since pak 0.6.0.\n",
    "Please use `pak::pkg_sysreqs()` instead.",
    call. = FALSE
  ))

  res <- remote(
    function(...) asNamespace("pak")$system_requirements_internal(...),
    list(os = os, os_release = os_release, root = NULL, package = package, execute = execute, sudo = sudo, echo = echo))
  if (execute) invisible(res) else res
}

filter_repos <- function(rspm_repo_url) {
  res <- curl::curl_fetch_memory(rspm_repo_url)
  data <-
    jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
  repos <- list()
  for (i in 1:length(data)) {
    if ((data[[i]]$type == "R") && !data[[i]]$hidden) {
      packages=as.data.frame(
        available.packages(
          repos=paste(DEFAULT_RSPM,data[[i]]$name,"latest",sep="/")
        )
      )$Package
      message(paste(DEFAULT_RSPM,data[[i]]$name,"latest",sep="/"))
      repos[[length(repos)+1]]=list(id=data[[i]]$id, packages=packages)
    }
    if ((data[[i]]$type == "Bioconductor") && (data[[i]]$name != "all") && !data[[i]]$hidden) {  
      Sys.setenv(BioC_mirror=paste(DEFAULT_RSPM,data[[i]]$name,sep="/"))
      bioc_repos<-BiocManager::repositories()
      bioc_repos[names(bioc_repos)[grep("BioC",names(bioc_repos),invert=TRUE)]]<-""
      packages=as.data.frame(available.packages(repos=bioc_repos))$Package
      repos[[length(repos)+1]]=list(id=data[[i]]$id, packages=packages)
    }
    
    
  }
  repos
}


find_package_deps <-  function(rspm_repo_url,
                               package,
                               os,
                               os_release,
                               repos) {
  appendstr <- ""
  repo_ctr = 1
  deps<-c()
  deps_found <- FALSE
  message(sprintf("%s: length_repos %s", repo_ctr, length(repos)))
  if (!is.null(package)) {
    while (repo_ctr<=length(repos)) {
    packages_in_repo <-
      repos[[repo_ctr]]$packages[repos[[repo_ctr]]$packages %in% package]
    message(sprintf("%s: length %s", repo_ctr, length(packages_in_repo)))
    message(repo_ctr, packages_in_repo)
    
    if (length(packages_in_repo) == 0) {
      message(paste("none of ", package, "found in repo", repo_ctr))
      repo_ctr <- repo_ctr + 1
    } else {
      deps_found <- FALSE
      while (!deps_found) {
        message(sprintf(">>> %s", repo_ctr))
        req_url <- sprintf(
          "%s/%s/sysreqs?all=false&pkgname=%s&distribution=%s&release=%s%s",
          rspm_repo_url,
          repos[[repo_ctr]]$id,
          paste(packages_in_repo, collapse = "&pkgname="),
          os,
          os_release,
          appendstr
        )
        message(sprintf("%s: %s", repo_ctr, req_url))
        res <- curl::curl_fetch_memory(req_url)
        message(sprintf("A %s: %s", repo_ctr, res$status_code))
        if (res$status_code == "404" &&
            length(res$content) == 0) {
          repo_ctr = repo_ctr + 1
          message(sprintf("B %s: %s", repo_ctr, res$status_code))
        } else {
          data <-
            jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
          message(sprintf("C %s: %s", repo_ctr, data$error))
          if (!is.null(data$error) &&
              data$error == sprintf("Could not locate package '%s'", "test")) {
            repo_ctr <- repo_ctr + 1
          }
          appendstr <- ""
          if (!is.null(data$error) &&
              data$error == "Bioconductor version not provided") {
            if (!exists("biocvers")) {
              biocvers <- BiocManager::version()
            }
            appendstr = sprintf("&bioc_version=%s", biocvers)
          }
          if (res$status_code == "200") {
            deps_found <- TRUE
            repo_ctr <- repo_ctr + 1
            data
            deps=c(deps,data$requirements)
            message(rawToChar(res$content))
          }
          
        }
        
      }
      
    }
    
    }
    deps
  }
}


system_requirements_internal <- function(os, os_release, root, package, execute, sudo, echo) {
  if (is.null(os) || is.null(os_release)) {
    d <- distro::distro()
    os <- os %||% d$id
    os_release <- os_release %||% d$short_version
  }
  
  

  os_versions <- supported_os_versions()

  os <- match.arg(os, names(os_versions))

  os_release <- match.arg(os_release, os_versions[[os]])

  rspm <- Sys.getenv("RSPM_ROOT", DEFAULT_RSPM)
  rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", DEFAULT_RSPM_REPO_ID)
  rspm_repo_url <- sprintf("%s/__api__/repos", rspm)


  if (!is.null(package)) {
    
    data<-find_package_deps(rspm_repo_url,package,os,os_release,filter_repos(rspm_repo_url))

    pre_install <- unique(unlist(c(data[["pre_install"]], lapply(data, `[[`, c("requirements", "pre_install")))))
    install_scripts <- unique(unlist(c(data[["install_scripts"]], lapply(data, `[[`, c("requirements", "install_scripts")))))
  } else {
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
    if (!is.null(data$error)) {
      stop(data$error)
    }

    pre_install <- unique(unlist(c(data[["pre_install"]], lapply(data[["dependencies"]], `[[`, "pre_install"))))
    install_scripts <- unique(unlist(c(data[["install_scripts"]], lapply(data[["dependencies"]], `[[`, "install_scripts"))))
  }

  commands <- as.character(c(pre_install, simplify_install(install_scripts)))
  if (echo) {
    callback <- function(x, ...) cli::cli_verbatim(sub("[\r\n]+$", "", x))
  } else {
    callback <- function(x, ...) invisible()
  }

  if (execute) {
    for (cmd in commands) {
      if (sudo) {
        cmd <- paste("sudo", cmd)
      }
      cli::cli_alert_info("Executing {.code {cmd}}")

      processx::run("sh", c("-c", cmd), stdout_callback = callback, stderr_to_stdout = TRUE)
    }
  }

  commands
}

# Adapted from https://github.com/rstudio/r-system-requirements/blob/master/systems.json
# OSs commented out are not currently supported by the API
supported_os_versions <- function() {
  list(
    #"debian" = c("8", "9"),
    "ubuntu" = c("14.04", "16.04", "18.04", "20.04", "22.04"),
    "centos" = c("6", "7", "8"),
    "redhat" = c("6", "7", "8"),
    "opensuse" = c("42.3", "15.0"),
    "sle" = c("12.3", "15.0")
    #"windows" = c("")
  )
}

# Grouping multiple `apt-get install -y` calls in install scripts.
# This should be done by the server, but isn't (yet).
simplify_install <- function(x) {
  rx <- "^apt-get install -y"
  ry <- "^yum install -y"
  if (length(x) == 0 ||
      (!all(grepl(rx, x)) && !all(grepl(ry, x)))) {
    return(x)
  }
  
  if (all(grepl(rx, x))) {
    return(paste0("apt-get install -y ",
           paste(gsub(rx, "", x), collapse = " ")))
  }
  
  if (all(grepl(ry, x))) {
    return(paste0("yum install -y ",
           paste(gsub(ry, "", x), collapse = " ")))
  }
}
