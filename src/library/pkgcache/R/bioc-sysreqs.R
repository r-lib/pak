
list_bioc_repos <- function() {
  url <- "https://git.bioconductor.org/"
  resp <- curl::curl_fetch_memory(url)
  if (resp$status_code != 200) {
    stop("Cannot get ", url, ", status code ", resp$status_code)
  }
  con <- NULL
  on.exit(tryCatch(close(con), error = function(e) e), add = TRUE)
  txt <- readLines(con <- rawConnection(resp$content))
  txt <- grep("\tpackages/[^.]", txt, value = TRUE)
  pkgs <- sub("^.*\tpackages/", "", txt)

  pkgs[order(tolower(pkgs))]
}

get_bioc_sysreqs <- function(pkg, ref = "HEAD") {
  synchronise(async_get_bioc_sysreqs(pkg, ref))
}

async_get_bioc_sysreqs <- function(pkg, ref = "HEAD") {
  url <- sprintf("https://raw.githubusercontent.com/bioc/%s/%s/DESCRIPTION", pkg, ref)
  tmp <- tempfile("pkgcache-bioc-")
  on.exit(unlink(tmp), add = TRUE)

  http_get(url)$
    then(http_stop_for_status)$
    catch(async_http_404 = function(err) list(content = raw()))$
    then(function(res) {
      on.exit(close(con), add = TRUE)
      desc <- read.dcf(con <- rawConnection(res$content))
      if ("SystemRequirements" %in% colnames(desc)) {
        unname(desc[, "SystemRequirements"])
      } else {
        NA_character_
      }
    })
}

get_all_bioc_sysreqs <- function(ref = "HEAD") {
  start_at <- Sys.time()

  pkgs <- list_bioc_repos()

  done <- c(0, length(pkgs))

  prog <- function() {
    cat(
      "\r[", paste(format(done), collapse = "/"), "]", sep = "",
      " -- ", format_time$pretty_dt(Sys.time() - start_at)
    )
  }

  prog()
  ret <- synchronise(async_map(
    pkgs,
    function(pkg) {
      force(pkg)
      async_get_bioc_sysreqs(pkg, ref = ref)$
        catch(error = function(e) {
          message(
            "\r", pkg, ":                         \n",
            conditionMessage(e)
          )
          NA_character_
        })$
        then(function(x) {
          done[1] <<- done[1] + 1L
          prog()
          x
        })
    }
  ))
  sq <- data.frame(
    stringsAsFactors = FALSE,
    Package = pkgs,
    SystemRequirements = unname(unlist(ret))
  )
  sq <- sq[!is.na(sq$SystemRequirements), ]
  write.dcf(sq, "inst/bioc-sysreqs.dcf")
  system("gzip inst/bioc-sysreqs.dcf")
  writeLines(format(start_at), "inst/bioc-sysreqs.ts")
}

bioc_sysreqs_cached <- function() {
  file.path(get_user_cache_dir()$root, "bioc-sysreqs.dcf.gz")
}

load_bioc_sysreqs <- function(path = NULL) {
  if (is.null(path)) {
    cached <- bioc_sysreqs_cached()
    if (file.exists(cached) && file.size(cached) != 0) {
      path <- cached
    } else {
      path <- system.file("bioc-sysreqs.dcf.gz", package = "pkgcache")
    }
  }
  pkgenv$bioc_sysreqs <- parse_packages(path, type = "gzip")
}
