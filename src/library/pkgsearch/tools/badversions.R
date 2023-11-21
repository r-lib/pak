
download_crandb <- function() {
  if (file.exists("cran-full.json.gz")) {
    system2("gzip", c("-d", "cran-full.json.gz"))
  }
  if (!file.exists("cran-full.json")) {
    download.file(
      "https://crandb.r-pkg.org:2053/cran/_all_docs?include_docs=true",
      "cran-full.json.tmp"
    )
    file.rename("cran-full.json.tmp", "cran-full.json")
  }

  entries <- jsonlite::fromJSON("cran-full.json", simplifyVector = FALSE)$rows
  pkgs <- Filter(function(x) is.null(x$doc$type) && !startsWith(x$id, "_"), entries)

  pkgs
}

extract_versions <- function(pkgs) {
  vers <- lapply(pkgs, function(pkg) {
    unname(sapply(pkg$doc$versions, "[[", "Version"))
  })
  names(vers) <- sapply(pkgs, "[[", "id")

  data.frame(
    stringsAsFactors = FALSE,
    package = rep(names(vers), lengths(vers)),
    version = unname(unlist(vers))
  )
}

find_bad_versions <- function(vers) {
  pv <- package_version(vers$version, strict = FALSE)
  bad <- vers[is.na(pv), ]
  tapply(bad$version, bad$package, c)
}

badversions_main <- function() {
  pkgs <- download_crandb()
  vers <- extract_versions(pkgs)
  bad <- find_bad_versions(vers)
  bad
}

if (is.null(sys.calls())) {
  badversions_main()
}
