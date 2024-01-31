# -------------------------------------------------------------------------
# Name check app

name_check_app <- function() {
  `%||%` <- function(l, r) if (is.null(l)) r else l

  app <- webfakes::new_app()

  app$use(webfakes::mw_json())
  app$use(webfakes::mw_urlencoded())

  app$get("/crandb", function(req, res) {
    pkg <- sub("\"$", "", sub("^\"", "", req$query$key))
    if (pkg == "dbi") {
      res$send_json(list(
        total_rows = 20000,
        offset = 14000,
        rows = list(list(id = "DBI", key = "dbi", value = "DBI"))
      ), auto_unbox = TRUE)
    } else {
      res$send_json(list(
        total_rows = 20000,
        offset = 14000,
        rows = list()
      ))
    }
  })

  app$post("/wikipedia", function(req, res) {
    titles <- strsplit(req$form$titles, "|", fixed = TRUE)[[1]]
    Titles <- tools::toTitleCase(titles)
    ret <- list(query = list(
      normalized = list(list(from = titles, to = Titles)),
      pages = list(`11178` = list(
        pageid = 11178,
        title = Titles,
        extract = "The terms foobar (), foo, bar, and others are used ..."
      ))
    ))
    res$send_json(ret, auto_unbox = TRUE)
  })

  app$all(c("/echo", "/echo/define"), function(req, res) {
    out <- list(
      method = req$method,
      query = req$query_string,
      type = req$get_header("Content-Type") %||% NA_character_,
      body = rawToChar(req$.body %||% raw())
    )
    res$send_json(out, auto_unbox = TRUE)
  })

  app$get("/sentiment", function(req, res) {
    txt <- "abuse\t-3\nirony\t-1\nxo\t3\nxoxoxo\t4\n"
    res$send(txt)
  })

  app$get("/bioc/a", function(req, res) {
    res$send(paste0(collapse = "", c(
      "hello nobody, this is httpd@ip-172-30-0-33 running gitolite3 v3.6.6-6-g7c8f0ab on git 2.28.0",
      "",
      " R  \tpackages/a4",
      " R  \tpackages/a4Base",
      " R  \tpackages/a4Classif",
      " R  \tpackages/a4Core",
      " R  \tpackages/a4Preproc",
      " R  \tpackages/a4Reporting",
      " R  \tpackages/aCGH",
      " R  \tpackages/abseqR",
      " R  \tpackages/ag.db"
    ), "\n"))
  })

  app$get("/bioc/A", function(req, res) {
    res$send(paste0(collapse = "", c(
      "hello nobody, this is httpd@ip-172-30-0-33 running gitolite3 v3.6.6-6-g7c8f0ab on git 2.28.0",
      "",
      " R  \tpackages/ABAData",
      " R  \tpackages/ABAEnrichment",
      " R  \tpackages/ABSSeq",
      " R  \tpackages/AGDEX",
      " R  \tpackages/AHPathbankDbs",
      " R  \tpackages/AIMS",
      " R  \tpackages/ALDEx2",
      " R  \tpackages/ALL",
      " R  \tpackages/ALLMLL",
      " R  \tpackages/ALPS",
      " R  \tpackages/AMARETTO"
    ), "\n"))
  })

  app$get("/biocann/src/contrib/PACKAGES.gz", function(req, res) {
    tmp <- tempfile(fileext = ".gz")
    on.exit(unlink(tmp), add = TRUE)
    l <- c(
      "Package: adme16cod.db",
      "Version: 3.4.0",
      "Depends: R (>= 2.7.0), methods, AnnotationDbi (>= 1.31.18),",
      "        org.Rn.eg.db (>= 3.2.1)",
      "Imports: methods, AnnotationDbi",
      "Suggests: annotate, RUnit",
      "License: Artistic-2.0",
      "MD5sum: 3902516a40a503302ef732143b2394b9",
      "NeedsCompilation: no",
      "",
      "Package: ag.db",
      "Version: 3.2.3",
      "Depends: R (>= 2.7.0), methods, AnnotationDbi (>= 1.34.3),",
      "        org.At.tair.db (>= 3.3.0)",
      "Imports: methods, AnnotationDbi",
      "Suggests: DBI, annotate, RUnit",
      "License: Artistic-2.0",
      "MD5sum: e5913da38fe4487202306cacd885840d",
      "NeedsCompilation: no",
      "",
      "Package: agcdf",
      "Version: 2.18.0",
      "Depends: utils",
      "Imports: AnnotationDbi",
      "License: LGPL",
      "MD5sum: 5dd14bc6a6d2729f5e7b170105c78e48",
      "NeedsCompilation: no"
    )
    writeLines(l, con <- gzfile(tmp, open = "wb"))
    close(con)

    # We don't use send_file, because of a webfakes bug on Windows
    # with absolute paths. Webfakes prepends '/' to 'c:/...'.
    blob <- readBin(tmp, what = "raw", n = 10000)
    res$
      set_type("application/gzip")$
      send(blob)
  })

  app
}
