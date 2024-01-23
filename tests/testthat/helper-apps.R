
# -------------------------------------------------------------------------
# Dummy CRAN app

cran_app <- function(...) {
  asNamespace("pkgcache")$cran_app(...)
}

bioc_app <- function(...) {
  asNamespace("pkgcache")$bioc_app(...)
}

dcf <- function(...) {
  asNamespace("pkgcache")$dcf(...)
}

fix_port <- function(...) {
  asNamespace("pkgcache")$fix_port(...)
}

cran_app_pkgs <- dcf("
  Package: pkg1
  Version: 1.0.0

  Package: pkg1
  Version: 0.9.0

  Package: pkg1
  Version: 0.8.0

  Package: pkg2
  Version: 1.0.0
  Depends: pkg1

  Package: pkg3
  Version: 1.0.0
  Depends: pkg2

  Package: pkg3
  Version: 0.9.9

  Package: crayon
  Version: 1.0.0

  Package: needspak
  Imports: pak

  Package: pak

  Package: futurama
  Depends: R (>= 3000.0)

  Package: needsfuturama
  Imports: futurama

  Package: dplyr
  Imports: tibble
  Suggests: testthat

  Package: tibble

  Package: testthat

  Package: curl
  SystemRequirements: libcurl: libcurl-devel (rpm) or libcurl4-openssl-dev (deb).
")

fake_cran <- webfakes::local_app_process(
  cran_app(cran_app_pkgs),
  opts = webfakes::server_opts(num_threads = 3)
)

bioc_app_pkgs <- dcf("
  Package: Biobase
  Version: 1.2.3
  Depends: R (>= 2.10), BiocGenerics(>= 0.27.1), utils
  Imports: methods
  Suggests: tools, tkWidgets, ALL, RUnit, golubEsets
")

fake_bioc <- webfakes::local_app_process(
  bioc_app(bioc_app_pkgs),
  opts = webfakes::server_opts(num_threads = 3)
)

setup_fake_apps <- function(
  cran_app = NULL,
  bioc_app = NULL,
  cran_repo = NULL,
  bioc_repo = NULL,
  cran_options = NULL,
  bioc_options = NULL,
  .local_envir = parent.frame()) {

  cran_app <- if (!is.null(cran_app)) {
                cran_app
              } else if (!is.null(cran_repo)) {
                app <- webfakes::local_app_process(
                  cran_app(cran_repo, options = as.list(cran_options)),
                  opts = webfakes::server_opts(num_threads = 3),
                  .local_envir = .local_envir
                )
                assign(".cran_app", app, envir = .local_envir)
                app
              } else {
                fake_cran
              }

  bioc_app <- if (!is.null(bioc_app)) {
                bioc_app
              } else if (!is.null(bioc_repo)) {
                app <- webfakes::local_app_process(
                  bioc_app(bioc_repo, options = as.list(bioc_options)),
                  opts = webfakes::server_opts(num_threads = 3),
                  .local_envir = .local_envir
                )
                assign(".bioc_app", app, envir = .local_envir)
                app
              } else {
                fake_bioc
              }

  withr::local_options(
    repos = c(CRAN = cran_app$url()),
    pkg.cran_metadata_url = cran_app$url(),
    .local_envir = .local_envir
  )
  withr::local_envvar(
    R_PKG_CRAN_METADATA_URL = cran_app$url(),
    R_BIOC_CONFIG_URL = paste0(bioc_app$url(), "/config.yaml"),
    R_BIOC_VERSION = NA_character_,
    R_BIOC_MIRROR = bioc_app$url(),
    .local_envir = .local_envir
  )
}

# -------------------------------------------------------------------------
# GH app

gh_app_desc <- function(pkg) {
  sprintf("Package: %s\nVersion: 1.0.0\n", pkg)
}

random_sha <- function() {
  paste(
    sample(c(0:9, letters[1:6]), 64, replace = TRUE),
    collapse = ""
  )
}

gh_app_repos <- list(
  users = list(
    "r-lib" = list(
      repos = list(
        pak = list(
          commits = list(
            list(
              sha = "111ef906acb58fe406370f7bc0a72cac55dbbb231ea687494c25742ca521255a",
              branch = "main",
              tag = "HEAD",
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            ),
            list(
              sha = "a503fe843f11c279864f29d58137f8de319d115b239ce48ccc15406306019480",
              branch = "main",
              tag = "v0.1.2",
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            ),
            list(
              sha = "e65de1e9630dbfcaf1044718b742bf806486b107239ce48ccc15406306019480",
              branch = "main",
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            ),
            list(
              sha = "b001d6ddeab1589ad367b62baabbeeb2af3b0ebac2e61d239df660c1d63e3232",
              branch = "somebranch",
              pull = 90,
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            ),
            list(
              sha = "b001d6ddeab1589ad367b62baabbeeb2af3b0ebac2e61d239df660c1d63e3232",
              latestRelease = TRUE,
              tagName = "v1.2.3",
              files = list("DESCRIPTION" = gh_app_desc("pak"), NAMESPACE = "")
            )
          )
        ),
        bad = list(
          commits = list(
            list(
              sha = "546d9eab84b002c35302dda3822560950c7528cfc9ef1b916cecd9dbef3cf6b6",
              tag = "HEAD",
              branch = "main",
              files = list(
                DESCRIPTION = "this is not\na good file\n",
                "bin/DESCRIPTION" = charToRaw("\xf0\xb0\xa0")
              )
            ),
            list(
              sha = "546d9eab84b002c35302dda3822560950c7528cfc9ef1b916cecd9dbef3cf6b6",
              pull = 100,
              branch = "other",
              files = list(DESCRIPTION = "this is not\na good file\n")
            )
          )
        ),
        crayon = list(
          commits = list(
            list(
              sha = "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975",
              tag = "HEAD",
              branch = "main",
              files = list(DESCRIPTION = gh_app_desc("crayon"), NAMESPACE = "")
            ),
            list(
              sha = "b5221ab024605019800ddea474f7a0981a4d53f719f5af2b1af627b34e0760b2",
              branch = "b5221ab024605019800ddea474f7a0981a4d53f719f5af2b1af627b34e0760b2",
              files = list(DESCRIPTION = gh_app_desc("crayon"), NAMESPACE = "")
            ),
            list(
              sha = "9d93692f8f7c1d6b2308d0c4aa83cdc2d99ec1fd0097cede1d9aa1301247cb01",
              branch = "pr61",
              pull = 79,
              files = list(DESCRIPTION = gh_app_desc("crayon"), NAMESPACE = "")
            )
          )
        ),
        pkgconfig = list(
          commits = list(
            list(
              sha = "c9be9cde5e91ad771d1b5150781e6e8d32a7be0e9ab227bdf45cb41ad513004c",
              branch = "pr7",
              pull = 7,
              files = list(DESCRIPTION = gh_app_desc("pkgconfig"), NAMESPACE = "")
            )
          )
        )
      )
    ),

    "wesm" = list(
      repos = list(
        "feather" = list(
          commits = list(
            list(
              sha = "ec40c1eae1ac83b86fc41bb2f5cd916152d19015649c3d209f2c08115dd993b1",
              tag = "HEAD",
              branch = "main",
              files = list("R/DESCRIPTION" = gh_app_desc("feather"), NAMESPACE = "")
            )
          )
        )
      )
    ),

    "gaborcsardi" = list(
      repos = list(
        "secret-test" = list(
          commits = list(
            list(
              sha = "599cc5d745d2079eddf1ff582b83d381e885cd30f33bafebbe83e73d010cfa93",
              tag = "HEAD",
              branch = "main",
              token = "b9984750bea6a170081ca98255c3b43fe5fb0978",
              files = list("DESCRIPTION" = gh_app_desc("secret"), NAMESPACE = "")
            )
          )
        ),
        "secret" = list(
          commits = list(
            list(
              sha = "7f9fb08e26015e05529cd4d7fc2a7edbd88c783d456ff83a96dcc58ace1d3ea5",
              tag = "HEAD",
              branch = "x",
              files = list("DESCRIPTION" = gh_app_desc("secret"), NAMESPACE = "")
            )
          )
        )
      )
    ),

    "tidyverse" = list(
      repos = list(
        "tidyverse.org" = list(
          commits = list(
            list(
              sha = "d998eab68c66d862c31a6091f9e71200b13bb44ea754e3371d098dcaa20e51a4",
              tag = "HEAD",
              branch = "main",
              files = list("foo" = "bar")
            )
          )
        )
      )
    ),

    "cran" = list(
      repos = list(
        "rJava" = list(
          commits = list(
            list(
              sha = "dfb3b64b13343e07b2db038777d9dc2aba5d824c5eca8c891c87bd4fd38d7256",
              tag = "HEAD",
              branch = "master",
              files = list(
                DESCRIPTION = "Package: rJava\nVersion: 1.0-6\nSystemRequirements: Java JDK 1.2 or higher (for JRI/REngine JDK 1.4 or higher), GNU make\n",
                NAMESPACE = ""
              )
            )
          )
        )
      )
    )

  )
)

fake_gh <- webfakes::local_app_process(
  gh_app(gh_app_repos),
  opts = webfakes::server_opts(num_threads = 3)
)

setup_fake_gh_app <- function(.local_envir = parent.frame()) {
  withr::local_envvar(
    .local_envir = .local_envir,
    R_PKG_GITHUB_API_URL = fake_gh$url()
  )
}


# -------------------------------------------------------------------------
# Name check app

new_check_app <- function() {
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


new_sysreqs_app <- function() {
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$use(webfakes::mw_urlencoded())

  db <- list(
    ubuntu = list(
      "22.04" = list(
        "java" = list(
          install_scripts = list("apt-get install -y default-jdk"),
          post_install = list(list(command = "R CMD javareconf"))
        ),
        "openssl" = list(
          install_scripts = list("apt-get install -y libssl-dev")
        ),
        "libcurl" = list(
          install_scripts = list("apt-get install -y libcurl4-openssl-dev")
        )
      ),
      "16.04" = list(
        "\\bgeos\\b" = list(
          pre_install = list(
            list(command = "apt-get install -y software-properties-common"),
            list(command = "add-apt-repository -y ppa:ubuntugis/ppa"),
            list(command = "apt-get update")
          ),
          install_scripts = list("apt-get install -y libgeos-dev")
        )
      )
    )
  )

  app$post("/__api__/repos/:id/sysreqs", function(req, res) {
    dist <- req$query$distribution
    rele <- req$query$release

    dsc <- desc::desc(text = rawToChar(req$.body))
    pkgsrq <- trimws(dsc$get("SystemRequirements"))
    if (is.na(pkgsrq)) pkgsrq <- ""

    if (dist == "ubuntu" && rele %in% c("16.04", "18.04", "20.04", "22.04")) {
      mydb <- db[[dist]][[rele]]
      srq <- lapply(names(mydb), function(nm) {
        if (grepl(nm, pkgsrq)) mydb[[nm]] else NULL
      })

      bf <- unlist(lapply(srq, "[[", "pre_install"), recursive = FALSE)
      is <- unlist(lapply(srq, "[[", "install_scripts"))
      af <- unlist(lapply(srq, "[[", "post_install"), recursive = FALSE)

      res$send_json(list(
            name = jsonlite::unbox("pkgdependssysreqs"),
            pre_install = bf,
            install_scripts = is,
            post_install = af
          ))

    } else {
      res$set_status(400)
      res$send_json(list(
        code = jsonlite::unbox(14),
        error = jsonlite::unbox("Unsupported system"),
        payload = jsonlite::unbox(NA)
      ))
    }
  })

  app
}

fake_sysreqs <- webfakes::local_app_process(new_sysreqs_app())

setup_fake_sysreqs_app <- function(.local_envir = parent.frame()) {
  withr::local_envvar(
    .local_envir = .local_envir,
    RSPM_ROOT = sub("/$", "", fake_sysreqs$url())
  )
}

transform_sysreqs_server <- function(x) {
  x <- gsub("https://packagemanager.posit.co", "<server>", x, fixed = TRUE)
  x <- gsub("http://127.0.0.1:[0-9]+", "<server>", x)
  x <- gsub("http://localhost:[0-9]+", "<server>", x)
  x
}

show_request <- function(req) {
  x <- jsonlite::fromJSON(rawToChar(req$content))
  cat(toupper(x$method), " ", x$type, sep = "", "\n")
  cat("Query string: ", x$query, sep = "", "\n")
  cat("Body: ", x$body, sep = "", "\n")
}

check_app <- webfakes::new_app_process(
  new_check_app(),
  opts = webfakes::server_opts(num_threads = 4)
)

transform_no_srcref <- function(x) {
  x <- sub("[ ]*at [-a-zA-Z0-9]+[.]R:[0-9]+:[0-9]+", "", x)
  x <- sub("[ ]*at line [0-9]+", "", x)
  x <- sub("\033[90m\033[39m", "", x, fixed = TRUE)
  x <- sub("Caused by error: ", "Caused by error:", x, fixed = TRUE)
  if (x[length(x)] == "") x <- x[-length(x)]
  x
}

transform_local_port <- function(x) {
  gsub("127\\.0\\.0\\.1:[0-9]+", "127.0.0.1:<port>", x)
}

transform_bioc_version <- function(x) {
  sub("3[.][0-9]+/bioc", "<bioc-version>/bioc", x)
}

transform_bytes <- function(x) {
  sub("[(][0-9]+ B[)]", "(<size>)", x)
}

transform_ext <- function(x) {
  x <- sub("[.](zip|tgz)", ".zip/.tgz/.tar.gz", x)
  x <- sub("_R_[-_a-z0-9A-Z]+[.]tar[.]gz", ".zip/.tgz/.tar.gz", x)
  x
}

transform_sha <- function(x) {
  gsub("[a-fA-F0-9]{64}", "<sha>", x)
}

transform_hash <- function(x) {
  x <- gsub("[a-f0-9]{32}", "<hash>", x)
  x <- gsub("[a-f0-9]{10}", "<hash>", x)
  x
}

transform_etag <- function(x) {
  sub("RemoteEtag: \"[a-z0-9]+\"", "RemoteEtag: \"<etag>\"", x)
}

transform_tempdir <- function(x) {
  x <- sub(tempdir(), "<tempdir>", x)
  x <- sub(normalizePath(tempdir()), "<tempdir>", x)
  x <- sub(normalizePath(tempdir(), winslash = "/"), "<tempdir>", x)
  x <- sub("[\\\\/]file[a-zA-Z0-9]+", "/<tempfile>", x)
  x <- sub("[A-Z]:.*Rtmp[a-zA-Z0-9]+/", "<tempdir>/", x)
  x
}

transform_show_cursor <- function(x) {
  gsub("\033[?25h", "", x, fixed = TRUE)
}

transform_no_links <- function(x) {
  cli::ansi_strip(x, sgr = FALSE, csi = FALSE, link = TRUE)
}

transform_installed_in_temp <- function(x) {
  m <- regexpr("installed::.*$", x)
  regmatches(x, m) <- paste0("installed::.../", long_basename(regmatches(x, m)))
  x
}

fake_git <- local({
  dir.create(tmp <- tempfile())
  untar(testthat::test_path("fixtures/git-repo.tar.gz"), exdir = tmp)
  app <- git_app(file.path(tmp, "repo"))
  webfakes::local_app_process(app)
})
