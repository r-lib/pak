
# -------------------------------------------------------------------------
#' Check if an R package name is available.
#'
#' Additionally, look up the candidate name in a number of
#' dictionaries, to make sure that it does not have a negative
#' meaning.
#'
#' ## Valid package name check
#'
#' Check the validity of `name` as a package name. See 'Writing R
#' Extensions' for the allowed package names. Also checked against a list
#' of names that are known to cause problems.
#'
#' ## CRAN checks
#'
#' Check `name` against the names of all past and current packages on
#' CRAN, including base and recommended packages.
#'
#' ## Bioconductor checks
#'
#' Check `name` against all past and current Bioconductor packages.
#'
#' ## Profanity check
#'
#' Check `name` with <https://www.purgomalum.com/service/containsprofanity>
#' to make sure it is not a profanity.
#'
#' ## Dictionaries
#'
#' See the `dictionaries` argument.
#'
#' @param name Package name candidate.
#' @param dictionaries Character vector, the dictionaries to query.
#'   Available dictionaries:
#'     * `wikipedia`
#'     * `wiktionary`,
#'     * `sentiment` (<https://github.com/fnielsen/afinn>),
#'     * `urban` (Urban Dictionary).
#'   If `NULL` (by default), the Urban Dictionary is omitted, as it
#'   is often offensive.
#' @return `pkg_name_check` object with a custom print method.
#'
#' @export
#' @examplesIf pkgdepends:::is_online()
#' pkg_name_check("cli")

pkg_name_check <- function(name, dictionaries = NULL) {
  synchronise(async_pkg_name_check(name, dictionaries))
}

async_pkg_name_check <- function(name, dictionaries = NULL) {
  assert_that(
    is_string(name),
    is.null(dictionaries) || is_character(dictionaries)
  )

  default_dictionaries <- c(
    "wikipedia",
    "wiktionary",
    "sentiment"
  )
  dicts <- dictionaries %||% default_dictionaries

  basics <- async_pnc_basics(name)
  result <- when_all(
    basics     = basics,
    wikipedia  = if ("wikipedia"  %in% dicts) async_wikipedia_get (name),
    wiktionary = if ("wiktionary" %in% dicts) async_wiktionary_get(name),
    sentiment  = if ("sentiment"  %in% dicts) async_sentiment_get (name),
    urban      = if ("urban"      %in% dicts) async_urban_get     (name)
  )$then(function(res) add_class(res, "pkg_name_check"))
}

#' @export

format.pkg_name_check <- function(x, limit = 6, ...) {
  x <- x[!vlapply(x, is.null)]
  unlist(lapply(x, format, limit = limit, ...))
}

#' @export

print.pkg_name_check <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

async_pnc_basics <- function(name) {
  when_all(
    name       = name,
    valid      = pnc_valid(name),
    base       = pnc_base(name),
    crandb     = async_pnc_crandb(name),
    bioc       = async_pnc_bioc(name),
    profanity  = async_profanity_get (name)
  )$then(function(res) add_class(res, "pkg_name_check_basics"))
}

# -------------------------------------------------------------------------

forbidden_package_names <- function() {
  c("pkg", "description")
}

pnc_valid <- function(name) {
  ans <- TRUE
  rx <- paste0("^", pkg_rx()$pkg_name, "$")
  if (!grepl(rx, name)) ans <- FALSE
  # This is not needed currently. But just in case character ranges will
  # accept non-ascii characters on some platforms, we keep it.
  if (ans && any(charToRaw(name) > 127)) ans <- FALSE
  if (ans && name %in% forbidden_package_names()) ans <- FALSE
  add_class(ans, "pkg_name_check_valid")
}

# -------------------------------------------------------------------------

pnc_base <- function(name) {
  bs <- base_packages()
  mch <- match(tolower(name), tolower(bs))
  add_class(
    list(base = is.na(mch), package = if (!is.na(mch)) bs[mch]),
    "pkg_name_check_base"
  )
}

# -------------------------------------------------------------------------
# TODO: this is not included currently. Do we need it?

async_cranlike_check <- function(name) {
  name

  repos <- c(
    CRAN = "https://cran.r-project.org",
    CRANextra = "https://www.stats.ox.ac.uk/pub/RWin"
  )

  meta <- pkgcache::cranlike_metadata_cache$new(
    platforms = "source",
    bioc = current_config()$get("use_bioconductor"),
    cran_mirror = "https://cran.r-project.org",
    repos = repos,
    update_after = as.difftime(5, units = "mins"))

  meta$async_check_update()$
    then(function(data) {
      mch <- match(tolower(name), tolower(data$pkgs$package))
      ret <- list(cran = TRUE, bioc = TRUE, package = NA_character_)
      if (!is.na(mch)) {
        ret$package <- data$pkgs$package[mch]
        type <- data$pkgs$type[mch]
        if (type == "cran") ret$cran <- FALSE
        if (type == "bioc") ret$bioc <- FALSE
      }
      ret
    })$then(function(res) add_class(res, "pkg_name_check_cranlike"))
}

# -------------------------------------------------------------------------

async_pnc_crandb <- function(name) {
  async_pnc_crandb_query(name)$
    then(pnc_crandb_process)
}

async_pnc_crandb_query <- function(name) {
  base <- Sys.getenv(
    "PKG_NAME_CHECK_CRANDB_URL",
    "https://crandb.r-pkg.org:2053/cran/_design/app/_view/alllower"
  )
  url <- paste0(base, "?key=%22", tolower(name), "%22")
  http_get(url)
}

pnc_crandb_process <- function(response) {
  http_stop_for_status(response)
  ans <- jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
  ret <- list(
    crandb = length(ans$rows) == 0,
    package = if (length(ans$rows) > 0) vcapply(ans$rows, "[[", "value")
  )
  add_class(ret, "pkg_name_check_crandb")
}

# -------------------------------------------------------------------------

#' @export

print.pkg_name_check_basics <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.pkg_name_check_basics <- function(x, ...) {

  cw <- cli::console_width()
  stars <- cli::col_yellow(cli::symbol$en_dash, "*", cli::symbol$en_dash)
  title <- paste(stars, cli::style_bold(cli::col_blue(x$name)), stars)
  tbox <- cli::boxx(
    cli::ansi_align(title, width = cw - 4, align = "center"),
    padding = c(0,1,0,1),
    border_style = "double",
    border_col = cli::col_yellow
  )

  ys <- cli::col_green(cli::symbol$tick)
  no <- cli::col_red(cli::symbol$cross)
  mark <- function(x) if (x) ys else no
  str <- c(
    paste(mark(x$valid), " valid name"),
    paste(mark(x$base$base && x$crandb$crandb), " CRAN"),
    paste(mark(x$bioc$bioc), " Bioconductor"),
    if ("profanity" %in% names(x)) {
      paste0(
        mark(!x$profanity),
        if (!x$profanity) "  not a" else " ", " profanity")
    }
  )
  cols <- cli::ansi_columns(str, width = cw - 4, max_cols = length(str))
  bbox <- cli::boxx(cols, padding = c(0,1,0,1), border_col = cli::col_silver)

  c(tbox, bbox)
}

# -------------------------------------------------------------------------

async_wikipedia_get <- function(terms) {
  async_wikipedia_get_query(terms)$
    then(http_stop_for_status)$
    then(function(resp) wikipedia_get_process(terms, resp))$
    then(function(res) add_class(res, "pkg_name_check_wikipedia"))
}

async_wikipedia_get_query <- function(terms) {
  url <- Sys.getenv(
    "PKG_NAME_CHECK_WIKIPEDIA_URL",
    "https://en.wikipedia.org/w/api.php"
  )
  data <- make_wikipedia_data(terms)
  http_post(url, data = data)
}

make_wikipedia_data <- function(terms, intro = TRUE) {
  terms <- enc2utf8(as.character(terms))
  params <- c(
    action = "query",
    format = "json",
    prop = "extracts",
    titles = utils::URLencode(paste(terms, collapse = "|")),
    redirects = 1,
    exintro = if (intro) 1L,
    explaintext = 1
  )
  pstr <- paste0(names(params), "=", params, collapse = "&")
  charToRaw(pstr)
}

wikipedia_get_process <- function(
    terms, resp, base_url = "https://en.wikipedia.org/wiki/") {

  obj <- jsonlite::fromJSON(
    rawToChar(resp$content),
    simplifyVector = FALSE
  )

  map_norm <- structure(
    vcapply(obj$query$normalized, "[[", "to"),
    names = vcapply(obj$query$normalized, "[[", "from")
  )
  nterms <- ifelse(is.na(map_norm[terms]), terms, map_norm[terms])

  map_redir <- structure(
    vcapply(obj$query$redirects, "[[", "to"),
    names = vcapply(obj$query$redirects, "[[", "from")
  )
  rterms <- ifelse(is.na(map_redir[nterms]), nterms, map_redir[nterms])

  map_text <- structure(
    vcapply(obj$query$pages, function(x) x$extract %||% NA_character_),
    names = vcapply(obj$query$pages, "[[", "title")
  )
  text <- map_text[rterms]

  url <- paste0(base_url, gsub(" ", "_", rterms))

  data_frame(
    term = terms,
    normalized = unname(nterms),
    redirected = unname(rterms),
    title = unname(rterms),
    text = unname(text),
    url = ifelse(is.na(text), NA_character_, url)
  )
}

#' @export

print.pkg_name_check_wikipedia <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.pkg_name_check_wikipedia <- function(x, limit = 6, ...) {
  x <- x[1,, drop = FALSE]

  if (is.na(x$text)) {
    x$text <- "No definition found"
    x$url <- ""
  }

  txt <- paste(
    cli::style_underline(x$redirected),
    if (x$redirected != x$normalized) paste0("(from ", x$normalized, ")"),
    clean_wikipedia_text(x$text)
  )

  cw <- cli::console_width()
  wrp <- cli::ansi_strwrap(txt, width = cw - 4)
  hdr <- cli::col_green("Wikipedia")
  ftr <- cli::col_blue(x$url)
  alg <- cli::ansi_align(wrp, width = cw - 4)
  if (length(alg) > limit) alg <- c(alg[1:limit], cli::symbol$ellipsis)
  cli::boxx(
    alg,
    padding = c(0,1,0,1),
    header = hdr,
    footer = ftr,
    border_col = cli::col_silver
  )
}

clean_wikipedia_text <- function(x) {
  x <- trimws(x)
  x <- sub(
    "may refer to:$",
    "may refer to multiple articles, see link.",
    x
  )
  x <- gsub(" ()", "", x, fixed = TRUE)
  x <- gsub("\n", "\n\n", x , fixed = TRUE)
}

# -------------------------------------------------------------------------

async_wiktionary_get <- function(terms) {
  async_wiktionary_get_query(terms)$
    then(function(resp) wiktionary_get_process(terms, resp))
}

async_wiktionary_get_query <- function(terms) {
  url <- Sys.getenv(
    "PKG_NAME_CHECK_WIKTIONARY_URL",
    "https://en.wiktionary.org/w/api.php"
  )
  data <- make_wikipedia_data(terms, intro = FALSE)
  http_post(url, data = data)
}

wiktionary_get_process <- function(terms, resp) {
  http_stop_for_status(resp)

  res <- wikipedia_get_process(
    terms,
    resp,
    base_url = "https://en.wiktionary.org/wiki/"
  )[, c("term", "text", "url")]

  add_class(res, "pkg_name_check_wiktionary")
}

#' @export

print.pkg_name_check_wiktionary <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.pkg_name_check_wiktionary <- function(x, limit = 6, ...) {
  x <- x[1,, drop = FALSE]

  if (is.na(x$text)) {
    x$text <- "No definition found"
    x$url <- ""
  }

  txt <- paste(cli::style_underline(x$term), clean_wiktionary_text(x$text))

  cw <- cli::console_width()
  wrp <- cli::ansi_strwrap(txt, width = cw - 4)
  wrp <- wrp[cli::ansi_strip(wrp) != ""]
  if (length(wrp) > limit) wrp <- c(wrp[1:limit], cli::symbol$ellipsis)
  hdr <- cli::col_green("Wiktionary")
  ftr <- cli::col_blue(x$url)
  alg <- cli::ansi_align(wrp, width = cw - 4)
  cli::boxx(
    alg,
    padding = c(0,1,0,1),
    header = hdr,
    footer = ftr,
    border_col = cli::col_silver
  )
}

clean_wiktionary_text <- function(x) {
  langs <- strsplit(x, "\n== ", x)[[1]][-1]
  eng <- grep("^English", langs, value = TRUE)
  if (length(eng) == 0) return("No English definition found")
  eng2 <- sub("^English ==", "", eng)
  # remove pronunciation
  eng3 <- sub("\n=== Pronunciation ===\n+[^=]*\n+===", "\n===", eng2)
  # remove empty subsections
  eng4 <- gsub("\n==== [^=]+ ====\n", "\n\n", eng3)
  eng5 <- gsub(
    "\n=== ([^=]+) ===\n",
    paste0("\n\n", cli::style_underline("\\1:"), " "),
    eng4
  )
  eng6 <- cli::ansi_trimws(eng5)
  eng7 <- gsub("\n[\n]+", "\n\n", eng6)
  eng8 <- gsub("([^\n])\n([^\n])", "\\1 \\2", eng7)
  eng8
}

# -------------------------------------------------------------------------

async_profanity_get <- function(term) {
  async_profanity_get_query(term)$
    then(http_stop_for_status)$
    catch(error = function(err) list(content = charToRaw("NA")))$
    then(function(resp) profanity_get_process(term, resp))
}

async_profanity_get_query <- function(term) {
  base <- Sys.getenv(
    "PKG_NAME_CHECK_PROFANITY_URL",
    "https://www.purgomalum.com/service/containsprofanity"
  )
  url <- paste0(base, "?text=", utils::URLencode(term))
  http_get(url)
}

profanity_get_process <- function(term, resp) {
  txt <- as.logical(rawToChar(resp$content))
  add_class(txt, "pkg_name_check_profanity")
}

# -------------------------------------------------------------------------

sentiment_get_has_data <- function() ! is.null(pkgd_data$sentiment)

async_sentiment_get <- function(term) {
  start <- if (! sentiment_get_has_data()) {
    async_sentiment_get_data()
  } else {
    async_constant(pkgd_data$sentiment)
  }

  start$
    then(function(stm) structure(stm[term], names = term))$
    then(function(res) add_class(res, "pkg_name_check_sentiment"))
}

async_sentiment_get_data <- function() {
  url <- Sys.getenv(
    "PKG_NAME_CHECK_SENTIMENT_URL",
    "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt"
  )
  http_get(url)$
    then(http_stop_for_status)$
    catch(error = function(err) list(content = raw()))$
    then(function(resp) {
      chr <- rawToChar(resp$content)
      lns <- strsplit(chr, "\n", fixed = TRUE)[[1]]
      cls <- strsplit(lns, "\t", fixed = TRUE)
      pkgd_data$sentiment <- structure(
        as.integer(vapply(cls, "[[", character(1), 2)),
        names = vapply(cls, "[[", character(1), 1)
      )
    })
}

sentiment_string <- function(x) {
  emo <- c(
    "-5" = "\U0001F62D\U0001F62D",
    "-4" = "\U0001F62D",
    "-3" = "\U0001F61E\U0001F61E",
    "-2" = "\U0001F61E",
    "-1" = "\U0001F610",
    "0"  = "\U0001F610",
    "1"  = "\U0001F642",
    "2"  = "\U0001F642\U0001F642",
    "3"  = "\U0001F606",
    "4"  = "\U0001F606\U0001F606",
    "5"  = "\U0001F970"
  )

  asc <- c(
    "-5" = ";(;(",
    "-4" = ";(",
    "-3" = ":(:(",
    "-2" = ":(",
    "-1" = ":|",
    "0"  = ":|",
    "1"  = ":)",
    "2"  = ":):)",
    "3"  = ":D",
    "4"  = ":D:D",
    "5"  = ""
  )
  if (has_emoji()) emo[as.character(x)] else asc[as.character(x)]
}

#' @export

print.pkg_name_check_sentiment <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.pkg_name_check_sentiment <- function(x, ...) {
  if (is.na(x)) x <- 0
  str <- sentiment_string(x)
  txt <- paste0("Sentiment: ", str, cli::col_silver(paste0(" (", x, ")")))
  cw <- cli::console_width()
  alg <- cli::ansi_align(txt, width = cw - 4)
  cli::boxx(alg, padding = c(0,1,0,1), border_col = cli::col_silver)
}

# -------------------------------------------------------------------------

async_urban_get <- function(term) {
  async_urban_get_query(term)$
    then(function(resp) urban_get_process(term, resp))
}

async_urban_get_query <- function(term) {
  base <- Sys.getenv(
    "PKG_NAME_CHECK_URBAN_URL",
    "http://api.urbandictionary.com/v0/"
  )
  url <- paste0(base, "define?term=", utils::URLencode(term))
  http_get(url)$
    then(http_stop_for_status)$
    catch(error = function(err) list(content = charToRaw('{"list":[]}')))
}

urban_get_process <- function(term, resp) {
  obj <- jsonlite::fromJSON(
    rawToChar(resp$content),
    simplifyVector = FALSE
    )$list
  tbl <- data_frame(
    definition = vcapply(obj, "[[", "definition"),
    permalink = vcapply(obj, "[[", "permalink"),
    thumbs_up = viapply(obj, "[[", "thumbs_up"),
    word = vcapply(obj, "[[", "word"),
    written_on = vcapply(obj, "[[", "written_on"),
    example = vcapply(obj, "[[", "example"),
    thumbs_down = viapply(obj, "[[", "thumbs_down")
  )
  add_class(tbl, "pkg_name_check_urban")
}

#' @export

print.pkg_name_check_urban <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.pkg_name_check_urban <- function(x, limit = 6, ...) {
  if (nrow(x) == 0) {
    txt <- "No definition found."
    ftr <- ""
  } else {
    x <- x[1, , drop = FALSE]
    txt <- clean_urban(x$definition)
    ftr <- cli::col_blue(x$permalink)
  }
  cw <- cli::console_width()
  wrp <- cli::ansi_strwrap(txt, width = cw - 4)
  hdr <- cli::col_green("Urban dictionary")
  alg <- cli::ansi_align(wrp, width = cw - 4)
  if (length(alg) > limit) alg <- c(alg[1:limit], cli::symbol$ellipsis)
  cli::boxx(
    alg,
    padding = c(0,1,0,1),
    header = hdr,
    footer = ftr,
    border_col = cli::col_silver
  )
 }

clean_urban <- function(x) {
  gsub("[\r\n]", " ", x)
}

# -------------------------------------------------------------------------

async_pnc_bioc <- function(name) {
  # A removed package? Although maybe this is OK?
  removed <- pnc_bioc_removed()
  mch <- match(tolower(name), tolower(removed))
  if (!is.na(mch)) return(pnc_bioc_false(removed[mch]))

  # An annotatation package?
  ann <- pnc_bioc_old_annotation()
  mch <- match(tolower(name), tolower(ann))
  if (!is.na(mch)) return(pnc_bioc_false(ann[mch]))

  # Need to query
  async_pnc_bioc_web(name)
}

pnc_bioc_false <- function(pkg) {
  async_constant(list(bioc = FALSE, package = pkg))
}

async_pnc_bioc_web <- function(name) {
  name
  async_pnc_bioc_query(name)$
    then(function(response) pnc_bioc_process(name, response))
}

async_pnc_bioc_query <- function(name) {
  base1 <- Sys.getenv(
    "PKG_NAME_CHECK_BIOC_URL",
    "https://git.bioconductor.org/info?packages/"
  )
  url1 <- paste0(base1, substr(tolower(name), 1, 1))
  url2 <- paste0(base1, substr(toupper(name), 1, 1))
  url3 <- Sys.getenv("PKG_NAME_CHECK_BIOC_ANN_URL")
  if (url3 == "") {
    url3 <- paste0(
      pkgcache::bioc_repos(pkgcache::bioc_devel_version())[["BioCann"]],
      "/src/contrib/PACKAGES.gz"
    )
  }
  when_all(http_get(url1), http_get(url2), http_get(url3))
}

pnc_bioc_process <- function(name, response) {
  pkgs <- unique(c(
    pnc_bioc_parse(response[[1]]),
    pnc_bioc_parse(response[[2]]),
    pnc_bioc_parse_pgz(response[[3]])
  ))
  mch <- match(tolower(name), tolower(pkgs))
  add_class(
    list(bioc = is.na(mch), package = if (!is.na(mch)) pkgs[mch]),
    "pkg_name_check_bioc"
  )
}

pnc_bioc_parse <- function(response) {
  http_stop_for_status(response)
  cnt <- strsplit(rawToChar(response$content), "\n")[[1]]
  mch <- re_match(cnt, "packages/(?<package>[a-z0-9A-Z.]+)")
  na.omit(mch$package)
}

pnc_bioc_parse_pgz <- function(response) {
  rc <- rawConnection(response$content)
  on.exit(close(rc), add = TRUE)
  pkgs <- read.dcf(gzcon(rc))
  pkgs[, "Package"]
}

# These are the packages that are not in the Bioconductor git repo

pnc_bioc_removed <- function() {
  c(# removed in 3.5
    "betr", "encoDnaseI", "ggtut",
    # removed in 3.4
    "AffyTiling", "cellHTS", "DASiR", "DAVIDQuery", "GenoView",
    "inSilicoDb", "inSilicoMerging", "jmosaic", "metaX", "MMDiff",
    "neaGUI", "Rolexa", "RWebServices", "SJava", "SomaticCA", "spade",
    # removed in 3.1
    "asmn", "COPDSexualDimorphism", "DNaseR", "flowFlowJo", "flowPhyto",
    # removed in 3.0
    "RMAPPER", "virtualArray",
    # removed in 2.14
    "Agi4x44PreProcess", "maDB", "pgUtils",
    # removed in 2.13
    "dualKS", "externalVector", "GeneGroupAnalysis", "iFlow", "KEGGSOAP",
    "xmapcore",
    # removed in 2.12
    "cosmo", "cosmoGUI", "gene2pathway",
    # removed in 2.11
    "PatientGeneSets",
    # removed in 2.10
    "edd", "GeneRfold", "ontoTools", "GeneR", "RMAGEML", "RTooks4TB",
    # Packages removed with Bioconductor 2.9 release
    "GeneSpring", "GeneTraffic", "makePlatformDesign", "Rdbi", "RdbiPgSQL",
    "rflowcyt", "Rredland", "Ruuid", "simulatorAPMS", "snpMatrix",
    # Packages removed with Bioconductor 2.8 release
    "exonmap", "biocDatasets",
    # Packages removed with Bioconductor 2.7 release
    "matchprobes", "GeneticsBase", "fbat",
    # Packages removed with Bioconductor 2.6 release
    "keggorth",
    # Packages removed with Bioconductor 2.5 release
    "stam", "Rintact",
    # Packages removed with Bioconductor 2.4 release
    "AnnBuilder", "RSNPer",
    # Packages removed with Bioconductor 2.3 release
    "SemSim", "WidgetInvoke",
    # Packages removed with Bioconductor 2.2 release
    "SAGElyzer", "GeneTS", "arrayMagic", "chromoViz", "iSPlot", "iSNetword",
    # Packages removed with Bioconductor 2.1 release
    "bim", "arrayQCplot",
    # Packages removed with Bioconductor 2.0 release
    "gff3Plotter", "mmgmos", "applera",
    # Packages removed with Bioconductor 1.9 release
    "reposTools", "exprExternal", "SNAData", "rfcdmin", "goCluster",
    "GenomeBase", "y2hStat",
    # Packages removed with Bioconductor 1.8 release
    "msbase", "ideogram",
    # Packages removed with Bioconductor 1.7 release
    "mscalib"
  )
}

# nocov start
function() {
  bv <- setdiff(
    as.character(pkgcache::bioc_version_map()$bioc_version),
    c("1.6", "1.7")
  )
  pkgs <- lapply(bv, function(v) {
    repos <- pkgcache::bioc_repos(v)["BioCann"]
    rownames(utils::available.packages(repos = repos))
  })
  packages <- sort(unique(unlist(pkgs)))
  saveRDS(
    packages,
    file = "inst/exdata/biocpackages.rds",
    version = 2,
    compress = "xz"
  )
}
# nocov end

pnc_bioc_old_annotation <- function() {
  readRDS(system.file("exdata", "biocpackages.rds", package = "pkgdepends"))
}
