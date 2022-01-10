
# default_cran_mirror

cran_install_order <- local({

  base_packages <- function() {
    c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
      "methods", "parallel", "splines", "stats", "stats4", "tcltk",
      "tools", "utils"
    )
  }

  parse_dep_fields <- function(flds) {
    flds[is.na(flds)] <- ""
    flds <- gsub("\\s+", "", flds)
    flds <- gsub("\\([^)]+\\)", "", flds)
    notempty <- nzchar(flds)
    res <- replicate(length(flds), character())
    flds <- flds[notempty]
    flds <- strsplit(flds, ",", fixed = TRUE)

    base <- base_packages()
    flds <- lapply(flds, setdiff, y = c("R", base))

    res[notempty] <- flds
    res
  }

  extract_deps <- function(pkg, db) {
    dep_types <- c("Depends", "Imports", "LinkingTo")
    fields <- db[ db[, "Package"] %in% pkg, dep_types]
    unlist(parse_dep_fields(fields))
  }


  calculate_deps <- function(pkgs) {
    db <- available.packages(repos = default_cran_mirror())
    current <- character()
    deps <- list()
    new <- pkgs
    while (length(new) > 0) {
      deps[new] <- lapply(new, extract_deps, db = db)
      new <- setdiff(unlist(deps[new]), names(deps))
    }
    deps
  }

  topo_deps <- function(deps) {
    pkgs <- character()
    while (length(deps) > 0) {
      ndeps <- vapply(deps, length, integer(1))
      nxt <- names(deps)[ndeps == 0]
      pkgs <- c(pkgs, nxt)
      deps <- deps[! names(deps) %in% nxt]
      deps[] <- lapply(deps, setdiff, nxt)
    }
    pkgs
  }

  function(pkgs) {
    deps <- calculate_deps(pkgs)
    topo_deps(deps)
  }

})
