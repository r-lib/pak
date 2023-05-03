
pkg_build <- function(package, lib = .libPaths()[1],
                      flavor = Sys.getenv("PKG_BUILD_FLAVOR")) {
  remote(
    function(...) asNamespace("pak")$pkg_build_internal(...),
    list(package = package, lib = lib, flavor = flavor)
  )
}

pkg_build_internal <- function(package, lib, flavor) {
  pkgdepends::pkg_build(package, library = lib, flavor = flavor)
}

repo <- list(
  add = function(file, ..., path = ".") {
    remote(
      function(...) pkgdepends::repo$add(...),
      list(file = file, ..., path = path)
    )
  },

  delete = function(package, ..., path = ".") {
    remote(
      function(...) pkgdepends::repo$delete(...),
      list(package = package, ..., path = path)
    )
  },

  list = function(..., path = ".") {
    remote(
      function(...) pkgdepends::repo$list(...),
      list(..., path = path)
    )
  },

  update = function(file, ..., path = ".") {
    remote(
      function(...) pkgdepends::repo$update(...),
      list(file = file, ..., path = path)
    )
  },

  update_gh = function(repo, subdir, file) {
    remote(
      function(...) pkgdepends::repo$update_gh(...),
      list(repo = repo, subdir = subdir, file = file)
    )
  }
)

ghr <- list(
  add_asset = function(repo, file, tag, name = basename(file)) {
    remote(
      function(...) pkgdepends::ghr$add_asset(...),
      list(repo = repo, file = file, tag = rag, name = name)
    )
  },

  create = function(repo, tag) {
    remote(
      function(...) pkgdepends::ghr$create(...),
      list(repo = repo, tag = tag)
    )
  },

  get = function(repo, tag) {
    remote(
      function(...) pkgdepends::ghr$get(...),
      list(repo = repo, tag = tag)
    )
  },

  list = function(repo) {
    remote(
      function(...) pkgdepends::ghr$list(...),
      list(repo = repo)
    )
  },

  list_assets = function(repo, tag) {
    remote(
      function(...) pkgdepends::ghr$list_assets(...),
      list(repo = repo, tag = tag)
    )
  }
)
