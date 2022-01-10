
embed_lib <- local({
  function(pkgroot) {
    libdir <- file.path(getNamespaceInfo("pak", "path"), "library")
    pip <- pkgdepends::new_pkg_installation_proposal(
      paste0("deps::", pkgroot),
      config = list(
        library = libdir,
        dependencies = "Config/needs/dependencies"
      )
      )
    pip$set_solve_policy("upgrade")
    pip$resolve()
    pip$solve()
    pip$download()
    pip$install()
  }
})
