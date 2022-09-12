# pak (development version)

# pak 0.3.1

* The `?ignore` parameter works correctly now.

* Dependency resolution now does not fail if a package is not found.

* pak can now install `url::` remotes from GitHub.

* pak now does not fail when the package of a `.tar.gz` GitHub
  snapshot is in a subdirectory, or in a subdirectory of a subdirectory.

* pak now errors early if it cannot deduce the name of the package
  from a `Remotes` or `Config/Needs/*` entry.

* Solver failures now include details in some cases where previously they
  did not.

* pak can now update packages in Docker containers where the
  old version was installed in the different Docker later (#251)

* pak errors are now user friendlier and better formatted.

* pak now does not load tibble and its dependencies in the pak subprocess,
  so their dlls are not locked by the pak subprocess on Windows.

* pak now does not fail when installing a package that uses a non-UTF-8
  encoding on R 4.3.x and later (#404).

# pak 0.3.0

* pak functions that used to return tibbles return data frames now.
  While data frames and tibbles are very similar, they are not completely
  compatible. To convert the outputs of pak functions to tibbles call the
  `tibble::as_tibble()` function on them. If the pillar package is loaded,
  it improves the printing of the returned data frames.

  Relatedly, `pak::pak_install_extra()` installs pillar now, instead of tibble.

* pak now supports `file://` repositories.

* pak now uses HTTP 1.1 to download packages on Linux, in addition to macOS.
  This fixes HTTP issues with some servers (#358).

* New `?ignore-before-r` parameter to ignore optional dependencies that
  need a newer R version (https://github.com/r-lib/pkgdepends/issues/243).

* New `?ignore` parameter to ignore an optional dependency.

* Allow specifying downstream package parameters with the `package=?param`
  syntax.

* `lockfile_install()` now works better for `any::` refs, and pak always
  install the version it has planned for.

* System requirement installation is now more robust and works for
  Unix shell expressions (#347).

* CRAN-like resolution is more robust now if a repository is missing
  the usual metadata.

* The lock file is pretty JSON now.

* pak now handles all version requirement types properly:
  '<', '<=', `==`, `>=`, `>`.

* The dependency solver now uses better heuristics and does not
  (effectively) freeze if multiple repositories have multiple versions of
  the same packages (e.g. RSPM and CRAN)
  (https://github.com/r-lib/pkgdepends/pull/277)

# pak 0.2.1

No user visible changes.

# pak 0.2.0

Lots of news, too much to list. This is a completely new package now.

# pak 0.1.2

First version on CRAN.
