# pak 0.7.2

* pak now supports using parameters for all packages with the
  `*=?<param>` form. E.g. `*=?source` installs all packages from source.

* pak now supports R 4.4.0 again, and also Rtools44.

# pak 0.7.1

* pak can now handle the case when `Config/Needs/*` dependencies
  are requested for package from a repository.

* pak uses safer `*printf()` format strings now.

# pak 0.7.0

* pak now correctly handles the latest GitHub release with
  the `@*release` notation (@pawelru,
  https://github.com/r-lib/pkgdepends/pull/321)

* pak now correctly handles having multiple instances of the same
  package in the metadata, with different R version requirements
  (#534, #538, https://github.com/r-lib/pkgdepends/pull/331).

* `git::` package references work better now for Azure DevOps
  (@jameslairdsmith, https://github.com/r-lib/pkgdepends/pull/333,
  https://github.com/r-lib/pkgdepends/pull/342).

* pak now does a better job at accepting installed packages, and
  avoids reinstalling more packages than needed when using a lock file
  (https://github.com/r-lib/actions/issues/759,
  https://github.com/r-lib/pkgdepends/pull/338).

# pak 0.6.0

* pak now requires R >= 3.5.0.

* Many improvements in system requirements support:
  - New functions:
    - `pkg_sysreqs()`: calculate system requirements of packages.
    - `sysreqs_db_list()`, `sysreqs_db_match()`, `sysreqs_db_update()`:
      query the system requirements database.
    - `sysreqs_list_system_packages()`, `sysreqs_check_installed()`,
      `sysreqs_fix_installed()`: query and install missing system packages.
    - `sysreqs_platforms()`: list supported platforms.
  - The installation proposal, printed before installation, now includes
    required and missing system packages, on supported platforms.
  - New `sysreqs_platform` configuration option to override the auto-detected
    platform.
  - Faster, asynchronous system requirements lookup.
  - pak now does not reinstall system requirements by default,
    if they are already installed. (You can force a reinstall/upgrade
    with the `sysreqs_update` configuration option.)

* New `gitlab::` package source to install packages from GitLab
  (https://github.com/r-lib/pkgdepends/pull/315).

* pak now correctly parses multiple `git::` packages at once
  (https://github.com/r-lib/pkgdepends/issues/318).

* `git::` package sources now support version 1 of the git protocol.
  E.g. the Bioconductor git repositories now work:
  `git::https://git.bioconductor.org/packages/limma`
  (https://github.com/r-lib/pkgdepends/issues/314).

* The `platforms` config parameter now works correctly with `deps::`
  package sources (#522).

* New `include_linkingto` config parameter to always include `LinkingTo`
  packages in the solution, even for binaries
  (https://github.com/r-lib/pkgdepends/issues/485).

* `pkg_name_check()` now does not include Acromine results, because the web
  site was unstable.

* In `repo_add()` and `repo_resolve()` the `MRAN@` prefix is now deprecated
  and resolves to PPM, because MRAN will be retired soon. See more at
  <https://posit.co/blog/migrating-from-mran-to-posit-package-manager/>.

* The metadata cache now has `SystemRequirements` information for Bioconductor
  packages.

# pak 0.5.1

* No user visible changes.

# pak 0.5.0

* The `meta_*()` functions now consider the `platforms`, `cran_mirror` and
  `r_versions` config entries, see `?"pak-config"`.

* Better Posit Package Manager (PPM) support. New `+ppm_has_binaries()`,
  `ppm_r_versions()`, `ppm_repo_url()`, `ppm_snapshots()` and `ppm_platforms()`
  functions to help interacting with PPM. See the new 'pkgcache and Posit
  Package Manager on Linux' article at https://r-lib.github.io/pkgcache.
  (r-lib/pkgcache#47 and r-lib/pkgdepends#186).

* New `system_r_platform()` and `system_r_platform_data()` functions to query
  the current platform.

* pak now support git repositories as package references. E.g.
  `git::https://github.com/r-lib/pak.git`.

* pak now supports versioned CRAN packages, e.g. `dplyr@1.1.1` will
  always install dplyr 1.1.1. Note that only CRAN packages are supported,
  Bioconductor packages are not (yet).

* pak now has an alternative system requirements lookup
  implementation. It supports Fedora and Debian systems as well, in
  addition to Debian, Ubuntu, SUSE and RedHat derivatives.
  You can switch to this implementation by setting the
  `R_PKG_SYSREQS2` environment variable to `true`.

* pak now does a better job looking up dependencies for
  hand-selected dependency types. E.g. `dependencies = "LinkingTo"`.

* pak now removes `?ignore`-d packages from dependencies, and
  uses the correct version comparison for `?ignore-before.r`
  (https://github.com/r-lib/actions/issues/708).

* pak now does not fail for circular soft dependencies (#306).

* pak now reports dependency solver failures better in some cases
  (#305, https://github.com/r-lib/pak/issues/474).

* pak now uses locally built CRAN binaries from the cache. Use the
  `?nocache` parameter to opt out from this, or
  `cache_delete(package = ...)` to remove a package from the cache.

# pak 0.4.0

* pak has much improved and more informative error messages now.
  This work is not yet finished, so if you find an unclear error message,
  please open an issue. Thank you!

* The solver is now more robust for non-canonical input (e.g. `DESCRIPTION`
  files) (https://github.com/r-lib/pak/issues/423).

* Better installation output. Standard output and error are now
  collected together (https://github.com/r-lib/pkgdepends/commit/0669f0f8c).

* The solver is now doing a better job when multiple versions of the
  same package are present in the same repository
  (https://github.com/r-lib/actions/issues/559).

* `pkg_name_check()` now works again, it needed a fix after changes at
  https://crandb.r-pkg.org.

* Explicit package names in local and URL package sources, as in
  `package=local::...` or `package=url::...` are now parsed correctly in
  dependencies.

* pak is now more robust to `Archs` fields missing from the CRAN
  metadata for packages with compiled code
  (https://github.com/r-lib/pak/issues/448).

* `url::` packages now always work correctly, even if the digest package is
  not installed (https://github.com/r-lib/pak/issues/433).

* pak is now more robust when installing packages from subdirectories
  of GitHub repositories (https://github.com/r-lib/pak/issues/431,
  @paleolimbot).

* Parameters `?reinstall`, `?source` and `?ignore` now work correctly when
  specified in the `package=?parameter` format (#294).

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
  old version was installed in the different Docker later
  (https://github.com/r-lib/pak/issues/251)

* Update R version -> Bioconductor version mapping. R 4.2.x now maps to
  Bioconductor 3.16.

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
