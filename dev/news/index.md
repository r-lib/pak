# Changelog

## pak (development version)

## pak 0.9.1

CRAN release: 2025-12-01

- pak is now more tolerant with extraneous whitespace in `PACKAGES*`
  metadata files (<https://github.com/r-lib/pak/issues/785>).

- pak now builds again on FreeBSD
  ([\#790](https://github.com/r-lib/pak/issues/790)).

- Installing dependencies of a package file with `deps::<filename>`
  works again ([\#784](https://github.com/r-lib/pak/issues/784)).

- pak now always uses the correct working directory in the subprocess;
  this was a common source of errors.

- New (nightly) binary builds for aarch64 Windows.

- We do not build binary builds for the release candidate of pak any
  more, only for stable and devel pak. `rc` is an alias to `stable` now.

## pak 0.9.0

CRAN release: 2025-05-27

- pak now supports HTTP basic authentication for CRAN-like repositories.
  See ‘Authenticated repositories’ in the reference manual.

- New function
  [`scan_deps()`](https://pak.r-lib.org/dev/reference/scan_deps.md) to
  auto-detect package dependencies from R code. `deps::.` automatically
  uses detected dependencies now if no `DESCRIPTION` file is found.

- The dependency solver now uses better heuristics, that works better
  (=faster) with multiple repositories with large overlaps
  (<https://github.com/r-lib/pkgdepends/pull/392>).

- pak is now much better at detecting the correct Linux distribution
  when deciding about PPM and system requirements support.

- pak now uses the `use_bioconductor` configuration option in `meta_*()`
  and `repo_*()` functions
  ([\#295](https://github.com/r-lib/pak/issues/295),
  [\#726](https://github.com/r-lib/pak/issues/726),
  [@meztez](https://github.com/meztez)).

## pak 0.8.0.2

CRAN release: 2025-04-08

No changes.

## pak 0.8.0.1

CRAN release: 2025-01-16

No changes.

## pak 0.8.0

CRAN release: 2024-08-26

- [`pkg_deps()`](https://pak.r-lib.org/dev/reference/pkg_deps.md) now
  accepts a vector of package names.

- The metadata cache now does not use source URLs for packages in
  `Archive` on Posit Package Manager repositories. This URLs may serve a
  different package, even a source package when the main URL for the
  same package serves a binary package. The alternative URLs are not
  needed on PPM, anyway, because PPM is in a consistent state w.r.t.
  metadata and package files
  ([\#623](https://github.com/r-lib/pak/issues/623)).

- pak now supports `gitlab::` package sources better, by adding explicit
  syntax to specify subdirectories
  (<https://github.com/r-lib/pkgdepends/issues/353>,
  [@dgkf](https://github.com/dgkf)).

- `gitlab::` and `git::` package sources now support git submodules if
  the `git-submodules` configuration option is set to `TRUE`. See
  [`?"pak-config"`](https://pak.r-lib.org/dev/reference/pak-config.md)
  (<https://github.com/r-lib/pkgdepends/issues/354>).

- The new `?ignore-unavailable` parameter makes it easy to ignore soft
  dependencies that are unavailable
  ([\#606](https://github.com/r-lib/pak/issues/606)).

- pak now automatically ignores soft dependencies that have an
  incompatible OS type (`OS_type` entry in `DESCRIPTION`) when
  installing packages.

- [`repo_add()`](https://pak.r-lib.org/dev/reference/repo_add.md) and
  the `ppm_*()` functions,
  e.g. [`ppm_snapshots()`](https://pak.r-lib.org/dev/reference/ppm_snapshots.md),
  now work again after the PPM API changes
  (<https://github.com/r-lib/pkgcache/issues/110>,
  <https://github.com/r-lib/pkgcache/issues/115>).

## pak 0.7.2

CRAN release: 2024-03-17

- pak now supports using parameters for all packages with the
  `*=?<param>` form. E.g. `*=?source` installs all packages from source.

- pak now supports R 4.4.0 again, and also Rtools44.

## pak 0.7.1

CRAN release: 2023-12-10

- pak can now handle the case when `Config/Needs/*` dependencies are
  requested for package from a repository.

- pak uses safer `*printf()` format strings now.

## pak 0.7.0

CRAN release: 2023-11-17

- pak now correctly handles the latest GitHub release with the
  `@*release` notation ([@pawelru](https://github.com/pawelru),
  <https://github.com/r-lib/pkgdepends/pull/321>)

- pak now correctly handles having multiple instances of the same
  package in the metadata, with different R version requirements
  ([\#534](https://github.com/r-lib/pak/issues/534),
  [\#538](https://github.com/r-lib/pak/issues/538),
  <https://github.com/r-lib/pkgdepends/pull/331>).

- `git::` package references work better now for Azure DevOps
  ([@jameslairdsmith](https://github.com/jameslairdsmith),
  <https://github.com/r-lib/pkgdepends/pull/333>,
  <https://github.com/r-lib/pkgdepends/pull/342>).

- pak now does a better job at accepting installed packages, and avoids
  reinstalling more packages than needed when using a lock file
  (<https://github.com/r-lib/actions/issues/759>,
  <https://github.com/r-lib/pkgdepends/pull/338>).

## pak 0.6.0

CRAN release: 2023-08-29

- pak now requires R \>= 3.5.0.

- Many improvements in system requirements support:

  - New functions:
    - [`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md):
      calculate system requirements of packages.
    - [`sysreqs_db_list()`](https://pak.r-lib.org/dev/reference/sysreqs_db_list.md),
      [`sysreqs_db_match()`](https://pak.r-lib.org/dev/reference/sysreqs_db_match.md),
      [`sysreqs_db_update()`](https://pak.r-lib.org/dev/reference/sysreqs_db_update.md):
      query the system requirements database.
    - [`sysreqs_list_system_packages()`](https://pak.r-lib.org/dev/reference/sysreqs_list_system_packages.md),
      [`sysreqs_check_installed()`](https://pak.r-lib.org/dev/reference/sysreqs_check_installed.md),
      [`sysreqs_fix_installed()`](https://pak.r-lib.org/dev/reference/sysreqs_check_installed.md):
      query and install missing system packages.
    - [`sysreqs_platforms()`](https://pak.r-lib.org/dev/reference/sysreqs_platforms.md):
      list supported platforms.
  - The installation proposal, printed before installation, now includes
    required and missing system packages, on supported platforms.
  - New `sysreqs_platform` configuration option to override the
    auto-detected platform.
  - Faster, asynchronous system requirements lookup.
  - pak now does not reinstall system requirements by default, if they
    are already installed. (You can force a reinstall/upgrade with the
    `sysreqs_update` configuration option.)

- New `gitlab::` package source to install packages from GitLab
  (<https://github.com/r-lib/pkgdepends/pull/315>).

- pak now correctly parses multiple `git::` packages at once
  (<https://github.com/r-lib/pkgdepends/issues/318>).

- `git::` package sources now support version 1 of the git protocol.
  E.g. the Bioconductor git repositories now work:
  `git::https://git.bioconductor.org/packages/limma`
  (<https://github.com/r-lib/pkgdepends/issues/314>).

- The `platforms` config parameter now works correctly with `deps::`
  package sources ([\#522](https://github.com/r-lib/pak/issues/522)).

- New `include_linkingto` config parameter to always include `LinkingTo`
  packages in the solution, even for binaries
  (<https://github.com/r-lib/pkgdepends/issues/485>).

- [`pkg_name_check()`](https://pak.r-lib.org/dev/reference/pkg_name_check.md)
  now does not include Acromine results, because the web site was
  unstable.

- In [`repo_add()`](https://pak.r-lib.org/dev/reference/repo_add.md) and
  [`repo_resolve()`](https://pak.r-lib.org/dev/reference/repo_add.md)
  the `MRAN@` prefix is now deprecated and resolves to PPM, because MRAN
  will be retired soon. See more at
  <https://posit.co/blog/migrating-from-mran-to-posit-package-manager/>.

- The metadata cache now has `SystemRequirements` information for
  Bioconductor packages.

## pak 0.5.1

CRAN release: 2023-04-27

- No user visible changes.

## pak 0.5.0

CRAN release: 2023-04-20

- The `meta_*()` functions now consider the `platforms`, `cran_mirror`
  and `r_versions` config entries, see
  [`?"pak-config"`](https://pak.r-lib.org/dev/reference/pak-config.md).

- Better Posit Package Manager (PPM) support. New `+ppm_has_binaries()`,
  [`ppm_r_versions()`](https://pak.r-lib.org/dev/reference/ppm_r_versions.md),
  [`ppm_repo_url()`](https://pak.r-lib.org/dev/reference/ppm_repo_url.md),
  [`ppm_snapshots()`](https://pak.r-lib.org/dev/reference/ppm_snapshots.md)
  and
  [`ppm_platforms()`](https://pak.r-lib.org/dev/reference/ppm_platforms.md)
  functions to help interacting with PPM. See the new ‘pkgcache and
  Posit Package Manager on Linux’ article at
  <https://r-lib.github.io/pkgcache>. (r-lib/pkgcache#47 and
  r-lib/pkgdepends#186).

- New
  [`system_r_platform()`](https://pak.r-lib.org/dev/reference/system_r_platform.md)
  and
  [`system_r_platform_data()`](https://pak.r-lib.org/dev/reference/system_r_platform.md)
  functions to query the current platform.

- pak now support git repositories as package references. E.g.
  `git::https://github.com/r-lib/pak.git`.

- pak now supports versioned CRAN packages, e.g. `dplyr@1.1.1` will
  always install dplyr 1.1.1. Note that only CRAN packages are
  supported, Bioconductor packages are not (yet).

- pak now has an alternative system requirements lookup implementation.
  It supports Fedora and Debian systems as well, in addition to Debian,
  Ubuntu, SUSE and RedHat derivatives. You can switch to this
  implementation by setting the `R_PKG_SYSREQS2` environment variable to
  `true`.

- pak now does a better job looking up dependencies for hand-selected
  dependency types. E.g. `dependencies = "LinkingTo"`.

- pak now removes `?ignore`-d packages from dependencies, and uses the
  correct version comparison for `?ignore-before.r`
  (<https://github.com/r-lib/actions/issues/708>).

- pak now does not fail for circular soft dependencies
  ([\#306](https://github.com/r-lib/pak/issues/306)).

- pak now reports dependency solver failures better in some cases
  ([\#305](https://github.com/r-lib/pak/issues/305),
  <https://github.com/r-lib/pak/issues/474>).

- pak now uses locally built CRAN binaries from the cache. Use the
  `?nocache` parameter to opt out from this, or
  `cache_delete(package = ...)` to remove a package from the cache.

## pak 0.4.0

CRAN release: 2023-01-15

- pak has much improved and more informative error messages now. This
  work is not yet finished, so if you find an unclear error message,
  please open an issue. Thank you!

- The solver is now more robust for non-canonical input
  (e.g. `DESCRIPTION` files)
  (<https://github.com/r-lib/pak/issues/423>).

- Better installation output. Standard output and error are now
  collected together
  (<https://github.com/r-lib/pkgdepends/commit/0669f0f8c>).

- The solver is now doing a better job when multiple versions of the
  same package are present in the same repository
  (<https://github.com/r-lib/actions/issues/559>).

- [`pkg_name_check()`](https://pak.r-lib.org/dev/reference/pkg_name_check.md)
  now works again, it needed a fix after changes at
  <https://crandb.r-pkg.org>.

- Explicit package names in local and URL package sources, as in
  `package=local::...` or `package=url::...` are now parsed correctly in
  dependencies.

- pak is now more robust to `Archs` fields missing from the CRAN
  metadata for packages with compiled code
  (<https://github.com/r-lib/pak/issues/448>).

- `url::` packages now always work correctly, even if the digest package
  is not installed (<https://github.com/r-lib/pak/issues/433>).

- pak is now more robust when installing packages from subdirectories of
  GitHub repositories (<https://github.com/r-lib/pak/issues/431>,
  [@paleolimbot](https://github.com/paleolimbot)).

- Parameters `?reinstall`,
  [`?source`](https://rdrr.io/r/base/source.html) and `?ignore` now work
  correctly when specified in the `package=?parameter` format
  ([\#294](https://github.com/r-lib/pak/issues/294)).

- The `?ignore` parameter works correctly now.

- Dependency resolution now does not fail if a package is not found.

- pak can now install `url::` remotes from GitHub.

- pak now does not fail when the package of a `.tar.gz` GitHub snapshot
  is in a subdirectory, or in a subdirectory of a subdirectory.

- pak now errors early if it cannot deduce the name of the package from
  a `Remotes` or `Config/Needs/*` entry.

- Solver failures now include details in some cases where previously
  they did not.

- pak can now update packages in Docker containers where the old version
  was installed in the different Docker later
  (<https://github.com/r-lib/pak/issues/251>)

- Update R version -\> Bioconductor version mapping. R 4.2.x now maps to
  Bioconductor 3.16.

## pak 0.3.1

CRAN release: 2022-09-08

- The `?ignore` parameter works correctly now.

- Dependency resolution now does not fail if a package is not found.

- pak can now install `url::` remotes from GitHub.

- pak now does not fail when the package of a `.tar.gz` GitHub snapshot
  is in a subdirectory, or in a subdirectory of a subdirectory.

- pak now errors early if it cannot deduce the name of the package from
  a `Remotes` or `Config/Needs/*` entry.

- Solver failures now include details in some cases where previously
  they did not.

- pak can now update packages in Docker containers where the old version
  was installed in the different Docker later
  ([\#251](https://github.com/r-lib/pak/issues/251))

- pak errors are now user friendlier and better formatted.

- pak now does not load tibble and its dependencies in the pak
  subprocess, so their dlls are not locked by the pak subprocess on
  Windows.

- pak now does not fail when installing a package that uses a non-UTF-8
  encoding on R 4.3.x and later
  ([\#404](https://github.com/r-lib/pak/issues/404)).

## pak 0.3.0

CRAN release: 2022-04-11

- pak functions that used to return tibbles return data frames now.
  While data frames and tibbles are very similar, they are not
  completely compatible. To convert the outputs of pak functions to
  tibbles call the
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  function on them. If the pillar package is loaded, it improves the
  printing of the returned data frames.

  Relatedly,
  [`pak::pak_install_extra()`](https://pak.r-lib.org/dev/reference/pak_install_extra.md)
  installs pillar now, instead of tibble.

- pak now supports `file://` repositories.

- pak now uses HTTP 1.1 to download packages on Linux, in addition to
  macOS. This fixes HTTP issues with some servers
  ([\#358](https://github.com/r-lib/pak/issues/358)).

- New `?ignore-before-r` parameter to ignore optional dependencies that
  need a newer R version
  (<https://github.com/r-lib/pkgdepends/issues/243>).

- New `?ignore` parameter to ignore an optional dependency.

- Allow specifying downstream package parameters with the
  `package=?param` syntax.

- [`lockfile_install()`](https://pak.r-lib.org/dev/reference/lockfile_install.md)
  now works better for `any::` refs, and pak always install the version
  it has planned for.

- System requirement installation is now more robust and works for Unix
  shell expressions ([\#347](https://github.com/r-lib/pak/issues/347)).

- CRAN-like resolution is more robust now if a repository is missing the
  usual metadata.

- The lock file is pretty JSON now.

- pak now handles all version requirement types properly: ‘\<’, ‘\<=’,
  `==`, `>=`, `>`.

- The dependency solver now uses better heuristics and does not
  (effectively) freeze if multiple repositories have multiple versions
  of the same packages (e.g. RSPM and CRAN)
  (<https://github.com/r-lib/pkgdepends/pull/277>)

## pak 0.2.1

CRAN release: 2021-12-20

No user visible changes.

## pak 0.2.0

CRAN release: 2021-12-01

Lots of news, too much to list. This is a completely new package now.

## pak 0.1.2

CRAN release: 2019-02-19

First version on CRAN.
