# Find the ideal set of packages and versions to install

pak contains a package dependency solver, that makes sure that the
package source and version requirements of all packages are satisfied,
before starting an installation. For CRAN and BioC packages this is
usually automatic, because these repositories are generally in a
consistent state. If packages depend on other other package sources,
however, this is not the case.

## Details

Here is an example of a conflict detected:

    > pak::pkg_install(c("r-lib/pkgcache@conflict", "r-lib/cli@message"))
    Error: Cannot install packages:
      * Cannot install `r-lib/pkgcache@conflict`.
        - Cannot install dependency r-lib/cli@main
      * Cannot install `r-lib/cli@main`.
    - Conflicts r-lib/cli@message

`r-lib/pkgcache@conflict` depends on the main branch of `r-lib/cli`,
whereas, we explicitly requested the `message` branch. Since it cannot
install both versions into a single library, pak quits.

When pak considers a package for installation, and the package is given
with its name only, (e.g. as a dependency of another package), then the
package may have *any* package source. This is necessary, because one R
package library may contain only at most one version of a package with a
given name.

pak's behavior is best explained via an example. Assume that you are
installing a local package (see below), e.g. `local::.`, and the local
package depends on `pkgA` and `user/pkgB`, the latter being a package
from GitHub (see below), and that `pkgA` also depends on `pkgB`. Now pak
must install `pkgB` *and* `user/pkgB`. In this case pak interprets
`pkgB` as a package from any package source, instead of a standard
package, so installing `user/pkgB` satisfies both requirements.

Note that that `cran::pkgB` and `user/pkgB` requirements result a
conflict that pak cannot resolve. This is because the first one *must*
be a CRAN package, and the second one *must* be a GitHub package, and
two different packages with the same cannot be installed into an R
package library.
