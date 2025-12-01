# Frequently Asked Questions

Please take a look at this list before asking questions.

## Package installation

### How do I reinstall a package?

pak does not reinstall a package, if the same version is already
installed. Sometimes you still want a reinstall, e.g. to fix a broken
installation. In this case you can delete the package and then install
it, or use the `?reinstall` parameter:

    pak::pkg_install("tibble")

    #>
    #> → Will install 13 packages.
    #> → All 13 packages (7.68 MB) are cached.
    #> + cli         3.3.0
    #> + crayon      1.5.1
    #> + ellipsis    0.3.2
    #> + fansi       1.0.3
    #> + glue        1.6.2
    #> + lifecycle   1.0.1
    #> + magrittr    2.0.3
    #> + pillar      1.7.0
    #> + pkgconfig   2.0.3
    #> + rlang       1.0.2
    #> + tibble      3.1.7
    #> + utf8        1.2.2
    #> + vctrs       0.4.1
    #> i No downloads are needed, 13 pkgs (7.68 MB) are cached
    #> v Got utf8 1.2.2 (aarch64-apple-darwin20) (209.24 kB)
    #> v Installed cli 3.3.0  (76ms)
    #> v Installed crayon 1.5.1  (87ms)
    #> v Installed ellipsis 0.3.2  (97ms)
    #> v Installed fansi 1.0.3  (103ms)
    #> v Installed glue 1.6.2  (111ms)
    #> v Installed lifecycle 1.0.1  (153ms)
    #> v Installed magrittr 2.0.3  (158ms)
    #> v Installed pillar 1.7.0  (162ms)
    #> v Installed pkgconfig 2.0.3  (87ms)
    #> v Installed rlang 1.0.2  (39ms)
    #> v Installed tibble 3.1.7  (41ms)
    #> v Installed utf8 1.2.2  (39ms)
    #> v Installed vctrs 0.4.1  (32ms)
    #> v 1 pkg + 12 deps: added 13, dld 1 (209.24 kB) [1.8s]

    pak::pkg_install("tibble?reinstall")

    #>
    #> → Will install 1 package.
    #> → The package (724.32 kB) is cached.
    #> + tibble   3.1.7
    #> i No downloads are needed, 1 pkg (724.32 kB) is cached
    #> v Installed tibble 3.1.7  (42ms)
    #> v 1 pkg + 12 deps: kept 11, added 1 [343ms]

### How do I install a dependency from a binary package

Sometimes it is sufficient to install the binary package of an older
version of a dependency, instead of the newer source package that
potentially needs compilers, system tools or libraries.

[`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md) and
[`lockfile_create()`](https://pak.r-lib.org/reference/lockfile_create.md)
default to `upgrade = FALSE`, which always chooses binaries over source
packages, so if you use
[`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md) you
don't need to do anything extra.

The `local_install_*` functions default to `upgrade = TRUE`, as does
[`pak()`](https://pak.r-lib.org/reference/pak.md) with `pkg = NULL`, so
for these you need to explicitly use `upgrade = FALSE`.

### How do I install a package from source?

To force the installation of a source package (instead of a binary
package), use the [`?source`](https://rdrr.io/r/base/source.html)
parameter:

    pak::pkg_install("tibble?source")

    #>
    #> → Will install 1 package.
    #> → The package (672.34 kB) is cached.
    #> + tibble   3.1.7 👷🏼‍♀️🔧
    #> i No downloads are needed, 1 pkg (672.34 kB) is cached
    #> i Building tibble 3.1.7
    #> v Built tibble 3.1.7 (3.1s)
    #> v Installed tibble 3.1.7  (35ms)
    #> v 1 pkg + 12 deps: kept 11, added 1 [4.1s]

### How do I install the latest version of a dependency?

If you want to always install a dependency from source, because you want
the latest version or some other reason, you can use the `source`
parameter with the `<package>=` form: `<package>=?source`. For example
to install tibble, with its cli dependency installed from source you
could write:

    pak::pkg_install(c("tibble", "cli=?source"))

    #>
    #> → Will install 1 package.
    #> → The package (540.04 kB) is cached.
    #> + cli   3.3.0 👷🏽🔧
    #> i No downloads are needed, 1 pkg (540.04 kB) is cached
    #> i Building cli 3.3.0
    #> v Built cli 3.3.0 (4.5s)
    #> v Installed cli 3.3.0  (68ms)
    #> v 1 pkg + 12 deps: kept 11, added 1 [4.9s]

### How do I ignore an optional dependency?

    pak::pkg_install(
      c("tibble", "DiagrammeR=?ignore", "formattable=?ignore"),
      dependencies = TRUE
    )

    #>
    #> i No downloads are needed
    #> v 1 pkg + 12 deps: kept 12 [583ms]

The syntax is

    <packagename>=?ignore

Note that you can only ignore *optional* dependencies, i.e. packages in
`Suggests` and `Enhances`.

## Others

### How can I use pak with renv?

Since version 1.0.0 renv has official support for using pak. This needs
to be enabled with the `renv.config.pak.enabled` option or the
`RENV_CONFIG_PAK_ENABLED` environment variable set to `TRUE`. For more
information see the renv
[documentation](https://rstudio.github.io/renv/reference/config.html?q=pak#renv-config-pak-enabled).
