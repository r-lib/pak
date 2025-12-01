# Install packages from CRAN, Bioconductor, GitHub, URLs, etc.

Install packages from CRAN, Bioconductor, GitHub, URLs, etc. Learn how
to tell pak which packages to install, and where those packages can be
found.

If you want a quick overview of package sources, see "[Get started with
pak](https://pak.r-lib.org/reference/get-started.md)".

## Details

### Package references

Many pkgdepends and pak functions take package names as arguments. E.g.
[`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md)
takes the names of the packages to install and
[`pak::pkg_deps_tree()`](https://pak.r-lib.org/reference/pkg_deps_tree.md)
takes the names of the packages to draw dependency trees for.

Most of these functions can also take more generic *package references*
instead of package names. A package reference also tells pak where to
find the package source.

To specify a package source, use its name as a prefix, with a `::`
separator. E.g. `cran::mypkg` means the `mypkg` package from CRAN.

A package name is a special package reference that implicitly specifies
the configured CRAN(-like) repositories as the package source (we call
this the `standard` package source). So `mypkg` is equivalent to
`standard::mypkg` and pak looks for mypkg in any of the configured
CRAN-like repositories. If you did not explicitly specify any CRAN-like
repositories (e.g. with `options("repos")`), then pak uses the CRAN and
Bioconductor repositories by default.

This is the list of the currently supported package sources. We will
discuss each in detail below.

- `cran`: a CRAN package.

- `bioc`: a Bioconductor package.

- `standard`: a package from a configured CRAN-like repository.

- `github`: a package from GitHub.

- `gitlab`: a package from GitLab.

- `git`: a package in a Git repository.

- `local`: a local package file or directory.

- `url`: an URL that points to a package archive.

- `installed`: an installed package.

- `deps`: the dependencies of a local package file or directory.

- `any`: a special reference type that accepts a package from any
  source. See below.

- `param`: a special reference to change how other references are
  downloaded or installed. See "Parameters" below.

#### Shorthands

To save typing, you do not always need to fully specify the package
source in a package reference. You have seen before that a package name
implicitly has a `standard` package source. Here are the complete rules
for such shorthands, in the order they are applied:

If the package reference is

- a valid package name, or a package name with an `@` version
  specification, the `standard` package source is used. E.g. `pkg` is
  equivalent to `standard::pkg` and `pkg@1.0` is equivalent to
  `standard::pkg@1.0`.

- a valid `github` ref type without the `github::` prefix, then `github`
  is used. E.g. `user/repo` is equivalent to `github::user/repo` and
  `user/repo@tag` is equivalent to `github::user/repo@tag`, etc.

- a GitHub URL (see below) without the `github::` prefix, then `github`
  is used.

- a path that starts with `.` or `/` or `\` or `~`, then `local` is
  used. (pak does not check if the path exists.)

- of the form `<package-name>=?<parameters>`, then it will be the
  special `param` type. See "Parameters" below.

If the package reference does not have an explicit package source, and
the package source cannot be determined from these rules, then pak
throws an error.

#### Package names

When pak is looking up the dependencies of a package, it needs to be
able to determine the name of the dependency from the package reference.
This is sometimes not easy for dependencies in `Remotes` (or similar)
fields.

- For `github::` and `gitlab::` dependencies pak assumes that the
  package name is the same as the name of the repository. If this does
  not hold, then you need to specify the package name explicitly, using
  a `<package>=` prefix. E.g. `pins=rstudio/pins-r`. To specify both the
  package source type and the package name at the same time, write it
  like this: `pins=github::rstudio/pins-r`.

- For `git::` dependencies, pak assumes that the package name is the
  same as the last component of the repository. If this does not hold,
  then you need to specify the package name explicitly, using a
  `<package>=` prefix. E.g.
  `pins=git::https://github.com/rstudio/pins-r`.

- For `local::` dependencies, you always need to specify the package
  name explicitly. E.g. `pins=local::~/works/pins`.

- For `url::` dependencies, you always need to specify the package name
  explicitly. E.g.
  `ggplot2=url::https://cloud.r-project.org/src/contrib/...`.

#### Parameters

Package references may have optional parameters, added after a question
mark. Different parameters are separated by an ampersand (`&`)
character. (This is very similar to how HTTP URLs take query
parameters.)

Parameters may be flags that turn on some behavior, or they can have a
string value, assigned with an equal sign (`=`). If no value is
assigned, then a `true` value is assumed. For example, these two package
references are equivalent:

 

    cran::testthat?source&nocache
    cran::testthat?source=true&nocache=true

##### Parameters for downstream packages

pak allows specifying parameters for downstream packages, using the
`<package>=?<params>` special package reference, where `package` is the
name of the package, and `<params>` are the parameters, as above. This
is useful if you want to add a parameter to a downstream dependency.

For example, to install ggplot2, and always reinstall its cli package
dependency, you could use the `ggplot2` and `cli=?reinstall` package
references. The latter tells pak to always reinstall cli, even if it is
already installed.

##### Currently supported parameters

- `ignore` is a flag parameter. If specified, the package is ignored.
  This usually makes sense in the `packagename=?ignore` form, to ignore
  a downstream soft dependency. If all versions of a hard dependency are
  ignored that will lead to a solution error.

- `ignore-before-r` is a version number parameter. The package will be
  ignored on R versions that are older than the specified one. E.g.
  `Matrix=?ignore-before-r=4.1.2` will ignore the Matrix package on R
  versions that are older than 4.1.2. This parameter really only makes
  sense in the `packgename=?ignore-before-r` form.

- `ignore-unavailable` is a flag. It can only be specified for soft
  dependencies. If specified and the package is not available, it will
  be ignored. This parameter really only makes sense in the
  `packagename=?ignore-unavailable` form.

- `source` is a flag parameter. If specified, then a source R package is
  requested from a CRAN-like repository. For package installations,
  `source` always triggers a re-install. In other words, `source`
  implies the `reinstall` parameter. This parameter is supported for
  `bioc::`, `cran::` and `standard::` remote types, and it is ignored
  for others.

- `reinstall` requests a re-install for package installations. It is
  supported by the `bioc::`, `cran::`, `git::`, `github::`, `gitlab::`,
  `local::`, `standard::`, and `url::` remote types.

- `nocache` will ignore the package cache. It will always download the
  package file, and it will not add the downloaded (and built)
  package(s) to the package cache. It is supported by the `bioc::`,
  `cran::`, `git::`, `github::`, `gitlab::`, `standard::` and `url::`
  remote types.

#### Package source details

##### CRAN packages (`cran::`)

A package from CRAN. Full syntax:

 

    [cran::]<package>[@[>=]<version> | @current | @last]

- `<package>` is a valid package name.

- `<version>` is a version or a version requirement.

Examples:

 

    forecast
    forecast@8.8
    forecast@>=8.8
    cran::forecast
    forecast@last
    forecast@current

Note: pak currently parses the version specification part (everything
after `@`), but does not use it.

##### Bioconductor packages (`bioc::`)

A package from Bioconductor. The syntax is the same as for CRAN
packages, except for the prefix.

 

    [bioc::]<package>[@[>=]<version> | @current | @last]

##### Standard packages (`standard::`)

These are packages either from CRAN or Bioconductor, the full syntax is
the same as for CRAN packages, except for the prefix:

 

    [standard::]<package>[@[>=]<version> | current | last]

##### GitHub packages (`github::`)

Packages from a GitHub repository. Full syntax:

 

    [<package>=][github::]<username>/<repository>[/<subdir>][<detail>]

- `<package>` is the name of the package. If this is missing, then the
  name of the repository is used.

- `<username>` is a GitHub username or organization name.

- `<repository>` is the name of the repository.

- `<subdir>` specifies an optional subdirectory, if the package is
  within a subdirectory in the repository.

- `<detail>` specifies a certain version of the package, see below.

`<detail>` may specify:

- a Git branch, tag or (prefix of) a commit hash: `@<commitish>`;

- a pull request: `#<pull-request>`; or

- the latest release: `@*release`.

If `<detail>` is missing, then the latest commit of the *default* branch
is used.

Examples:

 

    r-lib/crayon
    github::r-lib/crayon
    r-lib/crayon@84be6207
    r-lib/crayon@branch
    r-lib/crayon#41
    r-lib/crayon@release

For convenience, GitHub HTTP URLs can also be used to specify a package
from GitHub. Examples:

 

    https://github.com/r-lib/withr
    # A branch:
    https://github.com/r-lib/withr/tree/ghactions
    # A tag:
    https://github.com/r-lib/withr/tree/v2.1.1
    # A commit:
    https://github.com/r-lib/withr/commit/8fbcb548e316
    # A pull request:
    https://github.com/r-lib/withr/pull/76
    # A release:
    https://github.com/r-lib/withr/releases/tag/v2.1.0

A GitHub remote string can also be used instead of a URL, for example:
`git@github.com:r-lib/pak.git`

##### GitLab packages (`gitlab::`)

Packages from a GitLab repository. Full syntax:

 

    [<package>=][gitlab::]<project-path>/<repository>[/-/<subdir>][<detail>]

- `<package>` is the name of the package. If this is missing, then the
  name of the repository is used.

- `<project-path>` is typically the GitLab user or group name, but it
  may contain subgroups.

- `<repository>` is the name of the repository, or the project in GitLab
  terminology. GitLab
  [subgroups](https://docs.gitlab.com/ee/user/group/subgroups/) are
  fully supported.

- `<subdir>` specifies an optional subdirectory, if the package is
  within a subdirectory in the repository. Note that for GitLab, this
  must come after a `/-` prefix, to be able to distinguish it from
  subgroups.

- `<detail>` may specify a Git branch, tag or (prefix of) a commit hash.

If `<detail>` is missing, then the latest commit of the *default* branch
is used.

`gitlab::` supports Git submodules, see the `git-submodules`
configuration entry.

Examples:

 

    gitlab::gaborcsardi/cli
    gitlab::r-hub/filelock@main
    gitlab::group/subgroup/subsubgroup/project/-/subdir@ref

##### Packages in Git repositories (`git::`)

Full syntax:

 

    [<package>=]git::https?://<host>[<detail>]

- `<package>` is the name of the package. If this is missing, then the
  last component of the `<host>` is used.

- `<host>` is the host name and path of the Git repository. Some Git
  repositories need the `.git` suffix here, others are more forgiving.

- `<detail>` specifies a certain version of the package: a Git branch,
  tag or (prefix of) a commit hash: `@<commitish>`.

If `<detail>` is missing, then the latest commit of the *default* branch
is used.

`git::` supports Git submodules, see the `git-submodules` configuration
entry.

Examples:

 

    git::https://github.com/r-lib/crayon
    git::https://github.com/r-lib/crayon.git
    git::https://github.com/r-lib/crayon.git@84be6207
    git::https://github.com/r-lib/crayon.git@branch
    git::https://gitlab.com/gaborcsardi/cli.git

Note that pak has a built-in Git client, and does **not** require a
system Git installation.

If the system has Git installed, then pak will use the credentials
stored in the configured Git credential store, automatically, via the
gitcreds package.

##### Local packages (`local::`)

A path that refers to a package file built with `R CMD build`, or a
directory that contains a package. Full syntax:

 

    local::<path>

For brevity, you can omit the `local::` prefix, if you specify an
absolute path, a path from the user's home directory, starting with `~`,
or a relative path starting with `./` or `.\\`.

A single dot (`"."`) is considered to be a local package in the current
working directory.

Examples:

 

    local::/foo/bar/package_1.0.0.tar.gz
    local::/foo/bar/pkg
    local::.
    /absolute/path/package_1.0.0.tar.gz
    ~/path/from/home
    ./relative/path
    .

If you specify a local package in a dependency (i.e. in `DESCRIPTION`),
then you also need to specify the name of the package, see "Package
names" above.

##### URLs (`url::`)

You can use `url::` to refer to URLs that hold R package archives (i.e.
properly built with `R CMD build`), or compressed directories of package
trees (i.e. not built with `R CMD build`). pak will figure out if it
needs to run `R CMD build` on the package first.

This remote type supports `.tar.gz` and `.zip` files.

Note that URLs are not ideal remote types, because pak needs to download
the package file to resolve its dependencies. When this happens, it puts
the package file in the cache, so no further downloads are needed when
installing the package later.

Examples:

 

    url::https://cloud.r-project.org/src/contrib/Archive/cli/cli_1.0.0.tar.gz
    url::https://github.com/tidyverse/stringr/archive/HEAD.zip

If you specify a package from a URL in a dependency (i.e. in
`DESCRIPTION`), then you also need to specify the name of the package,
see "Package names" above.

##### Installed packages (`installed::`)

This is usually used internally, but can also be used directly. Full
syntax:

 

    installed::<path>/<package>

- `<path>` is the library the package is installed to.

- `<package>` is the package name.

Example:

 

    installed::~/R/3.6/crayon

##### Package dependencies (`deps::`)

Usually used internally, it specifies the dependencies of a local
package. It can be used to download or install the dependencies of a
package, without downloading or installing the package itself. Full
syntax:

 

    deps::<path>

Examples:

 

    deps::/foo/bar/package_1.0.0.tar.gz
    deps::/foo/bar/pkg
    deps::.

##### `any::` packages

Sometimes you need to install additional packages, but you don't mind
where they are installed from. Here is an example. You want to install
cli from GitHub, from `r-lib/cli`. You also want to install glue, and
you don't mind which version of glue is installed, as long as it is
compatible with the requested cli version. If cli specifies the
development version of glue, then that is fine. If cli is fine with the
CRAN version of glue, that's OK, too. If a future version of cli does
not depend on glue, you still want glue installed, from CRAN. The
`any::` reference type does exactly this.

In our example you might write

 

    pak::pkg_install(c("glue", "r-lib/cli"))

first, but this will fail if `rlib/cli` requests (say) `tidyverse/glue`,
because in
[`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md)
`"glue"` is interpreted as `"standard::glue"`, creating a conflict with
`tidyverse/glue`. On the other hand

 

    pak::pkg_install(c("any::glue", "r-lib/cli"))

works, independently of which glue version is requested by cli.

##### Parameter refs (`param::`)

See "Parameters" above.

#### The `Remotes` field

In the `DESCRIPTION` file of an R package you can mark any regular
dependency defined in the `Depends`, `Imports`, `Suggests` or `Enhances`
fields as being installed from a non-standard package source by adding a
package reference to a `Remotes` entry. pak will download and install
the package from the specified location, instead of a CRAN-like
repository.

The remote dependencies specified in `Remotes` are a comma separated
list of package sources:

 

    Remotes: <pkg-source-1>, <pkg-source-2>, [ ... ]

Note that you will still need to add the package to one of the regular
dependency fields, i.e. `Imports`, `Suggests`, etc. Here is a concrete
example that specifies the `r-lib/glue` package:

 

    Imports: glue
    Remotes: r-lib/glue,
      r-lib/httr@v0.4,
      klutometis/roxygen#142,
      r-lib/testthat@c67018fa4970

The CRAN and Bioconductor repositories do not support the `Remotes`
field, so you need to remove this field, before submitting your package
to either of them.
