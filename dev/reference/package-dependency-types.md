# Various types of R package dependencies

Various types of R package dependencies

## Details

R packages may have various types of dependencies, see [Writing R
Extensions](https://cran.r-project.org/doc/manuals/R-exts.html).pak
groups dependencies into three groups:

- hard dependencies: "Depends", "Imports", and "LinkingTo",

- soft dependencies: "Suggests" and "Enhances",

- extra dependencies, see below.

pak supports concise ways of specifying which types of dependencies of a
package should be installed. It is similar to how
[`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)
interprets its `dependencies` argument.You typically use one of these
values:

- `NA` or `"hard"` to install a package and its required dependencies,

- `TRUE` to install all required dependencies, plus optional and
  development dependencies.

If you need more flexibility, the full description of possible values
for the `dependencies` argument are:

- `TRUE`: This means all hard dependencies plus `Suggests` for direct
  installations, and hard dependencies only for dependent packages.

- `FALSE`: no dependencies are installed at all.

- `NA` (any atomic type, so `NA_character_`, etc. as well): only hard
  dependencies are installed.

- If a list with two entries named `direct` and `indirect`, it is taken
  as the requested dependency types, for direct installations and
  dependent packages.

- If a character vector, then it is taken as the dependency types for
  direct installations, and the hard dependencies are used for the
  dependent packages.

If `"hard"` is included in the value or a list element, then it is
replaced by the hard dependency types. If `"soft"` or `"all"` is
included, then it is replaced by all hard and soft dependency.

### Extra dependencies

pak supports extra dependency types for direct installations not from
CRAN-like repositories. These are specified with a `Config/Needs/`
prefix in the `DESCRIPTION` and they can contain package references,
separated by commas. For example you can specify packages that are only
needed for the pkgdown website of the package:

 

    Config/Needs/website: r-lib/pkgdown

To use these dependency types, you need to specify them in the
`dependencies` argument to pak functions.

Note that `Config/Needs/*` fields are currently *not* used from CRAN
packages, and packages in CRAN-like repositories in general.

Usually you specify that a `Config/Needs/*` dependency type should be
installed together with `"hard"` or `"all"`, to install all hard or soft
dependencies as well.
