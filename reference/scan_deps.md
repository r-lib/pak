# Scan R code for dependent packages

Scan all R files of a project or directory for packages used within
them. It parses R code to find
[`library(package)`](https://rdrr.io/r/base/library.html),
`package::func()`, and similar calls that imply package dependencies.
See details below.

## Usage

``` r
scan_deps(path = NULL, root = NULL)
```

## Arguments

- path:

  Files and/or directories to scan. Defaults to the current project,
  detected by finding the first parent directory of the current working
  directory, that contains a file or directory called DESCRIPTION, .git,
  .Rproj.user, renv.lock, or renv. (Note that this is different from
  [`renv::dependencies()`](https://rstudio.github.io/renv/reference/dependencies.html),
  which only scans the current working directory by default!)

  If `path` is not `NULL`, then only the specified files and directories
  are scanned, the directories recursively. In this case the `root`
  argument is used as the project root, to find `.gitignore` and
  `.renvignore` files. All entries of `path` must be within the `root`,
  the project root.

- root:

  The root directory of the project. It is used to find the `.gitignore`
  and `.renvignore` files. By default the same algorithm is used to
  detect this as for `path`. If `path` is specified and it is not within
  the detected or specified `root`, `scan_path()` throws an error.

## Value

Data frame with columns:

- `path`: Path to the file in which the dependencies was found.

- `package`: Detected package dependency. Typically a package name, but
  it can also be a package reference, e.g. a package from GitHub.

- `type`: Dependency type. It is `"prod"`, `"test"` or `"dev"`. See
  'Dependency types' below.

- `code`: The piece of code the dependency was extracted from.

- `start_row`: Start row of the code the dependency was extracted from.

- `start_column`: Start column of the code the dependency was extracted
  from.

- `start_byte`: Start byte of the code the dependency was extracted
  from.

Note the data frame may contain the same package multiple times, if it
was detected multiple times, e.g. multiple
[`library()`](https://rdrr.io/r/base/library.html) calls load the same
package.

## Detected dependencies

`scan_deps()` detects package dependencies from these R expressions:

- [`library()`](https://rdrr.io/r/base/library.html),
  [`require()`](https://rdrr.io/r/base/library.html),
  [`loadNamespace()`](https://rdrr.io/r/base/ns-load.html) and
  `requireNamespace` calls.

- `::` and `:::` operators.

- Any of the calls in this list in R code from R markdown or quarto `R`
  and `Rscript` (case insensitive) code blocks or inline R code.

- A dependency on the methods package is inferred from finding
  `setClass()` and/or `setGeneric()` calls.

- [`xfun::pkg_attach()`](https://rdrr.io/pkg/xfun/man/pkg_attach.html)
  and
  [`xfun::pkg_attach2()`](https://rdrr.io/pkg/xfun/man/pkg_attach.html)
  calls.

- `pacman::p_load()` calls.

- `modules::import()` and `modules::module()` calls.

- `import::from()`, `import::here()` and `import::into()` calls.

- `box::use()` calls.

- `targets::tar_option_set(packages = ...)` calls.

- Any of the calls in this list in R code from
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  strings.

- A dependency on the svglite package is inferred from
  `ggplot2::ggsave()` calls saving `.svg` files.

- Dependencies from `parsnip::set_engine()` calls, the default engine to
  package mapping is:

  - `"glm"` -\> stats,

  - `"glmnet"` -\> glmnet,

  - `"keras"` -\> keras,

  - `"kknn"` -\> kknn,

  - `"nnet"` -\> nnet,

  - `"rpart"` -\> rpart,

  - `"spark"` -\> sparklyr,

  - `"stan"` -\> rstanarm. You can override the default mapping by
    setting the `renv.parsnip.engines` option to a named list.

- A dependency on the xml2 package is inferred from using the "Junit"
  reporter (`JunitReporter`) from the testthat package.

- A dependency on the ragg package is inferred from setting the default
  knitr device (`dev` option) to `"ragg_png"`.

- A dependency on the hexbin package is inferred from using
  `ggplot2::geom_hex()`.

- A custom symbol name to package name mapping can be defined in the
  `renv.dependencies.database` option. This must be a named list of
  named lists, where the outer names are package names, the inner names
  are function or object names, and the values are package names. E.g.

      options(renv.dependencies.database = list(
        ggplot2 = list(geom_hex = "hexbin"),
        testthat = list(JunitReporter = "xml2")
      ))

## Dependency types

`scan_deps()` classifies package dependencies into three groups, based
on which files they were found:

- Production dependencies: `"prod"`.

- Test dependencies: `"test"`.

- Development dependencies: `"dev"`.

## Examples

``` r
if (FALSE) {
scan_deps("myproject")
scan_deps("myproject")[]
}
```
