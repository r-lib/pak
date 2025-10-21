# Install packages based on a lock file

Install a lock file that was created with
[`lockfile_create()`](https://pak.r-lib.org/dev/reference/lockfile_create.md).

## Usage

``` r
lockfile_install(lockfile = "pkg.lock", lib = .libPaths()[1], update = TRUE)
```

## Arguments

- lockfile:

  Path to the lock file.

- lib:

  Library to carry out the installation on.

- update:

  Whether to online install the packages that either not installed in
  `lib`, or a different version is installed for them.

## See also

Other lock files:
[`lockfile_create()`](https://pak.r-lib.org/dev/reference/lockfile_create.md)
