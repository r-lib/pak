
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zip

> Cross-Platform ‘zip’ Compression

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-lib/zip/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/zip/actions/workflows/R-CMD-check.yaml)
[![](https://www.r-pkg.org/badges/version/zip)](https://www.r-pkg.org/pkg/zip)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/zip)](https://www.r-pkg.org/pkg/zip)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/zip/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/zip?branch=main)
<!-- badges: end -->

## Installation

``` r
install.packages("zip")
```

## Usage

``` r
library(zip)
```

### Creating ZIP files

`zip()` creates a new ZIP archive. (It overwrites the output file if it
exists.) Simply supply all directories and files that you want to
include in the archive.

It makes sense to change to the top-level directory of the files before
archiving them, so that the files are stored using a relative path name.

``` r
zip("sources.zip", c("R", "src"))
file.info("sources.zip")
#>               size isdir mode               mtime               ctime
#> sources.zip 580179 FALSE  644 2023-04-17 13:49:31 2023-04-17 13:49:31
#>                           atime uid gid       uname grname
#> sources.zip 2023-04-17 13:49:31 501  20 gaborcsardi  staff
```

Directories are added recursively by default.

`zip_append()` is similar to `zip()`, but it appends files to an
existing ZIP archive.

### Listing ZIP files

`zip_list()` lists files in a ZIP archive. It returns a data frame:

``` r
zip_list("sources.zip")
#>                                                     filename compressed_size
#> 1                                                         R/               0
#> 2                                             R/assertions.R             151
#> 3                                                R/inflate.R             627
#> 4                                                R/process.R            1793
#> 5                                                  R/utils.R            1202
#> 6                                                    R/zip.R            3274
#> 7                                                       src/               0
#> 8                                                 src/init.c             367
#> 9                                                 src/init.o            1648
#> 10                                        src/install.libs.R             272
#> 11                                              src/Makevars             199
#> 12                                          src/Makevars.win             273
#> 13                                               src/miniz.c           55268
#> 14                                               src/miniz.h           18115
#> 15                                               src/miniz.o          120248
#> 16                                                src/rzip.c            2853
#> 17                                                src/rzip.o           10431
#> 18                                                src/tools/               0
#> 19                                        src/tools/cmdunzip           62958
#> 20                                      src/tools/cmdunzip.c             590
#> 21                                  src/tools/cmdunzip.dSYM/               0
#> 22                         src/tools/cmdunzip.dSYM/Contents/               0
#> 23               src/tools/cmdunzip.dSYM/Contents/Info.plist             304
#> 24               src/tools/cmdunzip.dSYM/Contents/Resources/               0
#> 25         src/tools/cmdunzip.dSYM/Contents/Resources/DWARF/               0
#> 26 src/tools/cmdunzip.dSYM/Contents/Resources/DWARF/cmdunzip           70680
#> 27                                          src/tools/cmdzip           63367
#> 28                                        src/tools/cmdzip.c            1066
#> 29                                    src/tools/cmdzip.dSYM/               0
#> 30                           src/tools/cmdzip.dSYM/Contents/               0
#> 31                 src/tools/cmdzip.dSYM/Contents/Info.plist             303
#> 32                 src/tools/cmdzip.dSYM/Contents/Resources/               0
#> 33           src/tools/cmdzip.dSYM/Contents/Resources/DWARF/               0
#> 34     src/tools/cmdzip.dSYM/Contents/Resources/DWARF/cmdzip           71098
#> 35                                           src/unixutils.c             724
#> 36                                           src/unixutils.o            4329
#> 37                                            src/winutils.c            1949
#> 38                                                 src/zip.c            2830
#> 39                                                 src/zip.h             808
#> 40                                                 src/zip.o           10541
#> 41                                                src/zip.so           66645
#>    uncompressed_size           timestamp permissions    crc32 offset
#> 1                  0 2023-04-17 10:58:10         755 00000000      0
#> 2                398 2023-04-17 10:20:40         644 7f73a9e4     32
#> 3               2174 2023-04-17 10:20:40         644 3b0fdb9e    243
#> 4               6585 2023-04-17 10:20:40         644 99447436    927
#> 5               3915 2023-04-17 10:58:10         644 e653bff7   2777
#> 6              10369 2023-04-17 10:51:34         644 4d620f14   4034
#> 7                  0 2023-04-17 10:20:46         755 00000000   7361
#> 8                962 2023-04-17 10:20:40         644 e1c62c7d   7395
#> 9               3744 2023-04-17 10:20:46         644 6da275de   7818
#> 10               587 2022-03-04 14:05:04         644 4f80df1a   9522
#> 11               525 2022-12-19 13:17:12         644 c8789e48   9858
#> 12               700 2022-03-04 14:05:04         644 373232f9  10115
#> 13            314933 2023-04-17 10:20:40         644 450881b2  10450
#> 14             66871 2023-04-17 10:20:40         644 6c5a6c1e  65775
#> 15            333800 2023-04-17 10:20:46         644 887d9996  83947
#> 16             10857 2023-04-17 10:20:40         644 a2c358b5 204252
#> 17             23448 2023-04-17 10:20:46         644 ea86cde3 207161
#> 18                 0 2023-04-17 10:20:46         755 00000000 217648
#> 19            228469 2023-04-17 10:20:46         755 57376bd5 217688
#> 20              1343 2022-03-04 14:05:04         644 1a6e34f1 280710
#> 21                 0 2023-04-17 09:50:00         755 00000000 281366
#> 22                 0 2023-04-17 09:50:00         755 00000000 281420
#> 23               637 2023-04-17 10:20:46         644 294928f0 281483
#> 24                 0 2023-04-17 09:50:00         755 00000000 281876
#> 25                 0 2023-04-17 09:50:00         755 00000000 281949
#> 26            177363 2023-04-17 10:20:46         644 9d6e0672 282028
#> 27            228627 2023-04-17 10:20:46         755 0aa7342d 352811
#> 28              2909 2022-03-04 14:05:04         644 bfb4d8f3 416240
#> 29                 0 2023-04-17 09:50:00         755 00000000 417370
#> 30                 0 2023-04-17 09:50:00         755 00000000 417422
#> 31               635 2023-04-17 10:20:46         644 c3375fb9 417483
#> 32                 0 2023-04-17 09:50:00         755 00000000 417873
#> 33                 0 2023-04-17 09:50:00         755 00000000 417944
#> 34            178267 2023-04-17 10:20:46         644 d905adc8 418021
#> 35              1944 2022-03-04 14:05:04         644 d38da4b6 489218
#> 36              9592 2023-04-17 10:20:46         644 f55ccd93 490003
#> 37              6880 2023-04-17 10:20:40         644 4f65da62 494393
#> 38             11643 2023-04-17 10:20:40         644 119a0ee7 496402
#> 39              2349 2023-04-17 08:32:08         644 92f80ead 499287
#> 40             25288 2023-04-17 10:20:46         644 b69779f4 500150
#> 41            247235 2023-04-17 10:20:46         755 2090ba35 510746
```

### Uncompressing ZIP files

`unzip()` uncompresses a ZIP archive:

``` r
exdir <- tempfile()
unzip("sources.zip", exdir = exdir)
dir(exdir)
#> [1] "R"   "src"
```

### Compressing and uncompressing in background processes

You can use the `zip_process()` and `unzip_process()` functions to
create background zip / unzip processes. These processes were
implemented on top of the `processx::process` class, so they are
pollable.

## License

CC0
