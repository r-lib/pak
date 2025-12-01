# pak: Another Approach to Package Installation

The goal of 'pak' is to make package installation faster and more
reliable. In particular, it performs all HTTP operations in parallel, so
metadata resolution and package downloads are fast. Metadata and package
files are cached on the local disk as well. 'pak' has a dependency
solver, so it finds version conflicts before performing the
installation. This version of 'pak' supports CRAN, 'Bioconductor' and
'GitHub' packages as well.

## See also

Useful links:

- <https://pak.r-lib.org/>

- <https://github.com/r-lib/pak>

- Report bugs at <https://github.com/r-lib/pak/issues>

## Author

**Maintainer**: Gábor Csárdi <csardi.gabor@gmail.com>

Authors:

- Jim Hester

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]

- Winston Chang (R6, callr, processx) \[contributor\]

- Ascent Digital Services (callr, processx) \[copyright holder, funder\]

- Hadley Wickham (cli, curl, pkgbuild, yaml) \[contributor, copyright
  holder\]

- Jeroen Ooms (curl, jsonlite) \[contributor\]

- Maëlle Salmon (desc, pkgsearch) \[contributor\]

- Duncan Temple Lang (jsonlite) \[contributor\]

- Lloyd Hilaiel (jsonlite) \[copyright holder\]

- Alec Wong (keyring) \[contributor\]

- Michel Berkelaar and lpSolve authors (lpSolve) \[contributor\]

- R Consortium (pkgsearch) \[funder\]

- Jay Loden (ps) \[contributor\]

- Dave Daeschler (ps) \[contributor\]

- Giampaolo Rodola (ps) \[contributor\]

- Shawn Garbett (yaml) \[contributor\]

- Jeremy Stephens (yaml) \[contributor\]

- Kirill Simonov (yaml) \[contributor\]

- Yihui Xie (yaml) \[contributor\]

- Zhuoer Dong (yaml) \[contributor\]

- Jeffrey Horner (yaml) \[contributor\]

- Will Beasley (yaml) \[contributor\]

- Brendan O'Connor (yaml) \[contributor\]

- Gregory Warnes (yaml) \[contributor\]

- Michael Quinn (yaml) \[contributor\]

- Zhian Kamvar (yaml) \[contributor\]

- Charlie Gao (yaml) \[contributor\]

- Kuba Podgórski (zip) \[contributor\]

- Rich Geldreich (zip) \[contributor\]
