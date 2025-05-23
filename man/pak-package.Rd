% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pak-package.R
\docType{package}
\name{pak-package}
\alias{pak-package}
\title{pak: Another Approach to Package Installation}
\description{
The goal of 'pak' is to make package installation faster and more reliable. In particular, it performs all HTTP operations in parallel, so metadata resolution and package downloads are fast. Metadata and package files are cached on the local disk as well. 'pak' has a dependency solver, so it finds version conflicts before performing the installation. This version of 'pak' supports CRAN, 'Bioconductor' and 'GitHub' packages as well.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://pak.r-lib.org/}
  \item \url{https://github.com/r-lib/pak}
  \item Report bugs at \url{https://github.com/r-lib/pak/issues}
}

}
\author{
\strong{Maintainer}: Gábor Csárdi \email{csardi.gabor@gmail.com}

Authors:
\itemize{
  \item Jim Hester
}

Other contributors:
\itemize{
  \item Posit Software, PBC (\href{https://ror.org/03wc8by49}{ROR}) [copyright holder, funder]
  \item Winston Chang (R6, callr, processx) [contributor]
  \item Ascent Digital Services (callr, processx) [copyright holder, funder]
  \item Hadley Wickham (cli, curl, pkgbuild, yaml) [contributor, copyright holder]
  \item Jeroen Ooms (curl, jsonlite) [contributor]
  \item Maëlle Salmon (desc, pkgsearch) [contributor]
  \item Duncan Temple Lang (jsonlite) [contributor]
  \item Lloyd Hilaiel (jsonlite) [copyright holder]
  \item Alec Wong (keyring) [contributor]
  \item Michel Berkelaar and lpSolve authors (lpSolve) [contributor]
  \item R Consortium (pkgsearch) [funder]
  \item Jay Loden (ps) [contributor]
  \item Dave Daeschler (ps) [contributor]
  \item Giampaolo Rodola (ps) [contributor]
  \item Shawn Garbett (yaml) [contributor]
  \item Jeremy Stephens (yaml) [contributor]
  \item Kirill Simonov (yaml) [contributor]
  \item Yihui Xie (yaml) [contributor]
  \item Zhuoer Dong (yaml) [contributor]
  \item Jeffrey Horner (yaml) [contributor]
  \item Will Beasley (yaml) [contributor]
  \item Brendan O'Connor (yaml) [contributor]
  \item Gregory Warnes (yaml) [contributor]
  \item Michael Quinn (yaml) [contributor]
  \item Zhian Kamvar (yaml) [contributor]
  \item Charlie Gao (yaml) [contributor]
  \item Kuba Podgórski (zip) [contributor]
  \item Rich Geldreich (zip) [contributor]
}

}
\keyword{internal}
