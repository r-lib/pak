
#' Title: pak is fast
#' Width: 80
#' End_delay: 20

# << setup
library(pak)
dir.create(lib <- tempfile())
Sys.setenv(R_LIBS = lib)
.libPaths(lib)
options(width = 75)

# <<
# Install package and its dependencies.
# <<

pak::pkg_install("usethis")

# <<
# If package and dependencies are already installed, then nothing to do.
# <<

pak::pkg_install("usethis")

# <<
# Let's remove a dependency of usethis. Then pak only reinstalls the
# missing dependency.
# <<

pak::pkg_remove("mime")
pak::pkg_install("usethis")
