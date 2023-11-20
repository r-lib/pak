#! /bin/bash

RVER="$1"
PKGFILE="$2"
LIB="$3"

# Do we need to cross-compile?
if [[ -e "/usr/local/bin/R-${RVER}" ]]; then
    # nope
    R_MAKEVARS_USER="`pwd`/Makevars-macos-${RVER}" \
        "/usr/local/bin/R-${RVER}" CMD INSTALL -l "$LIB" "$PKGFILE"
    exit 0
elif [[ "$RVER" =~ -arm64$ ]]; then
    RVERX861="${RVER%-arm64}"
    RVERX862="${RVER%-arm64}-x86_64"
    if [[ -e "/usr/local/bin/R-${RVERX861}" ]]; then
      RVERX86="$RVERX861"
    elif [[ -e "/usr/local/bin/R-${RVERX862}" ]]; then
      RVERX86="$RVERX862"
    else
      printf "ERROR: don't know how to build pak with R-${RVER}\n" >&2
      exit 1
    fi
else
    printf "ERROR: don't know how to build pak with R-${RVER}\n" >&2
    exit 1
fi

# -------------------------------------------------------------------------
# The rest is for cros-compilation only

echo "Cross compiling for R ${RVER} using R ${RVERX86}."

# Need to install all dependencies for the build arch first.
# TODO: look up the needed packages automatically
# TODO: should make sure the right versions are installed

R_MAKEVARS_USER="`pwd`/Makevars-macos-${RVERX86}" \
    R_COMPILE_AND_INSTALL_PACKAGES=always \
    "/usr/local/bin/R-${RVERX86}" -q \
    -e 'install.packages(c("pkgdepends", "pkgsearch"))'

# TODO: we will need to hsve R-version specific platform triplets,
# when a version of R starts using a newer build than the current
# Big Sur (darwin 20).

R_MAKEVARS_USER="`pwd`/Makevars-macos-${RVER}" \
    "/usr/local/bin/R-${RVERX86}" CMD INSTALL \
    --configure-args='--host=aarch64-apple-darwin20' \
    -l "$LIB" "$PKGFILE"
