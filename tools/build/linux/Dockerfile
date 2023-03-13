
ARG ARCH
ARG R_MAJOR=4.1

FROM ghcr.io/r-lib/pak-builder:${ARCH}-${R_MAJOR}
ARG TOKEN=dummy

# Need the copy of the package --------------------------------------------

ARG PAK_ROOT=../../..
COPY ${PAK_ROOT} /root/pak

WORKDIR /root/pak

# Install pak without the dependencies ------------------------------------

RUN PAK_BUNDLE=false R CMD INSTALL .

# system requirements -----------------------------------------------------

RUN apk add linux-headers bash gcc musl-dev g++ pkgconf patchelf

# Install pkgdepends ------------------------------------------------------

RUN PROCESSX_UNLINK_R=true R -q -e 'pak:::safe_cran_install("pkgdepends")'

# Install pak dependencies ------------------------------------------------

RUN PROCESSX_UNLINK_R=true R -q -e 'pak:::embed_lib(".")'

# Embed CA certs ----------------------------------------------------------

RUN R -q -e 'pak:::embed_ca_certs()'

# Minimize library --------------------------------------------------------

ENV PAKROOT=/usr/local/lib/R/library/pak

RUN find ${PAKROOT}/library -name "*.so" | \
    xargs patchelf --remove-needed libR.so

RUN rm -rf ${PAKROOT}/library/_cache && \
    rm -rf ${PAKROOT}/library/*/help && \
    rm -rf ${PAKROOT}/library/*/doc &&  \
    find ${PAKROOT}/library -name "*.so" | xargs strip -x

# Build binary package ----------------------------------------------------

RUN R -q -e 'pak:::build_pak_binary_linux()'

# Install skopeo ----------------------------------------------------------

# Temporarily, to get skopeo 1.6.0, once this is in a stable Alpine,
# we should get it from there, and eventually from the current version

RUN apk add skopeo --repository=http://dl-cdn.alpinelinux.org/alpine/edge/community

# Test --------------------------------------------------------------------

# TODO

# Deploy

RUN R -q -e 'pak::pkg_install(c("glue", "gitcreds", "processx", "digest", "desc"))'

RUN apk add git
RUN git config --global user.email "csardi.gabor@gmail.com"
RUN git config --global user.name "Gabor Csardi"
RUN git config credential.helper cache

ENV GITHUB_PAT=${TOKEN}

# This has to be in one step, because the cache credential helper is
# very ephemeral.
RUN R -q -e 'gitcreds::gitcreds_approve(list(url = "https://github.com", username = "PersonalAccessToken", password = Sys.getenv("GITHUB_PAT"))); pak:::push_packages(dir("/tmp/", patter = "pak_.*.tar.gz", full.names=TRUE))'
