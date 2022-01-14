
ARG R_MAJOR=4.1

FROM ghcr.io/r-lib/pak-builder:${R_MAJOR}

# Need the copy of the package --------------------------------------------

COPY ../../.. /root/pak

WORKDIR /root/pak

# Install pak without the dependencies ------------------------------------

RUN PAK_BUNDLE=false R CMD INSTALL .

# system requirements -----------------------------------------------------

RUN apk add linux-headers bash gcc musl-dev g++ pkgconf

# Install pkgdepends ------------------------------------------------------

RUN R -q -e 'pak:::safe_cran_install("pkgdepends")'

# Install pak dependencies ------------------------------------------------

RUN R -q -e 'pak:::embed_lib(".")'

# Embed CA certs ----------------------------------------------------------

RUN R -q -e 'pak:::embed_ca_certs()'

# Minimize library --------------------------------------------------------

ENV PAKROOT=/usr/local/lib/R/library/pak

RUN rm -rf ${PAKROOT}/library/_cache && \
    rm -rf ${PAKROOT}/library/*/help && \
    rm -rf ${PAKROOT}/library/*/doc &&  \
    find ${PAKROOT}/library -name "*.so" | xargs strip -x

# Build binary package ----------------------------------------------------

RUN R -q -e 'pak:::build_pak_binary_linux()'

# Install skopeo ----------------------------------------------------------

RUN apk add skopeo

# Test --------------------------------------------------------------------

# TODO

# Deploy

RUN apk add git
RUN R -q -e 'pak::pkg_install("deps::.")'
RUN R -q -e 'pak:::push_packages(dir("/tmp/", patter = "pak_.*.tar.gz", full.names=TRUE))'
