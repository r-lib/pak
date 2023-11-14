ARG R_MAJOR=4.1

FROM ghcr.io/r-lib/pak-libs:latest AS libs
FROM ghcr.io/r-hub/r-minimal/r-minimal:${R_MAJOR}
COPY --from=libs /usr/local /usr/local

ARG TOKEN=dummy

# -------------------------------------------------------------------------
# set up static compilation
# -------------------------------------------------------------------------

RUN mkdir -p /root/.R
COPY Makevars-x86_64 /root/.R/Makevars

# Need the copy of the package --------------------------------------------

ARG PAK_ROOT=../../..
COPY ${PAK_ROOT} /root/pak

WORKDIR /root/pak

# system requirements -----------------------------------------------------

RUN apk add linux-headers bash gcc musl-dev g++ pkgconf patchelf \
    coreutils findutils

# Build binary package ----------------------------------------------------

RUN cd .. && R CMD build pak
RUN cd .. && R CMD INSTALL pak_*.tar.gz

# Minimize library --------------------------------------------------------

ENV PAKROOT=/usr/local/lib/R/library/pak

RUN find ${PAKROOT}/library -name "*.so" | \
    xargs patchelf --remove-needed libR.so

RUN rm -rf ${PAKROOT}/library/_cache && \
    rm -rf ${PAKROOT}/library/*/help && \
    rm -rf ${PAKROOT}/library/*/doc &&  \
    find ${PAKROOT}/library -name "*.so" | xargs strip -x

# Embed CA certs ----------------------------------------------------------

RUN R -q -e 'pak:::embed_ca_certs()'

# Build binary package ----------------------------------------------------

RUN R -q -e 'pak:::build_pak_binary_linux()'

# Install skopeo ----------------------------------------------------------

# Temporarily, to get skopeo 1.6.0, once this is in a stable Alpine,
# we should get it from there, and eventually from the current version

RUN apk add skopeo --repository=http://dl-cdn.alpinelinux.org/alpine/edge/community

# Test --------------------------------------------------------------------

# TODO

# Deploy

RUN R -q -e 'pak::pkg_install(c("glue", "gitcreds", "processx", "digest", "desc", "jsonlite"))'

RUN apk add git
RUN git config --global user.email "csardi.gabor@gmail.com"
RUN git config --global user.name "Gabor Csardi"
RUN git config credential.helper cache

ENV GITHUB_PAT=${TOKEN}

# This has to be in one step, because the cache credential helper is
# very ephemeral.
RUN R -q -e 'gitcreds::gitcreds_approve(list(url = "https://github.com", username = "PersonalAccessToken", password = Sys.getenv("GITHUB_PAT"))); pak:::push_packages(dir("/tmp/", patter = "pak_.*.tar.gz", full.names=TRUE))'
