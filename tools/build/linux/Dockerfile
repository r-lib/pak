ARG R_MAJOR=4.1

FROM ghcr.io/r-lib/pak-libs:latest AS libs
FROM ghcr.io/r-hub/r-minimal/r-minimal:${R_MAJOR}
COPY --from=libs /usr/local /usr/local
USER root
WORKDIR /root

ARG TOKEN=dummy

# -------------------------------------------------------------------------
# set up static compilation
# -------------------------------------------------------------------------

RUN mkdir -p /root/.R
COPY Makevars-x86_64 /root/.R/Makevars

# system requirements -----------------------------------------------------

RUN apk add linux-headers bash gcc musl-dev g++ pkgconf patchelf \
    coreutils findutils

# Build binary package ----------------------------------------------------

COPY pak_*.tar.gz /root/
RUN R CMD INSTALL pak_*.tar.gz

# Minimize library --------------------------------------------------------

ENV PAKROOT=/usr/local/lib/R/library/pak

RUN find ${PAKROOT}/library -name "*.so" | \
    xargs patchelf --remove-needed libR.so

RUN rm -rf ${PAKROOT}/library/_cache && \
    rm -rf ${PAKROOT}/library/*/help && \
    rm -rf ${PAKROOT}/library/*/doc &&  \
    find ${PAKROOT}/library -name "*.so" | xargs strip -x

# Embed CA certs ----------------------------------------------------------

RUN R -q -e 'pak:::embed_ca_certs(lib = .libPaths()[1])'

# Build binary package ----------------------------------------------------

RUN R -q -e 'pak:::build_pak_binary_linux(lib = .libPaths()[1])'

# Install skopeo ----------------------------------------------------------

RUN apk add skopeo

# Test --------------------------------------------------------------------

# TODO

# Deploy ------------------------------------------------------------------

RUN R -q -e 'pak::pkg_install(c("glue", "gitcreds", "processx", "digest", "desc", "jsonlite"))'

RUN apk add git
RUN git config --global user.email "csardi.gabor@gmail.com"
RUN git config --global user.name "Gabor Csardi"
RUN git config --global credential.helper cache

ENV GITHUB_PAT=${TOKEN}
RUN git clone --depth 1 https://github.com/r-lib/pak.git

# This has to be in one step, because the cache credential helper is
# very ephemeral.
RUN cd pak && R -q -e '                                          \
    gitcreds::gitcreds_approve(list(                             \
      url = "https://github.com",                                \
      username = "PersonalAccessToken",                          \
      password = Sys.getenv("GITHUB_PAT")                        \
    ));                                                          \
    pak:::push_packages(                                         \
      dir("/tmp/", pattern = "pak_.*.tar.gz", full.names=TRUE)   \
    )'
