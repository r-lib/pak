ARG ARCH=invalid
FROM ghcr.io/r-lib/pak-glibc-${ARCH}-libs:latest

ARG R_VERSION=4.5.1

ENV TZ=UTC
ENV PKG_CONFIG_PATH=/opt/r-lib/lib/pkgconfig

# Install R --------------------------------------------------------------------

RUN curl -LO https://github.com/r-hub/R/releases/download/v${R_VERSION}/r-${R_VERSION}-glibc_1_$(dpkg --print-architecture).deb && \
    dpkg -i r-*.deb && \
    rm r-*.deb && \
    ln -s /opt/R/${R_VERSION}-glibc/bin/R /usr/local/bin/R

# Install pak ------------------------------------------------------------------

COPY pak_*.tar.gz /root/
RUN PROCESSX_UNLINK_R=true R CMD INSTALL pak_*.tar.gz

# Minimize library --------------------------------------------------------

ENV PAKROOT=/opt/R/${R_VERSION}-glibc/lib/R/library/pak

RUN find ${PAKROOT}/library -name "*.so" | \
    xargs patchelf --remove-needed libR.so

RUN rm -rf ${PAKROOT}/library/_cache && \
    rm -rf ${PAKROOT}/library/*/help && \
    rm -rf ${PAKROOT}/library/*/doc &&  \
    find ${PAKROOT}/library -name "*.so" | xargs strip -x

# Embed CA certs ----------------------------------------------------------

# We use the pak build that we just installed.

RUN R -q -e 'pak:::embed_ca_certs(lib = .libPaths()[1])'

# Build binary package ----------------------------------------------------

# We use the pak build that we just installed.

RUN R -q -e 'pak:::build_pak_binary_linux(lib = .libPaths()[1])'

# Deploy ------------------------------------------------------------------

# This also serves as a minimal test suite that we can download and
# install real packages, with the new build.

RUN R -q -e 'pak::pkg_install(c("R6@2.5.1", "glue@1.6.2", "gitcreds", \
    "processx", "digest", "desc", "jsonlite"))'

RUN git config --global user.email "csardi.gabor@gmail.com"
RUN git config --global user.name "Gabor Csardi"
RUN git config --global credential.helper cache

ARG TOKEN=dummy

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
