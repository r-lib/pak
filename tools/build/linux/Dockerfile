# Build a static Linux x86_64 binary package

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

# needed for C23, which is the default on R-devel
RUN sed -i 's/basename[(][)]/basename(char*)/' /usr/include/string.h

COPY pak_*.tar.gz /root/

# Fix platform string, on R 3.5.x this is incorrect
RUN R -s -e 'cat(R.Version()$platform)' | sed 's/linux-gnu/linux-musl/g' \
    > /tmp/r-target-platform
RUN PROCESSX_UNLINK_R=true R CMD INSTALL \
     --configure-args="--host=`cat /tmp/r-target-platform`" \
     pak_*.tar.gz
# Also replace in the installed package, this is what wee actually use.
# We don't care about replacing it in the RDS file, that's not needed.
# Need to also do it in curl, because that's where wee are taking the
# platform, as pak has no compiled code.
ENV PAKROOT=/usr/local/lib/R/library/pak
RUN sed -i 's/linux-gnu/linux-musl/g' ${PAKROOT}/DESCRIPTION
RUN sed -i 's/linux-gnu/linux-musl/g' ${PAKROOT}/library/curl/DESCRIPTION

# Minimize library --------------------------------------------------------

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

# Install skopeo ----------------------------------------------------------

RUN apk add skopeo

# Test --------------------------------------------------------------------

# TODO: have a proper test suite here.

# Deploy ------------------------------------------------------------------

# This also serves as a minimal test suite that we can download and
# install real packages, with the new build.

RUN R -q -e 'pak::pkg_install(c("R6@2.5.1", "glue@1.6.2", "gitcreds", "processx", "digest", \
    "desc", "jsonlite"))'

RUN apk add git
RUN git config --global user.email "csardi.gabor@gmail.com"
RUN git config --global user.name "Gabor Csardi"
RUN git config --global credential.helper cache

# `push_packages() only works from within the pak git repo, because it
# checks out the `packaged` branch in another subtree.

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
