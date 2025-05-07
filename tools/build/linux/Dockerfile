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
    coreutils findutils libsecret-static glib-static libgcrypt-static \
    libgpg-error-static util-linux-static libffi-dev pcre2-dev \
    libsecret-dev

# Build binary package ----------------------------------------------------

# needed for C23, which is the default on R-devel
RUN sed -i 's/basename[(][)]/basename(char*)/' /usr/include/string.h

# for static libsecret in keyring
RUN echo "LIBSECRET_CFLAGS=\"`pkg-config --cflags --static libsecret-1`\"" \
    >> /root/.Renviron && \
    echo "LIBSECRET_LIBS=\"`pkg-config --libs --static libsecret-1`\"" \
    >> /root/.Renviron

COPY pak_*.tar.gz /root/
RUN PROCESSX_UNLINK_R=true R CMD INSTALL pak_*.tar.gz

# Minimize library --------------------------------------------------------

ENV PAKROOT=/usr/local/lib/R/library/pak

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

