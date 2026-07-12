# Build the Linux binary pak packages.
#
# We build two flavors:
# - `musl`  on Alpine,       for musl-based distros,
# - `glibc` on Ubuntu 18.04, for glibc-based distros (glibc >= 2.27).
#
# Both are built natively, so run `make` on an x86_64 machine to build
# x86_64 binaries and on an aarch64 machine to build aarch64 binaries.
# There is no cross-compilation.
#
# To build some musl binaries:
# ```
# RVERSIONS_MUSL="4.4 4.5" make musl
# ```
#
# To build some glibc binaries:
# ```
# RVERSIONS_GLIBC="4.4.3 4.5.3" make glibc
# ```

# ------------------------------------------------------------------------

RVERSIONS_MUSL ?= 3.5 3.6 4.0 4.1 4.2 4.3 4.4 4.5 4.6 4.7
RVERSIONS_GLIBC ?= 3.5.3 3.6.3 4.0.5 4.1.3 4.2.3 4.3.3 4.4.3 4.5.3 4.6.1 devel
PAKVERSION=$(shell grep "^Version:" ../../../DESCRIPTION | tr -cd '0-9.')
DONE_MUSL=$(patsubst %,R-%-musl.done,$(RVERSIONS_MUSL))
DONE_GLIBC=$(patsubst %,R-%-glibc.done,$(RVERSIONS_GLIBC))

all: musl glibc

musl: $(DONE_MUSL)

glibc: $(DONE_GLIBC)

# The built binary package is exported to out/$*/pak_*.tar.gz, a separate
# deploy job pushes it to ghcr.io.

R-%-musl.done: libs-musl.done pak_$(PAKVERSION).tar.gz
	rm -f $@
	docker buildx build -f Dockerfile-musl \
		--build-arg R_MAJOR=$* \
		--target export --output type=local,dest=out .
	touch $@

R-%-glibc.done: libs-glibc.done pak_$(PAKVERSION).tar.gz
	rm -f $@
	docker buildx build -f Dockerfile-glibc \
		--build-arg R_VERSION=$* \
		--target export --output type=local,dest=out .
	touch $@

# ------------------------------------------------------------------------

# The static libraries (libcurl, OpenSSL, zlib, ...) are collected once into
# a local image per flavor and reused across all R versions. Once these
# images are published, the `.done` rules can `docker pull` them instead of
# rebuilding.

libs-musl:
	docker build -t ghcr.io/r-lib/pak-libs-musl:latest \
		-f Dockerfile-libs-musl .

libs-musl.done:
	rm -f $@
	docker build -t ghcr.io/r-lib/pak-libs-musl:latest \
		-f Dockerfile-libs-musl .
	touch $@

libs-glibc:
	docker build -t ghcr.io/r-lib/pak-libs-glibc:latest \
		-f Dockerfile-libs-glibc .

libs-glibc.done:
	rm -f $@
	docker build -t ghcr.io/r-lib/pak-libs-glibc:latest \
		-f Dockerfile-libs-glibc .
	touch $@

pak_$(PAKVERSION).tar.gz:
	@if command -v RR >/dev/null; then \
	  echo "Building R package using `command -v R`."; \
	  R CMD build ../../..; \
	else \
	  CTR="ghcr.io/r-hub/r-minimal/r-minimal:latest"; \
	  echo "Building R package using Docker ($${CTR})."; \
	  docker run -v "`pwd -P`/../../..":"`pwd`/../../.." -w "`pwd`" \
	    "$${CTR}" R CMD build ../../..; \
	fi

# ------------------------------------------------------------------------

clean:
	rm -rf *.tar.gz *.done out

.PHONY: all musl glibc libs-musl libs-glibc clean
