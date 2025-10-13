# Build pak binary for Linux, glibc, x86_64 and aarch64

# To build some R versions:
# ```
# RVERSIONS="4.3 4.4" make
# ```
#
# This will use the native architecture.

RVERSIONS ?= 3.5.3 3.6.3 4.0.5 4.1.3 4.2.3 4.3.3 4.4.3 4.5.1 devel
PAKVERSION=$(shell grep "^Version:" ../../../DESCRIPTION | tr -cd '0-9')
DONE=$(patsubst %,R-%.done,$(RVERSIONS))
ARCH=$(shell ./detect-docker-arch.sh)

all: build

build: $(DONE)

R-%.done: libs.done pak_$(PAKVERSION).tar.gz
	rm -f $@
	docker build -f Dockerfile \
		--platform linux/$(ARCH) \
		--build-arg ARCH=$(ARCH) \
		--build-arg R_VERSION=$* \
		--build-arg TOKEN="${GHCR_TOKEN}" .
	touch $@

# ------------------------------------------------------------------------

libs:
	docker build --platform linux/$(ARCH) \
		-t ghcr.io/r-lib/pak-glibc-$(ARCH)-libs:latest \
		-f Dockerfile-libs .

libs.done:
	docker pull ghcr.io/r-lib/pak-glibc-$(ARCH)-libs:latest
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
	rm -rf *.tar.gz

.PHONY: all build clean libs
