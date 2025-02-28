# Build static pak binary package on Linux, x86_64 and aarch64

# To build some R versions, on x86_64:
# ```
# RVERSIONS="4.3 4.4" make
# ```
#
# To do the same for aarch64:
# ```
# RVERSIONS=4.3 4.4" make aarch64
# ```

# ------------------------------------------------------------------------

RVERSIONS ?= 3.5 3.6 4.0 4.1 4.2 4.3 4.4
PAKVERSION=$(shell grep "^Version:" ../../../DESCRIPTION | tr -cd '0-9.')
DONE=$(patsubst %,R-%.done,$(RVERSIONS))
DONE_AARCH64=$(patsubst %,R-%-aarch64.done,$(RVERSIONS))

all: build

aarch64: $(DONE_AARCH64)

build: $(DONE)

R-%-aarch64.done: libs-aarch64.done pak_$(PAKVERSION).tar.gz
	rm -f $@
	docker build --platform linux/arm64 --build-arg R_MAJOR=$* \
		--build-arg TOKEN="${GHCR_TOKEN}" .
	touch $@

R-%.done: libs.done pak_$(PAKVERSION).tar.gz
	rm -f $@
	docker build --platform linux/amd64 --build-arg R_MAJOR=$* \
		--build-arg TOKEN="${GHCR_TOKEN}" .
	touch $@

# ------------------------------------------------------------------------

libs:
	docker build -t ghcr.io/r-lib/pak-libs-amd64:latest \
		--platform linux/amd64 -f Dockerfile-libs .

libs.done:
	rm -f $@
	docker pull ghcr.io/r-lib/pak-libs-amd64:latest
	touch $@

libs-aarch64:
	docker build -t ghcr.io/r-lib/pak-libs-arm64:latest \
		--platform linux/arm64 -f Dockerfile-libs .

libs-aarch64.done:
	rm -f $@
	docker pull ghcr.io/r-lib/pak-libs-arm64:latest
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

.PHONY: all build clean cross-aarch64 libs libs-aarch64 push rcmdbuild
