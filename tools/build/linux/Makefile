
# ------------------------------------------------------------------------

RVERSIONS ?= 3.5 3.6 4.0 4.1 4.2 4.3 4.4

# ------------------------------------------------------------------------

build: $(DONE)

all: build

# ------------------------------------------------------------------------

cross-aarch64:
	docker compose build cross-aarch64

libs: 
	docker compose build libs

libs-aarch64: cross-aarch64
	docker compose build libs-aarch64

push:
	docker compose push cross-aarch64 libs libs-aarch64

rcmdbuild:
	R CMD build ../../..

# ------------------------------------------------------------------------

clean:
	rm -rf *.tar.gz

.PHONY: all build clean cross-aarch64 libs libs-aarch64 push rcmdbuild
