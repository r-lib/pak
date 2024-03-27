# ------------------------------------------------------------------------

RVERSIONS ?= $(shell rig ls | tail +3 | cut -b 3- | cut -f 1 -d" ")
RVERSION_DEFAULT ?= $(shell rig default)

# ------------------------------------------------------------------------

ROOT=$(shell pwd)
PKGROOT=$(ROOT)/../../..

PAKVERSION=$(shell grep "^Version:" ../../../DESCRIPTION | tr -cd '0-9.')

DONE=$(patsubst %,lib/R-%.done,$(RVERSIONS))

build: $(DONE)

all: build deploy
export PAK_ROOT = $(ROOT)

R-%: lib/R-%.done
	@true

R=/usr/local/bin/R-$(RVERSION_DEFAULT)
sourcepkg: pak_$(PAKVERSION).tar.gz

pak_$(PAKVERSION).tar.gz:
	$(R) CMD build $(PKGROOT)

lib/R-%.done: pak_$(PAKVERSION).tar.gz
	rm -f $@
	mkdir -p lib/$*
	./build.sh "$*" "pak_$(PAKVERSION).tar.gz" "$(ROOT)/lib/$*"
	rm -rf $(ROOT)/lib/$*/pak/library/_cache
	rm -rf $(ROOT)/lib/$*/pak/library/*/help
	rm -rf $(ROOT)/lib/$*/pak/library/*/doc
	find $(ROOT)/lib/$*/pak/library -name "*.so.dSYM" | xargs rm -rf
	find $(ROOT)/lib/$*/pak/library -name "*.so" | xargs strip -x
	cd $(ROOT)/lib/$* && tar czf pak_$(PAKVERSION).tgz --no-xattrs pak
	touch $@

# ------------------------------------------------------------------------

RLIB=$(ROOT)/lib/$(RVERSION_DEFAULT)
export PKGFILES := $(patsubst %,lib/%/pak_$(PAKVERSION).tgz,$(RVERSIONS))

deploy:
	@echo "----- DEPLOYING packages -----------------------------------"
	# this is somewhat redundant, but needed when cross-compiling, to have
	# a native package installed.
	$(R) CMD INSTALL pak_$(PAKVERSION).tar.gz
	R_LIBS=$(RLIB) $(R) -q -e 'pak::pkg_install("deps::$(PKGROOT)", dependencies = "Config/Needs/deploy")'
	@echo Packages: "$$PKGFILES"
	R_LIBS=$(RLIB) $(R) -q -e \
		'pak:::push_packages(strsplit(Sys.getenv("PKGFILES"), " ")[[1]])'

# ------------------------------------------------------------------------

clean:
	rm -rf src opt lib *.tar.gz *.tar.bz2

.PHONY: all build deploy clean sourcepkg R-%

.PRECIOUS: lib/R-%.done
