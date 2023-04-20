
# ------------------------------------------------------------------------

RVERSIONS ?= $(shell rig list)
RVERSION_DEFAULT ?= $(shell R -s  -e 'cat(as.character(getRversion()[,1:2]))')

PKGCONFIGVERSION=0.29.2
PKGCONFIGURLS=https://pkgconfig.freedesktop.org/releases/pkg-config-$(PKGCONFIGVERSION).tar.gz \
	http://fresh-center.net/linux/misc/pkg-config-$(PKGCONFIGVERSION).tar.gz \
	http://fresh-center.net/linux/misc/legacy/pkg-config-$(PKGCONFIGVERSION).tar.gz
PKGCONFIGSHA=6fc69c01688c9458a57eb9a1664c9aba372ccda420a02bf4429fe610e7e7d591
PKGCONFIGFILE=$(shell basename $(word 1, $(PKGCONFIGURLS)))

CARESVERSION=1.18.1
CARESSHA=1a7d52a8a84a9fbffb1be9133c0f6e17217d91ea5a6fa61f6b4729cda78ebbcf
CARESURLS=https://c-ares.org/download/c-ares-$(CARESVERSION).tar.gz \
	https://github.com/c-ares/c-ares/releases/download/cares-1_17_2/c-ares-$(CARESVERSION).tar.gz \
	http://fresh-center.net/linux/misc/dns/c-ares-$(CARESVERSION).tar.gz \
	http://fresh-center.net/linux/misc/dns/legacy/c-ares-$(CARESVERSION).tar.gz
CARESFILE=$(shell basename $(word 1, $(CARESURLS)))

NGHTTP2VERSION=1.46.0
NGHTTP2SHA=4b6d11c85f2638531d1327fe1ed28c1e386144e8841176c04153ed32a4878208
NGHTTP2URLS=https://github.com/nghttp2/nghttp2/releases/download/v$(NGHTTP2VERSION)/nghttp2-$(NGHTTP2VERSION).tar.gz \
	http://fresh-center.net/linux/www/nghttp2-$(NGHTTP2VERSION).tar.gz \
	http://fresh-center.net/linux/www/legacy/nghttp2-$(NGHTTP2VERSION).tar.gz
NGHTTP2FILE=$(shell basename $(word 1, $(NGHTTP2URLS)))

LIBCURLVERSION=7.80.0
LIBCURLSHA=dd0d150e49cd950aff35e16b628edf04927f0289df42883750cf952bb858189c
LIBCURLURLS=https://curl.se/download/curl-$(LIBCURLVERSION).tar.bz2 \
	https://github.com/curl/curl/releases/download/curl-7_80_0/curl-$(LIBCURLVERSION).tar.bz2 \
	http://fresh-center.net/linux/www/curl-$(LIBCURLVERSION).tar.bz2 \
	http://fresh-center.net/linux/www/legacy/curl-$(LIBCURLVERSION).tar.bz2
LIBCURLFILE=$(shell basename $(word 1, $(LIBCURLURLS)))

# ------------------------------------------------------------------------

ROOT=$(shell pwd)
PKGROOT=$(ROOT)/../../..

# We leave out homebrew on purpose
BASEPATH=/usr/bin:/bin:/usr/sbin:/sbin:/Library/Apple/usr/bin
ARMPATH=$(ROOT)/opt/arm64/bin:$(BASEPATH)
X86PATH=$(ROOT)/opt/x86_64/bin:$(BASEPATH)
PAKVERSION=$(shell grep "^Version:" ../../../DESCRIPTION | tr -cd '0-9.')

RVERSIONSARM=$(filter %-arm64,$(RVERSIONS))
RVERSIONSX86=$(filter-out %-arm64,$(RVERSIONS))

DONE=$(patsubst %,lib/R-%.done,$(RVERSIONS))

build: $(DONE)

all: build deploy

LIBNAMES=pkgconfig cares nghttp2 libcurl
LIBSARM=$(patsubst %,opt/arm64/%.done,$(LIBNAMES))
LIBSX86=$(patsubst %,opt/x86_64/%.done,$(LIBNAMES))

LIBS=$(LIBSX86)
ifneq ($(strip $(RVERSIONSARM)),)
  LIBS+=$(LIBSARM)
endif

# ------------------------------------------------------------------------
# pkg-config seems to be compulsory for compiling curl. macOS or even Xcode
# does not have it, so we compile our own.

$(PKGCONFIGFILE):
	@ ./download.sh $(PKGCONFIGFILE) $(PKGCONFIGSHA) $(PKGCONFIGURLS)

opt/arm64/pkgconfig.done: $(PKGCONFIGFILE)
	rm -rf src/arm64/pkg-config*
	mkdir -p src/arm64
	tar xzf $(PKGCONFIGFILE) -C src/arm64
	cd src/arm64/pkg-config-$(PKGCONFIGVERSION) && \
	./configure CFLAGS=-mmacosx-version-min=11.0 \
		--prefix=$(ROOT)/opt/arm64 \
		--disable-debug \
		--disable-host-tool \
		--with-pc-path=$(ROOT)/lib/pkgconfig:$(ROOT)/share/pkgconfig \
		--with-internal-glib && \
	make && \
	make install
	touch $@

opt/x86_64/pkgconfig.done: $(PKGCONFIGFILE)
	rm -rf src/x86_64/pkg-config*
	mkdir -p src/x86_64
	tar xzf $(PKGCONFIGFILE) -C src/x86_64
	cd src/x86_64/pkg-config-$(PKGCONFIGVERSION) && \
	./configure CFLAGS=-mmacosx-version-min=10.13 \
		--prefix=$(ROOT)/opt/x86_64 \
		--disable-debug \
		--disable-host-tool \
		--with-pc-path=$(ROOT)/lib/pkgconfig:$(ROOT)/share/pkgconfig \
		--with-internal-glib && \
	make && \
	make install
	touch $@

# ------------------------------------------------------------------------
# $(eval ...) is a trick to change the path for this rule
# Unfortunately this implementation also rules out running these builds
# in parallel, because PATH is global, and it would create race conditions.

$(CARESFILE):
	@ ./download.sh $(CARESFILE) $(CARESSHA) $(CARESURLS)

opt/arm64/cares.done: $(CARESFILE) opt/arm64/pkgconfig.done \
		      $(eval PATH := $(ARMPATH))
	rm -rf src/arm64/c-ares*
	mkdir -p src/arm64
	tar xzf $(CARESFILE) -C src/arm64
	cd src/arm64/c-ares-$(CARESVERSION) && \
	./configure CFLAGS=-mmacosx-version-min=11.0 \
		--prefix=$(ROOT)/opt/arm64 \
		--enable-static && \
	make && \
	make install
	touch $@

opt/x86_64/cares.done: $(CARESFILE) opt/x86_64/pkgconfig.done \
		       $(eval PATH := $(X86PATH))
	rm -rf src/x86_64/c-ares*
	mkdir -p src/x86_64
	tar xzf $(CARESFILE) -C src/x86_64
	cd src/x86_64/c-ares-$(CARESVERSION) && \
	arch --x86_64 ./configure CFLAGS=-mmacosx-version-min=10.13 \
		--prefix=$(ROOT)/opt/x86_64 \
		--enable-static && \
	arch --x86_64 make && \
	arch --x86_64 make install
	touch $@

# ------------------------------------------------------------------------

$(NGHTTP2FILE):
	@ ./download.sh $(NGHTTP2FILE) $(NGHTTP2SHA) $(NGHTTP2URLS)

opt/arm64/nghttp2.done: $(NGHTTP2FILE) opt/arm64/pkgconfig.done \
			$(eval PATH := $(ARMPATH))
	rm -rf src/arm64/nghttp2*
	mkdir -p src/arm64
	tar xzf $(NGHTTP2FILE) -C src/arm64
	cd src/arm64/nghttp2-$(NGHTTP2VERSION) && \
	./configure CFLAGS=-mmacosx-version-min=11.0 \
		--prefix=$(ROOT)/opt/arm64 \
		--enable-lib-only && \
	make -C lib && \
	make -C lib install
	touch $@

opt/x86_64/nghttp2.done: $(NGHTTP2FILE) opt/x86_64/pkgconfig.done
			 $(eval PATH := $(X86PATH))
	rm -rf src/x86_64/nghttp2*
	mkdir -p src/x86_64
	tar xzf $(NGHTTP2FILE) -C src/x86_64
	cd src/x86_64/nghttp2-$(NGHTTP2VERSION) && \
	arch --x86_64 ./configure CFLAGS=-mmacosx-version-min=10.13 \
		--prefix=$(ROOT)/opt/x86_64 \
		--enable-lib-only && \
	arch --x86_64 make -C lib && \
	arch --x86_64 make -C lib install
	touch $@

# ------------------------------------------------------------------------

$(LIBCURLFILE):
	@ ./download.sh $(LIBCURLFILE) $(LIBCURLSHA) $(LIBCURLURLS)

opt/arm64/libcurl.done: $(LIBCURLFILE) opt/arm64/pkgconfig.done \
			opt/arm64/cares.done opt/arm64/nghttp2.done \
			$(eval PATH := $(ARMPATH))
	rm -rf src/arm64/curl*
	mkdir -p src/arm64
	tar xzf $(LIBCURLFILE) -C src/arm64
	cd src/arm64/curl-$(LIBCURLVERSION) && \
	./configure \
		CFLAGS=-mmacosx-version-min=11.0 \
		--prefix=$(ROOT)/opt/arm64 \
		--enable-static \
		--disable-shared \
		--disable-debug \
		--disable-dependency-tracking \
		--disable-silent-rules \
		--without-ssl \
		--with-ca-fallback \
		--with-secure-transport \
		--without-libpsl \
		--disable-ldap \
		--disable-ldaps \
		--with-nghttp2=$(ROOT)/opt/arm64 \
		--enable-ares=$(ROOT)/opt/arm64 \
		--without-zstd \
		--without-librtmp \
		--without-libidn2 \
		--without-brotli \
		--without-libssh2 && \
	make && \
	make install
	touch $@

opt/x86_64/libcurl.done: $(LIBCURLFILE) opt/x86_64/pkgconfig.done \
			 opt/x86_64/cares.done opt/x86_64/nghttp2.done \
			 $(eval PATH := $(X86PATH))
	rm -rf src/x86_64/curl*
	mkdir -p src/x86_64
	tar xzf $(LIBCURLFILE) -C src/x86_64
	cd src/x86_64/curl-$(LIBCURLVERSION) && \
	arch --x86_64 ./configure \
		CFLAGS=-mmacosx-version-min=10.13 \
		--prefix=$(ROOT)/opt/x86_64 \
		--enable-static \
		--disable-shared \
		--disable-debug \
		--disable-dependency-tracking \
		--disable-silent-rules \
		--without-ssl \
		--with-ca-fallback \
		--with-secure-transport \
		--without-libpsl \
		--disable-ldap \
		--disable-ldaps \
		--with-nghttp2=$(ROOT)/opt/x86_64 \
		--enable-ares=$(ROOT)/opt/x86_64 \
		--without-zstd \
		--without-librtmp \
		--without-libidn2 \
		--without-brotli \
		--without-libssh2 && \
	arch --x86_64 make && \
	arch --x86_64 make install
	touch $@

# ------------------------------------------------------------------------

export PAK_BUNDLE = false
export PAK_ROOT = $(ROOT)

R-%: lib/R-%.done
	@true

lib/R-%.done: $(ROOT)/lib/%/_pak0.done $(ROOT)/lib/%/_curl.done \
	      $(ROOT)/lib/%/pkgdepends $(ROOT)/lib/%/_pak.done
	@echo "----- CLEANING INSTALLATION ($*) ---------------------------"
	rm -rf $@
	rm -rf $(ROOT)/lib/$*/pak/library/_cache
	rm -rf $(ROOT)/lib/$*/pak/library/*/help
	rm -rf $(ROOT)/lib/$*/pak/library/*/doc
	find $(ROOT)/lib/$*/pak/library -name "*.so.dSYM" | xargs rm -rf
	find $(ROOT)/lib/$*/pak/library -name "*.so" | xargs strip -x
	cd $(ROOT)/lib/$* && tar czf pak_$(PAKVERSION).tgz pak
	touch $@

$(ROOT)/lib/%/_pak0.done:
	@echo "----- INSTALLING pak ($*) ----------------------------------"
	rm -rf $@
	mkdir -p $(ROOT)/lib/$*
	/usr/local/bin/R-$* CMD INSTALL -l $(ROOT)/lib/$* $(PKGROOT)
	touch $@

$(ROOT)/lib/%/_curl.done: curl.tar.gz $(LIBS) $(eval PATH := $(X86PATH))
	@echo "----- INSTALLING curl ($*) ---------------------------------"
	rm -rf $@
	rm -rf $(ROOT)/lib/$*/_curl
	mkdir -p $(ROOT)/lib/$*/_curl
	tar xzf curl.tar.gz -C $(ROOT)/lib/$*/_curl
	cp Makevars-curl.in $(ROOT)/lib/$*/_curl/curl/src/Makevars.in
	mkdir -p $(ROOT)/lib/$*/pak/library
	/usr/local/bin/R-$* CMD INSTALL -l $(ROOT)/lib/$*/pak/library $(ROOT)/lib/$*/_curl/curl
	cp -r $(ROOT)/lib/$*/pak/library/curl $(ROOT)/lib/$*/
	touch $@

curl.tar.gz:
	@echo "----- DOWNLOADING curl -------------------------------------"
	/usr/local/bin/R -q -e 'options(repos = c(CRAN = "https://cran.rstudio.com")); download.packages("curl", ".")' && \
	mv curl_*.tar.gz curl.tar.gz

$(ROOT)/lib/%/pkgdepends: $(ROOT)/lib/%/_pak0.done $(ROOT)/lib/%/_curl.done
	@echo "----- INSTALLING pkgdepends ($*) ---------------------------"
	rm -rf $@
	mkdir -p $(ROOT)/lib/$*/pak/library
	R_LIBS="$(ROOT)/lib/$*" /usr/local/bin/R-$* -q -e \
		'pak:::safe_cran_install("pkgdepends", lib = "$(ROOT)/lib/$*", exclude = "curl")'
	touch $@

$(ROOT)/lib/%/_pak.done: $(ROOT)/lib/%/_curl.done $(ROOT)/lib/%/pkgdepends
	@echo "----- INSTALLING pak dependences ($*) ----------------------"
	rm -rf $@
	R_LIBS="$(ROOT)/lib/$*" /usr/local/bin/R-$* -q -e \
		'pak:::embed_lib("$(PKGROOT)")'
	touch $@

# ------------------------------------------------------------------------

TESTS=$(patsubst %,$(ROOT)/lib/R-%.tested,$(RVERSIONS))

test: $(TESTS)

$(ROOT)/lib/R-%.tested: $(ROOT)/lib/%/_pak.done
	@echo "----- TESTING ($*) -----------------------------------------"
	rm -rf $@
	echo TODO
	touch $@

# ------------------------------------------------------------------------

R=/usr/local/bin/R-$(RVERSION_DEFAULT)
RLIB=$(ROOT)/lib/$(RVERSION_DEFAULT)
export PKGFILES := $(patsubst %,lib/%/pak_$(PAKVERSION).tgz,$(RVERSIONS))

deploy: test
	@echo "----- DEPLOYING packages -----------------------------------"
	@echo $(PKGFILES)
	R_LIBS=$(RLIB) $(R) -q -e 'pak::pkg_install("deps::$(PKGROOT)", dependencies = TRUE)'
	R_LIBS=$(RLIB) $(R) -q -e \
		'pak:::push_packages(strsplit(Sys.getenv("PKGFILES"), " ")[[1]])'

# ------------------------------------------------------------------------

clean:
	rm -rf src opt lib *.tar.gz *.tar.bz2

.PRECIOUS: $(ROOT)/lib/%/_pak0.done $(ROOT)/lib/%/_curl.done \
	   $(ROOT)/lib/%/pkgdepends $(ROOT)/lib/%/_pak.done \
	   $(ROOT)/lib/R-%.tested

.PHONY: all build test deploy clean R-%
