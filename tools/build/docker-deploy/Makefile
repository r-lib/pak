# Deploy all .zip, .tgz, .tar.gz files. Run this in an Alpine Linux
# container that has R. E.g. an r-hub/r-minimal container.
SHELL := /bin/bash

all: docker-deploy

docker-deploy:
	git clean -fdx src
	docker run -v `pwd`:/repo --workdir /repo \
	  -e PAK_GHCR_TOKEN -e GITHUB_PAT \
	  ghcr.io/r-hub/r-minimal/r-minimal:latest \
	  bash -c 'apk add make; make -f tools/build/docker-deploy/Makefile deploy'

deploy:
	apk add skopeo gcc musl-dev g++ curl-dev linux-headers git
	git config --global --add safe.directory `pwd`
	git config --global user.email csardi.gabor@gmail.com
	git config --global user.name "Gabor Csardi"
	R CMD INSTALL .
	R -q --no-save -e \
	  'pak::pkg_install("deps::.", dependencies = "Config/Needs/deploy")'
	shopt -s nullglob && R -q --no-save -e \
	  'pak:::push_packages(normalizePath(commandArgs(TRUE)))' \
	  --args *.{tar.gz,tgz,zip}

.PHONY: all docker-deploy deploy
