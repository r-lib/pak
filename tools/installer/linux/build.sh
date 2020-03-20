#! /bin/sh

set -ex

docker build -t rhub/pak-builder:3.6 -f tools/installer/linux/Dockerfile \
       tools/installer/linux

docker run -ti --rm -v `pwd`:/root/pak rhub/pak-builder:3.6
