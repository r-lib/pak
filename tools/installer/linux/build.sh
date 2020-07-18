#! /bin/sh

set -ex

docker build -t rhub/pak-builder:3.6 -f tools/installer/linux/Dockerfile \
       tools/installer/linux

docker run -ti --rm -v `pwd`:/root/pak rhub/pak-builder:3.6

#

docker build -t rhub/pak-builder:3.5 --build-arg 'R_MAJOR=3.5' \
       -f tools/installer/linux/Dockerfile \
       tools/installer/linux

docker run -ti --rm -v `pwd`:/root/pak rhub/pak-builder:3.5

#

docker build -t rhub/pak-builder:3.4 --build-arg 'R_MAJOR=3.4' \
       -f tools/installer/linux/Dockerfile \
       tools/installer/linux

docker run -ti --rm -v `pwd`:/root/pak rhub/pak-builder:3.4

#

docker build -t rhub/pak-builder:3.3 --build-arg 'R_MAJOR=3.3' \
       -f tools/installer/linux/Dockerfile \
       tools/installer/linux

docker run -ti --rm -v `pwd`:/root/pak rhub/pak-builder:3.3
