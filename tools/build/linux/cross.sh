#! /bin/bash

mkdir -p /aarch64/etc/apk/keys
cp /etc/apk/repositories /aarch64/etc/apk/
cp -r /etc/apk/keys/* /aarch64/etc/apk/keys/
cp -r /usr/share/apk/keys/* /aarch64/etc/apk/keys/
apk add --root /aarch64 --update --initdb --arch aarch64 \
    libc-dev libgcc

