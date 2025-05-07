#! /bin/sh

cd aports/gaborcsardi
for p in `ls`; do
    ( cd $p; abuild checksum; abuild -r )
done
