#! /bin/bash

set -Eeuo pipefail

lib=`mktemp -d`
echo "---> Temporary library at $lib"

pak_version=`grep "^Version:" DESCRIPTION | cut -f2 -d: | sed "s/[ \t]*//g"`
echo "---> Bundling pak version $pak_version"

function finish {
    echo "---> Cleaning up library at $lib"
    if [[ -n $lib ]]; then rm -rf "$lib"; fi
}
trap finish EXIT

echo "---> Installing pak"
R CMD INSTALL -l "$lib" .

echo "---> Installing pak dependencies"
export PAK_PRIVATE_LIBRARY="$lib/pak/library"
mkdir $PAK_PRIVATE_LIBRARY
R -q -e "pak::pak_setup()"

echo "---> Trimming private library"
rm -rf "$PAK_PRIVATE_LIBRARY"/*/help
rm -rf "$PAK_PRIVATE_LIBRARY"/*/doc
find $PAK_PRIVATE_LIBRARY -name "*.so" -exec strip -x \{\} \;
find $PAK_PRIVATE_LIBRARY -name "*.so.dSYM" | xargs rm -r

binary_package="pak_${pak_version}.tgz"
echo "---> Creating binary package at ../$binary_package"
cd "$lib"
tar czf "$binary_package" "pak"
cd -
cp "$lib/$binary_package" ..
