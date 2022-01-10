#!/bin/sh

echo $@

file=$1
sha=$2

shift
shift

ok() {
    if [ ! -f "$file" ]; then return 1; fi
    sha2=$(shasum -ba 256 "$file" | cut -f1 -d" ")
    if [ "$sha" == "$sha2" ]; then
        return 0;
    else
        return 1;
    fi
}

if ok; then exit 0; fi

while [ "$1" != "" ];
do
    curl -L -o "$file" "$1" || true
    if ok; then exit 0; fi
    shift
done

exit 1
