#!/bin/sh

if [ $# != 4 ]
then
    echo "usage: $0 srclib srcconf destlib destconf"
    exit
fi

srclib=$1
srcconf=$2
destlib=$3
destconf=$4

cp -n -r -t "$destlib" "$srclib"/*
TEMPDIR=$(mktemp -d)
cp -t "$TEMPDIR" "$srcconf"/*.conf

for i in "$TEMPDIR"/*.conf
do
    pkgname=$(basename $i)
    sed -i -e 's#^import-dirs: .*$#import-dirs: '"$destlib/$pkgname#g" \
           -e 's#^library-dirs: .*$#library-dirs: '"$destlib/$pkgname#g" $i
done

cp -n -t "$destconf" "$TEMPDIR"/*.conf
ghc-pkg recache
