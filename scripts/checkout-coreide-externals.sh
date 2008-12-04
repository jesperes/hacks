#!/bin/bash

svn propget svn:externals http://svn.iar.se/ide/platform/trunk | while read f; do
    d=`echo "$f" | awk '{ print $1; }'`
    if [ "$d" ]; then
	rev=`echo "$f" | awk '{ print $2; }'`
	url=`echo "$f" | awk '{ print $3; }'`
	echo "Checking out $url ($rev) -> $d"
	svn checkout -q $rev $url $d
    fi
done
