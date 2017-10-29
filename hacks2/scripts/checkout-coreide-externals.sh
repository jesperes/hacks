#!/bin/bash

function checkout_external()
{
    d=`echo "$f" | awk '{ print $1; }'`
    if [ "$d" ]; then
	rev=`echo "$f" | awk '{ print $2; }'`
	url=`echo "$f" | awk '{ print $3; }'`
	
	if [ -d "$d" ]; then
	    echo "svn switch $rev $url"
	    (cd $d && svn switch -q $rev $url)
	else
	    echo "svn checkout -q $rev $url $d"
	    svn checkout -q $rev $url $d
	fi
    fi
}

svn propget svn:externals http://svn.iar.se/ide/platform/trunk | while read f; do
    checkout_external "$d"
done

pushd . >/dev/null
mkdir -p Jkc/ARM
cd Jkc/ARM
svn propget svn:externals http://svn.iar.se/ide/platform/trunk/Jkc/ARM | while read f; do
    checkout_external "$d"
done
popd >/dev/null

