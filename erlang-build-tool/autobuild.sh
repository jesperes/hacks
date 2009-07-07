#!/bin/bash

function build() 
{
    clear
    if make -j2; then
	echo "--- OK ---"
    else
	echo "*** BUILD FAILED ***"
    fi
}

build
inotifywait -m -e close_write *.c *.erl | while read f; do
    date
    build
done
