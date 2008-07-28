#!/bin/bash
clear

inotifywait -m -e close_write *.c *.erl | while read f; do
    clear
    date
    make -j2
done
