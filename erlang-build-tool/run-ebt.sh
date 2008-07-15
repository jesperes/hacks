#!/bin/bash

(cd $(dirname $0); make -j)
# echo "$@"
exec erl -noinput -s ebt main -- "$@" --
