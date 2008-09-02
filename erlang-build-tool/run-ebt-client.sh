#!/bin/bash

SRCDIR=$(cd $(dirname $0); pwd)
exec erl -noshell -name ebt_client -pa $SRCDIR -run ebt_client start "$@"
