#!/bin/bash

SRCDIR=$(cd $(dirname $0); pwd)
(cd $SRCDIR; make)
exec erl -name ebt_client -pa $SRCDIR -run ebt_client start "$@"
