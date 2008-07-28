#!/bin/bash

SRCDIR=$(cd $(dirname $0); pwd)
(cd $SRCDIR; make)
exec erl -noinput -pa $SRCDIR -run ebt_server

