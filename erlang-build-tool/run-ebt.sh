#!/bin/bash

SRCDIR=$(cd $(dirname $0); pwd)
exec erl -name ebt_server -pa $SRCDIR -run ebt_server start "$1"
