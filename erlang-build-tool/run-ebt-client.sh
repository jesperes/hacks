#!/bin/bash

SRCDIR=$(cd $(dirname $0); pwd)
exec erl -noshell -name ebt_client -setcookie `cat $HOME/.erlang.cookie` -pa $SRCDIR -run ebt_client start "$@"
