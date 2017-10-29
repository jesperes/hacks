#!/usr/bin/python

import curses
import sft
import time
import sys

def foo(*args):
    print "foo:", args

simics = sft.Simics()
print sys.argv
simics.launch(sys.argv[1:])
# simics.launch("foobar")
# simics.register_callback("SFT_Protocol_Message", foo, None)
sft.handle_events(-1)

