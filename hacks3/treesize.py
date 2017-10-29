#!/usr/bin/python

import sys
import os
from os.path import *

class TreeSize:

    def format_size(self, size):
        kiB = 1024
        MiB = 1024*kiB
        GiB = 1024*MiB

        fsize = float(size)
        if size > GiB:
            return "%.2f Gb" % (fsize/GiB)
        elif size > MiB:
            return "%.2f Mb" % (fsize/MiB)
        elif size > kiB:
            return "%d kb" % (size/kiB)
        else:
            return "%d bytes" % (fsize)


if __name__ == "__main__":
    ts = TreeSize()
    ts.calculate(sys.argv[1])
