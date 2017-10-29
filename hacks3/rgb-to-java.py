#!/usr/bin/python

import sys
import re

print "/* automatically generated from /usr/X11R6/lib/X11/rgb.txt */"
print "\npackage com.virtutech.vdb.configurator.figures;"
print "import org.eclipse.swt.graphics.Color;\n"
print "public class ColorNames {"

colors = {}

for colorspec in [ line.split()
                   for line in sys.stdin.readlines()
                   if not re.match("^!.*", line.strip()) ]:
    r, g, b = colorspec[0:3]
    names = colorspec[3:]

    for name in names:
        colors[name.lower()] = [int(r), int(g), int(b)]


colornames = colors.keys()
colornames.sort()

for colorname in colornames:
    r, g, b = colors[colorname]
    
    print "\tstatic public final Color %s = new Color(null, %d, %d, %d);" % \
          (colorname, r, g, b)

print "}"

        
