#!/usr/bin/python

import os, sys

db = "/home/jojo/dvddb.txt"

class DVD:
    def __init__(self):
        self.director = ""
        self.original_title = ""
        self.actors = ""
        self.soundtrack = ""
        self.length = ""
        self.year = ""
        self.region = ""
        self.format = ""
        self.rating = ""
        self.audio = ""
        self.discs = ""
        self.category = ""
        self.notes = ""

    def __str__(self):
        return self.title

    def __repr__(self):
        return self.__str__()

dvd_list = []
dvd = None

for line in open(db, "r"):
    line = line.strip()

    if len(line) == 0:
        dvd = None
        continue

    if line[0] == '#':
        continue

    sep = line.find(":")
    if sep == -1:
        print "No separator found:", line
        continue

    field = line[:sep].strip().lower()
    value = line[sep + 1:].strip()

    if not dvd:
        dvd = DVD()
        dvd_list.append(dvd)

    setattr(dvd, field, value)

def dvd_sort_func(a, b):
    return cmp(a.title, b.title)

dvd_list.sort(dvd_sort_func)



def format_dvdlist(list):
    attrs = ['actors', 'audio', 'category', 'director', 'discs',
             'format', 'length', 'notes', 'original_title',
             'rating', 'region', 'soundtrack', 'title', 'year']


    columns = [ ('title', 30),
                ('director', 20),
                ('length', 6),
                ('media', 10),
                ('actors', 50) ]

    def draw_border(left_char, center_char, fill_char, right_char):
        sep = left_char + fill_char
        for field, width in columns:
            sys.stdout.write("%s%s" % (sep, fill_char * width))
            sep = fill_char + center_char + fill_char
        sys.stdout.write(fill_char + right_char + "\n")

    def draw_text(separator, contents):
        sep = separator + " "

        for text, width in contents:
            sys.stdout.write("%s%-*s" % (sep, width, text[:width]))
            sep = " " + separator + " "

        sys.stdout.write(" " + separator + "\n")

    draw_border("+", "+", "-", "+")
    draw_text("|", [ (field.capitalize(), width) for field, width in columns ])
    draw_border("+", "+", "-", "+")

    for dvd in list:
        draw_text("|", [ (getattr(dvd, field), width) for field, width in columns ])

    draw_border("+", "+", "-", "+")

    print "Total: %d movies, %d minutes total playing time" % \
          (len(dvd_list), sum([int(dvd.length) for dvd in dvd_list if len(dvd.length) > 0]))

def format_dvdlist_mysql(list):
    attrs = ['actors', 'audio', 'category', 'director', 'discs',
             'format', 'length', 'notes', 'original_title',
             'rating', 'region', 'soundtrack', 'title', 'year']

    for dvd in list:
        print dvd.title + "\t" + \
              dvd.original_title + "\t" +\
              dvd.director + "\t" +\
              dvd.length + "\t" +\
              dvd.year + "\t" +\
              dvd.region + "\t" +\
              dvd.format + "\t" +\
              dvd.rating + "\t" +\
              dvd.discs + "\t" +\
              dvd.category + "\t" +\
              dvd.media + "\t"
        
        
if __name__ == "__main__":
    # format_dvdlist(dvd_list)
    format_dvdlist_mysql(dvd_list)
