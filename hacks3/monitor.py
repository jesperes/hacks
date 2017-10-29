#!/usr/bin/python

import pygtk
pygtk.require("2.0")
import gtk
import gobject
import commands
import re
import sys
import os
import codecs

class Processor:
    def __init__(self):
        self.update()

    def update(self):
        self.update_freq()
        self.update_policy()

        return gtk.TRUE

    def update_freq(self):
        s = commands.getoutput("x86info -mhz | grep processor")
        m = re.match("(.*)Ghz processor.*", s)
        if m:
            self.freq = int(float(m.group(1)) * 1000)
            return
        
        m = re.match("(.*)MHz processor.*", s)
        if m:
            self.freq = int(float(m.group(1)))
            return
        
        self.freq = 0

    def update_policy(self):
        s = commands.getoutput("speedfreq")
        m = re.match("Current policy is ([a-z]+).*", s)
        if m:
            self.policy = m.group(1)
        else:
            self.policy = "?"


class Battery:
    def __init__(self):
        self.capacity = 0
        self.remaining = 0
        self.rate = 0
        self.level = 0                  # percent
        self.state = "?"
        
        self.update()

    def update(self):
        f = open("/proc/acpi/battery/C11F/info", "r")
        lines = f.readlines()
        f.close()
        
        for l in lines:
            attr, val = l.split(":")
            attr = attr.strip()
            val = val.strip()
            
            if attr == "design capacity":
                self.capacity = int(val.split()[0])

        f = open("/proc/acpi/battery/C11F/state", "r")
        lines = f.readlines()
        f.close()
        
        for l in lines:
            attr, val = l.split(":")

            attr = attr.strip()
            val = val.strip()
            
            if attr == "remaining capacity":
                self.remaining = int(val.split()[0])
                
            elif attr == "present rate":
                self.rate = int(val.split()[0])

            elif attr == "charging state":
                if val == "charged":
                    self.state = "AC"
                elif val == "charging":
                    self.state = "AC"
                elif val == "discharging":
                    self.state = "Offline"
            
        self.level = 100.0 * (float(self.remaining) / float(self.capacity))
        return gtk.TRUE

class MPC:
    def __init__(self):
        self.title = "?"
        self.state = "?"

    def update(self):
        s = os.popen("mpc").readlines()[0]
        s = s.strip().replace("&", "&amp;").decode("latin1").encode("utf-8")
        self.title = s
        return gtk.TRUE


        

class Monitor:
    def __init__(self):
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.set_title("monitor")

        # self.window.set_border_width(1)
        #frame = gtk.Frame()
        # frame.set_shadow_type(gtk.SHADOW_ETCHED_OUT)
        #self.window.add(frame)

        vbox = gtk.VBox(spacing = 0)
        # frame.add(vbox)
        self.window.add(vbox)
        
        self.label = gtk.Label()
        self.label.set_use_markup(gtk.TRUE)
        self.label.set_alignment(0.0, 0.5)
        vbox.add(self.label)
        vbox.set_border_width(3)
        self.window.connect("delete-event", self.delete_event)
        self.window.connect("destroy-event", self.delete_event)

        self.window.show_all()

        self.proc = Processor()
        self.batt = Battery()
        self.mpc = MPC()

        self.size_ok = 0
        self.update()
        self.size_ok = 0
        
        gobject.timeout_add(1000, self.update)
        gobject.timeout_add(1000, self.mpc.update)
        gobject.timeout_add(5000, self.batt.update)
        gobject.timeout_add(3000, self.proc.update)

    def delete_event(self, widget, event, data = None):
        gtk.main_quit()
        return gtk.FALSE

    def main(self):
        gtk.main()

    def update(self):
        if not self.size_ok:
            w, h = self.window.size_request()
            self.label.set_size_request(w, h)
            self.size_ok = 1

        title0 = ""
        title1 = ""

        title = [ s.strip() for s in self.mpc.title.split(" - ") if s.strip() != "Various" ]

        if len(title) >= 2:
            title0 = title[0]
            title1 = " ".join(title[1:])
        else:
            title0 = ""
            title1 = title[0]

        
        s = "<span size=\"x-small\">"
        s += "%dMHz (%s)" % (self.proc.freq, self.proc.policy)
        s += " : %s (%d%%)" % (self.batt.state, self.batt.level)
        s += "\n"
        s += "<i>" + title0 + "</i>\n"
        s += "<span size=\"small\" weight=\"bold\">" + title1 + "</span>"
        s += "</span>"

        print "\"%s\"" % s
        
        self.label.set_label(s)
        return gtk.TRUE

Monitor().main()

         

                        
    
