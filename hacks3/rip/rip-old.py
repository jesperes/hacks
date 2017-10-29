#!/usr/bin/python

#
# rip.py -- wrapper around mplayer/mencoder to rip/compress video streams
#

import os
import re
import sys
import optparse
import popen2
import time

aid = 128
sid = -1
device = "/dev/cdrom"
vcodec = "mpeg4"
default_bitrate = 1400

usage = "usage: %prog [options] file"

parser = optparse.OptionParser(usage)

parser.add_option("-b", "--bitrate", dest = "bitrate", metavar = "KBPS",
                  type = "int",
                  default = default_bitrate,
                  help =
"""Bitrate in kbit/s of the output video stream. Defaults to %d.""" % default_bitrate)

parser.add_option("-s", "--size", dest = "filesize", metavar = "MB", type = "int",
                  help =
"""Desired output file size in MB. Overrides bitrate.""")

parser.add_option("-o", "--output", dest = "output_file", metavar = "FILE",
                  default = "output.avi",
                  help = "Name of the output file.")

parser.add_option("--preview", dest = "preview",
                  action = "store_true",
                  help = "Encode a preview of the movie.")

parser.add_option("--preview-length", "-l", type = "int", dest = "preview_length", metavar = "SECS",
                  default = 30,
                  help = "Length of preview.")

parser.add_option("--preview-start", "-w", dest = "preview_start", metavar = "SECS", type = "int",
                  default = 300,
                  help = "Start position of preview")

parser.add_option("--crop-start-pos", "-p", type = "int", dest = "crop_start_pos", metavar = "SECS",
                  default = 300,
                  help = "Where to start doing crop-detection.")

parser.add_option("--deinterlace", action = "store_true", dest = "deinterlace",
                  help = "Use deinterlace postprocessing filter.")

parser.add_option("--aid", dest = "aid",
                  default = aid,
                  help = "Audio language ID. See the -aid option in the mplayer manpage.")

parser.add_option("--sid", dest = "sid",
                  default = sid,
                  help = "Subtitle language ID. See the -sid option in the mplayer manpage.")


class ProgressBar:
    def __init__(self):
        self.width = 68
        self.info_width = 7
        self.eta = ""
        self.reset()

    def update(self, percentage, info_str):
        c = int(float(self.width) * percentage)
        s = self.width - c
        now = time.time()

        # Throttle updates to four per second
        if now - self.last_update < .25:
            return

        self.last_update = now

        if percentage > 0:
            elapsed_time = now - self.start_time
            estimated_total_time = elapsed_time / percentage
            estimated_time_left = estimated_total_time - elapsed_time

            # update only when eta decreases
            # if estimated_time_left < self.old_eta_secs:

            self.eta = time.strftime(" ETA: %H:%M:%S",  time.gmtime(estimated_time_left))
            self.old_eta_secs = estimated_time_left

        sys.stdout.write("\r%-*s[%s%s]%s" % (self.info_width, info_str, "=" * c, " " * s, self.eta))
        sys.stdout.flush()

    def reset(self):
        sys.stdout.write("\r" + " " * (2 + self.width + self.info_width + len(self.eta)) + "\r")
        sys.stdout.flush()

        self.start_time = time.time()
        self.last_update = time.time()
        self.old_eta_secs = 10000000
        self.eta = ""

class MPlayer:
    def __init__(self):
        self.progress = ProgressBar()
        self.logfile = None
        
    def update_progress_bar(self, output_line):
        m = re.match("Pos: *([0-9\.]+)s *([0-9]+)f *\(([ 0-9]+)%\).*", output_line)
        if m:
            secs = float(m.group(1))
            percent = secs / self.length
            self.progress.update(percent, "%.1f%% " % (percent * 100.0))

        if self.logfile:
            self.logfile.write(output_line.strip() + "\n")
    

    def run(self, title, cmdline, logfile = "mplayer.out"):
        self.logfile = open(logfile, "w")
        
        print cmdline
        print "Command output to:", logfile
        
        self.progress.reset()

        if options.preview:
            self.length = options.preview_length
        else:
            self.length = title.get_length()

        fout = os.popen("nice %s 2>&1" % cmdline, "r", 32)

        buf = ""
        while True:
            output = fout.read(32)
            if output == "":
                break

            output = output.replace("\r", "\n")

            buf += output

            while True:
                p = buf.find("\n")
                if p == 0:
                    buf = buf[1:]
                    continue
                elif p > 0:
                    line = buf[:p]
                    self.update_progress_bar(line)
                    buf = buf[p:]
                else:
                    break

        self.progress.reset()
        self.logfile.close()
        self.logfile = None
        

class Dimension:
    def __init__(self, w, h):
        self.width = float(w)
        self.height = float(h)

    def aspect_ratio(self):
        return self.width / self.height

    def copy(self):
        return Dimension(self.width, self.height)

    def __str__(self):
        return "%d x %d, aspect %.02f" % (self.width, self.height, self.aspect_ratio())

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

class VideoInputException(Exception):
    pass


class DVDProber:
    def __init__(self, url):
        self.url = url

    def get_url(self):
        return self.url
    
    def probe(self):
        raise Exception("not implemented")

    def get_dimension(self):
        raise Exception("not implemented")

    def get_length(self):
        raise Exception("not implemented")

    def get_bitrate(self):
        raise Exception("not implemented")

    def get_format(self):
        raise Exception("not implemented")

    def get_aspect(self):
        raise Exception("not implemented")
    
class TCProbe(DVDProber):
    def probe(self):
        self.dimension = None
        self.length = 0
        self.bitrate = 0
        self.format = None
        self.aspect = 0
        
        url = self.get_url()
        m = re.match("dvd://(.*)", url)
        if m:
            cmd = "tcprobe -i %s -T %s 2>&1" % (device, m.group(1))
        else:
            cmd = "tcprobe -i %s 2>&1" % (url)
            
        for line in os.popen(cmd):
            if line.find("import frame size") != -1:
                m = re.match(".*-g (.*)x(.*) \[.*", line)
                if m:
                    self.dimension = Dimension(int(m.group(1)),
                                               int(m.group(2)))
                continue

            # Format is stated in the first [tcprobe] line
            if line.find("[tcprobe]") != -1 and not self.format:
                self.format = line[len("[tcprobe] "):].strip()
                continue

            if self.length == 0:
                if line.find("[tcprobe] V:") != -1:
                    m = re.match(".*, (.*) sec .*", line)
                    if m:
                        self.length = int(m.group(1))
                        continue

            if self.length == 0:
                if line.find("length: ") != -1:
                    m = re.match(".*length: (.*) frames, frame_time=(.*) msec.*", line)
                    if m:
                        self.length = int(int(m.group(1)) * float(m.group(2))/1000.0)

            if self.aspect <= 0:
                if line.find("aspect ratio") != -1:
                    m = re.match(".*aspect ratio: ([^ ]*):([^ ]*).*", line)
                    if m:
                        self.aspect = float(m.group(1))/float(m.group(2))
                        
    def get_dimension(self):
        return self.dimension

    def get_length(self):
        return self.length

    def get_bitrate(self):
        return self.bitrate

    def get_format(self):
        return self.format

    def get_aspect(self):
        return self.aspect
    
    
class MIdentify(DVDProber):
    def probe(self):
        self.info = {}
        
        for line in os.popen("midentify %s" % self.get_url()):
            var, val = line.strip().split("=")
            self.info[var] = val

        if len(self.info.keys()) == 0:
            raise VideoInputException("no such title: %s" % self.get_url())
    
    def get_dimension(self):
        """Return the physical size of the video input stream (i.e. without any scaling)"""
        return Dimension(int(self.info['ID_VIDEO_WIDTH']),
                         int(self.info['ID_VIDEO_HEIGHT']))

    def get_length(self):
        return int(self.info['ID_LENGTH'])

    def get_bitrate(self):
        return int(self.info['ID_VIDEO_BITRATE'])

    def get_format(self):
        return self.info['ID_VIDEO_FORMAT']

    def get_aspect(self):
        return float(self.info['ID_VIDEO_ASPECT'])
    
        

class VideoInput:
    def __init__(self):
        self.info = {}
        self.crop_dimension = None
        self.crop_area_pos = None

        self.probe = TCProbe(self.get_url())
        self.probe.probe()

    def get_dimension(self):
        return self.probe.get_dimension()

    def get_bitrate(self):
        "Return the bitrate to use for encoding."
        if options.filesize > 0:
            length = self.probe.get_length()
            filesize_kbit = options.filesize * 1024 * 8
            return (filesize_kbit / length) 
        else:
            return options.bitrate

    def get_aspect(self):
        return self.probe.get_aspect()

    def get_url(self):
        """Return a string which can be passed to mplayer as a input file identifier."""
        pass

    def get_length(self):
        return self.probe.get_length()
    
    def __str__(self):
        return "url = %s (%5s secs, format = %s, bitrate = %d kbps)" % \
               (self.get_url(), self.get_length(),
                self.probe.get_format(),
                self.probe.get_bitrate() / 1024)

    def cropdetect(self):
        print "Detecting crop area..."

        # TODO: sanity check that the crop area actually works

        crop_option = None

        mplayer_cmd = ["mplayer",
                       title.get_url(),
                       "-ao null",
                       "-ss %d" % options.crop_start_pos,
                       "-vf cropdetect",
                       "-frames 100 2>&1 | grep 'crop area' | tail -n 1"]

        print " ".join(mplayer_cmd)

        for line in os.popen(" ".join(mplayer_cmd)).readlines():
            m = re.match(".*-vf crop=((\d+):(\d+):(\d+):(\d+))\).*", line)
            if m:
                width = int(m.group(2))
                height = int(m.group(3))

                x = int(m.group(4))
                y = int(m.group(5))

                if width % 16 != 0:
                    raise Exception("crop area width not a multiple of 16!")

                if height % 16 != 0:
                    raise Exception("crop area height not a multiple of 16!")

                crop_option = "-vf crop=%d:%d:%d:%d" % (width, height, x, y)

                self.crop_dimension = Dimension(width, height)
                self.crop_area_pos = Point(x, y)

        if not crop_option:
            raise Exception("Crop detection failed")

        return crop_option

    def get_movie_aspect(self):
        """Return the actual movie aspect, i.e. how to scale the resulting (cropped)
        video stream for viewing."""
        if not self.crop_dimension:
            return 0.0

        return self.crop_dimension.aspect_ratio() * self.get_aspect() / \
               self.get_dimension().aspect_ratio()


    def encode(self):
        mplayer = MPlayer()

        os.system("rm -f frameno.avi divx2pass.log")

        filter = self.cropdetect()

        if options.preview:
            print "Encoding", title, "(%d second preview)" % options.preview_length
            preview = "-ss %d -endpos %d" % (options.preview_start, options.preview_length)
        else:
            print "Encoding", title
            preview = ""

        lavcopts = "vcodec=%s:vbitrate=%d:autoaspect" % (vcodec, self.get_bitrate())
        lavcopts = lavcopts + ":vhq"
        # lavcopts = lavcopts + ":v4mv:mbd=2:trell"
        # lavcopts = lavcopts + ":v4mv:mbd=2:trell:cmp=3:subcmp=3:mbcmp=3"
        movie_aspect = title.get_movie_aspect()

        # aspect = "-force-avi-aspect %.2f" % movie_aspect
        lang = ""

        if options.aid:
            lang = "-aid %s" % options.aid

        if options.sid >= 0:
            lang = "-sid %s -subalign 1" % options.sid

        if options.deinterlace:
            filter = filter
 + ",pp=ci"
        
        print "Video filter:", filter
        # print "Aspect ratio: %.2f" % movie_aspect

        t0 = time.time()

        print "Extracting audio..."
        mplayer.run(title, " ".join(["mencoder",
                                     self.get_url(),
                                     preview,
                                     "-ovc frameno",
                                     "-o frameno.avi",
                                     "-oac copy",
                                     "-lameopts abr:br=128",
                                     ]))
        t1 = time.time()
        print time.strftime("Audio pass: %H:%M:%S",  time.gmtime(t1 - t0))

        print "Encoding (pass 1)..."
        mplayer.run(title, " ".join(["mencoder",
                                     self.get_url(),
                                     preview,
                                     filter,
                                     lang,
                                     "-oac copy",
                                     "-o /dev/null",
                                     "-ovc lavc",
                                     "-lavcopts %s:vpass=1" % lavcopts,
                                     ]))

        t2 = time.time()
        print time.strftime("Encoder pass 1: %H:%M:%S",  time.gmtime(t2 - t1))

        print "Encoding (pass 2)..."
        mplayer.run(title, " ".join(["mencoder",
                                     self.get_url(),
                                     preview,
                                     filter,
                                     lang,
                                     "-oac copy",
                                     "-o", options.output_file,
                                     "-ovc lavc",
                                     "-lavcopts %s:vpass=2" % lavcopts,
                                     ]))
        t3 = time.time()
        print time.strftime("Encoder pass 2: %H:%M:%S",  time.gmtime(t3 - t2))

        print "Output file:", options.output_file
        os.system("rm -f frameno.avi divx2pass.log")

class DVDTitle(VideoInput):
    def __init__(self, num):
        self.title_num = num
        VideoInput.__init__(self)

    def get_url(self):
        return "dvd://%d" % self.title_num

class FileTitle(VideoInput):
    def __init__(self, filename):
        self.filename = filename
        VideoInput.__init__(self)

    def get_url(self):
        return self.filename

class DVD_TOC:
    def __init__(self):
        self.toc = []

        print "Probing DVD titles (this might take a while)..."
        n = 1
        while 1:
            try:
                title = DVDTitle(n)
                print title

            except VideoInputException, msg:
                break

            if title.get_length() > 0:
                self.toc.append(title)

            n += 1

    def __str__(self):
        if len(self.toc) == 0:
            return "No titles (insert DVD?)"

        s = ""
        for title in self.toc:
            s += str(title) + "\n"

        return s

    def guess_movie_title(self):
        """Return the first title on the DVD longer than an hour."""
        for title in self.toc:
            if title.get_length() > 3600:
                return title




if __name__ == "__main__":
    (options, args) = parser.parse_args()

    if len(args) == 0:
        dvd_toc = DVD_TOC()
    else:
        filename = args[0]
        title = FileTitle(filename)
        print title
        title.encode()
