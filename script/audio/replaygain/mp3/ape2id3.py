#! /usr/bin/env python2

import sys
from optparse import OptionParser

import mutagen
from mutagen.apev2 import APEv2
from mutagen.id3 import ID3, TXXX


def convert_gain(gain):
   if gain[-3:] == " dB":
       gain = gain[:-3]
   try:
       gain = float(gain)
   except ValueError:
       raise ValueError, "invalid gain value"
   return "%.2f dB" % gain

def convert_peak(peak):
   try:
       peak = float(peak)
   except ValueError:
       raise ValueError, "invalid peak value"
   return "%.6f" % peak


REPLAYGAIN_TAGS = (
   ("mp3gain_album_minmax", None),
   ("mp3gain_minmax", None),
   ("replaygain_album_gain", convert_gain),
   ("replaygain_album_peak", convert_peak),
   ("replaygain_track_gain", convert_gain),
   ("replaygain_track_peak", convert_peak),
)
 
 
class Logger(object):
   def __init__(self, log_level, prog_name):
       self.log_level = log_level
       self.prog_name = prog_name
       self.filename = None

   def prefix(self, msg):
       if self.filename is None:
           return msg
       return "%s: %s" % (self.filename, msg)

   def debug(self, msg):
       if self.log_level >= 4:
           print self.prefix(msg)

   def info(self, msg):
       if self.log_level >= 3:
           print self.prefix(msg)

   def warning(self, msg):
       if self.log_level >= 2:
           print self.prefix("WARNING: %s" % msg)

   def error(self, msg):
       if self.log_level >= 1:
           sys.stderr.write("%s: %s\n" % (self.prog_name, msg))

   def critical(self, msg, retval=1):
       self.error(msg)
       sys.exit(retval)


class Ape2Id3(object):
   def __init__(self, logger, force=False):
       self.log = logger
       self.force = force

   def convert_tag(self, name, value):
       pass

   def copy_replaygain_tag(self, apev2, id3, name, converter=None):
       self.log.debug("processing '%s' tag" % name)

       if not apev2.has_key(name):
           self.log.info("no APEv2 '%s' tag found, skipping tag" % name)
           return False
       if not self.force and id3.has_key("TXXX:%s" % name):
           self.log.info("ID3 '%s' tag already exists, skpping tag" % name)
           return False

       value = str(apev2[name])
       if callable(converter):
           self.log.debug("converting APEv2 '%s' tag from '%s'" %
                          (name, value))
           try:
               value = converter(value)
           except ValueError:
               self.log.warning("invalid value for APEv2 '%s' tag" % name)
               return False
           self.log.debug("converted APEv2 '%s' tag to '%s'" % (name, value))

       id3.add(TXXX(encoding=1, desc=name, text=value))
       self.log.info("added ID3 '%s' tag with value '%s'" % (name, value))
       return True

   def copy_replaygain_tags(self, filename):
       self.log.filename = filename
       self.log.debug("begin processing file")

       try:
           apev2 = APEv2(filename)
       except mutagen.apev2.error:
           self.log.info("no APEv2 tag found, skipping file")
           return
       except IOError:
           e = sys.exc_info()
           self.log.error("%s" % e[1])
           return

       try:
           id3 = ID3(filename)
       except mutagen.id3.error:
           self.log.info("no ID3 tag found, creating one")
           id3 = ID3()

       modified = False
       for name, converter in REPLAYGAIN_TAGS:
           copied = self.copy_replaygain_tag(apev2, id3, name, converter)
           if copied:
               modified = True
       if modified:
           self.log.debug("saving modified ID3 tag")
           id3.save(filename)

       self.log.debug("done processing file")
       self.log.filename = None


def main(prog_name, options, args):
   logger = Logger(options.log_level, prog_name)
   ape2id3 = Ape2Id3(logger, force=options.force)
   for filename in args:
       ape2id3.copy_replaygain_tags(filename)


if __name__ == "__main__":
   parser = OptionParser(version="0.1", usage="%prog [OPTION]... FILE...",
                         description="Copy APEv2 ReplayGain tags on "
                                     "FILE(s) to ID3v2.")
   parser.add_option("-q", "--quiet", dest="log_level",
                     action="store_const", const=0, default=1,
                     help="do not output error messages")
   parser.add_option("-v", "--verbose", dest="log_level",
                     action="store_const", const=3,
                     help="output warnings and informational messages")
   parser.add_option("-d", "--debug", dest="log_level",
                     action="store_const", const=4,
                     help="output debug messages")
   parser.add_option("-f", "--force", dest="force",
                     action="store_true", default=False,
                     help="force overwriting of existing ID3v2 "
                          "ReplayGain tags")
   prog_name = parser.get_prog_name()
   options, args = parser.parse_args()

   if len(args) < 1:
       parser.error("no files specified")

   try:
       main(prog_name, options, args)
   except KeyboardInterrupt:
       pass


# vim: set expandtab shiftwidth=4 softtabstop=4 textwidth=79:
