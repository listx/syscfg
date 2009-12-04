#!/usr/bin/ruby
# Author: Linus Arver (C) 2009
# License: GPL3
# Program Name: ureader.rb
# Description: returns an appropriate shell command to open up a file based on its extension

require '/home/listdata/syscfg/shellscripts/sys/ureader_extensions.rb'

file = ARGV.shift

# zsh will read whatever we ultimately "print" to STDOUT!
if file.nil? # if the user does not specify an argument
    print ""
else # figure out file extension
    case file.split(".").last.downcase
    # document formats
    #========================#
    # when "doc","odf","rtf" #
    #     print "soffice"    #
    #========================#
    when *Ext.doc
        print "soffice"
    when *Ext.web
        print "firefox"
    when *Ext.pdf
        print "evince"
    # image files -- typical simple files for plain viewing
    when *Ext.img
        print "eog"
    # image files -- ones meant to be edited (strange looking ones and also gimp's native format)
    when *Ext.img2
        print "gimp"
    # audio files
    when *Ext.audio
        print "mplayer"
    # midi files
    when *Ext.midi
        print "timidity"
    # movies
    when *Ext.movie
        print "mplayer"
    else # use vim to access files by default, including those without an extension
        print "vim"
    end
end
