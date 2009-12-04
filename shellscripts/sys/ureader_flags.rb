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
        print "-writer"
    when *Ext.web
    when *Ext.pdf
    # image files -- typical simple files for plain viewing
    when *Ext.img
    # image files -- ones meant to be edited (strange looking ones and also gimp's native format)
    when *Ext.img2
    # audio files
    when *Ext.audio
        print "-loop 0"
    # midi files
    when *Ext.midi
    # movies
    when *Ext.movie
        print "-loop 0"
    else # use vim to access files by default, including those without an extension
    end
end
