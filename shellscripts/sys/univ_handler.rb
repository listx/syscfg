#!/usr/bin/ruby
# Author: Shinobu (http://zuttobenkyou.wordpress.com)
# License: PUBLIC DOMAIN
# Program Name: univ_handler.rb
# Description: with the help of smart input from zsh, this script helps zsh open up directories and files intelligently

class Ext
    @doc = %w{doc odf odt rtf}
    @web = %w{htm html}
    @pdf = %w{pdf ps}
    @img = %w{bmp gif jpg jpeg png svg tiff}
    @img2 = %w{psd xcf}
    @audio = %w{flac mp3 wma}
    @midi = %w{mid midi}
    @movie = %w{avi flv ogg mkv mov mp4 mpg mpeg wmv}

    class<<self; attr_reader :doc end
    class<<self; attr_reader :web end
    class<<self; attr_reader :pdf end
    class<<self; attr_reader :img end
    class<<self; attr_reader :img2 end
    class<<self; attr_reader :audio end
    class<<self; attr_reader :midi  end
    class<<self; attr_reader :movie end
end

file = ARGV.shift
arg = ARGV.shift

# zsh will read whatever we ultimately "print" to STDOUT!
if file.nil? # if the user does not specify an argument
    print ""
else # figure out file extension
    if arg == "extension"
        case file.split(".").last.downcase
        when *Ext.doc
            print "soffice -writer"
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
            print "mplayer -loop 0"
        # midi files
        when *Ext.midi
            print "timidity"
        # movies
        when *Ext.movie
            print "mplayer -loop 0"
        else # use vim to access files by default, including those without an extension
            print "vim"
        end
    elsif arg == "parent" # find the closest valid parent directory
        if file.scan("/").empty? # basically, if we've completed a portion of a filename, but not the rest of it
            print "." # let's keep the user in the same directory
        else # if we have indeed completed a filename (at least the user thinks so), but it is not a valid file
            path = file.split("/")
            path.pop
            # return 1 directory higher than this one -- we assume that the reader used tab completions enough to avoid
            # having more than 1 bad directory/file level (that only the text following the last "/" is invalid)
            print path.join("/")
        end
    end
end
