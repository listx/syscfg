#!/usr/bin/ruby
# Author: Linus Arver (C) 2009
# License: GPL3
# Program Name: parent.rb
# Description: finds the next closes parent directory of a broken path

file = ARGV.shift

# zsh will read whatever we ultimately "print" to STDOUT!
# NOTE: the if-statements below explain the global conditions -- to understand how this script works, you MUST read
# the custom zsh function that references it -- namely univ_open()
if file.nil?
    print ""
else
    if file.scan("/").empty? # basically, if we've completed a portion of a filename, but not the rest of it
        print "." # let's keep the user in the same directory
    else # if we have indeed completed a filename (at least the user thinks so), but it is not a valid file
        path = file.split("/")
        path.pop
        print path.join("/")
    end
end
