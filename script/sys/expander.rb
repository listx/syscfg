#!/usr/bin/ruby
# Author: Linus Arver (C) 2009
# License: GPL3
# Program Name: ureader.rb
# Description: expands all special characters into their zsh-equivalent; currently, it is not used by zsh

orig = ARGV.shift

# we can't use the original string because it's frozen!
cleaned = ""
cleaned << orig

cleaned.gsub!(" ","\\\\\\ ")
cleaned.gsub!("(","\\\\\\(")
cleaned.gsub!(")","\\\\\\)")
cleaned.gsub!("[","\\\\\\[")
cleaned.gsub!("]","\\\\\\]")
cleaned.gsub!("<","\\\\\\<")
cleaned.gsub!(">","\\\\\\>")

print cleaned
