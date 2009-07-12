#!/usr/bin/ruby -w
# A simple ruby script to wake me up.

# a simple method to colorize output
# NOTE: the *color argument is an array
def princ(str, *color)
    if !color.empty?
        puts "\033[1;#{color}m#{str}\033[0m"
    else # default color is white
        puts "\033[1;37m#{str}\033[0m"
    end
end

# same as princ(), but with centered output
def princc(str, width, *color)
    output = ""
    left_padding = (width - str.size) / 2
    if !color.empty?
        output = "\033[1;#{color}m#{str}\033[0m"
    else # default color is white
        output = "\033[1;37m#{str}\033[0m"
    end
    left_padding.times {output.insert(0, " ")}
    puts output
end

width = 80

# display welcome message
#--------------------------#
# str = ""                 #
# width.times {str << "*"} #
# princ(str)               #
#--------------------------#
puts ""
princc("Good morning!", width)
puts ""


# display calendar
# the pal program outputs its calendar with a width of 76 characters
str = `pal -f /home/listdata/syscfg/pal/exelion.cfg`
a = str.split("\n")
a.each do |line|
    2.times {line.insert(0, " ")}
    puts line
end


# unmute if sound is muted, and set the volume to 0
`amixer -q set Master 0 unmute`

# start playing song in current playlist
`ncmpcpp play`

# # this for loop takes 30s to set volume from 70 to 100
# for ((i = 70; i <= 100; i++)) do
#     amixer -q set Master $i\%
#         # sleep for 1 second
#             sleep 1s
#             done
#

for i in (0..100)
    `amixer -q set Master #{i}%`
    sleep(5) # wait for 1 second
end

puts ""
princ("Press any key to exit this message...")

#http://stackoverflow.com/questions/174933/how-to-get-a-single-character-in-ruby-without-pressing-enter
# Getting a single keystroke input from the user to exit.

begin
  system("stty raw -echo")
  str = STDIN.getc
ensure
  system("stty -raw echo")
end
