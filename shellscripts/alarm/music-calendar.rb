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

def graduated_alarm_bell()
    alive = true

    princ("Press 's' to stop the music.")

    Thread.new {
        while alive
            for i in (30..100)
                `amixer -q set Master #{i}%`
                # sleep for an ever-increasing duration of seconds -- but after each second, check for user input
                for j in (1..((i/15)**2))
                    sleep(j)
                    if alive == false
                        Thread.kill
                    end
                end
            end
        end
    }

    while true
        system("stty -echo -icanon min 1 time 0")
        str = STDIN.getc
        char = str.chr

        case char
        when 's'
            alive = false
            `ncmpcpp pause`
            break # exit this method and give control back to the caller
        else
            puts "character #{char.inspect} not recognized"
            princ("Press 's' to stop the music.")
        end
    end
end

width = 80

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

puts ""


# unmute if sound is muted, and set the volume to 0
`amixer -q set Master 0 unmute`

# start playing song in current playlist
`ncmpcpp play`

graduated_alarm_bell()

puts ""
princ("Press 'p' to turn computer power off, or 'q' to exit this message...")

while true
    system("stty -echo -icanon min 1 time 0")
    str = STDIN.getc
    char = str.chr

    case char
    when 'q'
        puts ""
        princc("Have an excellent day!", width)
        puts ""
        break
    when 'p'
        `sudo poweroff`
    else
        puts "character #{char.inspect} not recognized"
        princ("Press 'p' to turn computer power off, or 'q' to exit this message...")
    end
end
