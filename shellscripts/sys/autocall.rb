#!/usr/bin/ruby
#===============================================================================================================#
# Program name: Autocall                                                                                        #
# LICENSE: PUBLIC DOMAIN                                                                                        #
#                                                                                                               #
# This program takes 2 or 3 arguments; the first 2 is the command and file, while the third optional arg is the #
# delay b/n each possible execution of the command. By default this delay is 1 second (it checks if the file has#
# been modified every second)                                                                                   #
#                                                                                                               #
# Place this script somewhere, like in ~/scripts                                                                #
# Then, open up a terminal and call it like so: ~/scripts/autocall.rb [program] [file]                          #
#                                                                                                               #
# You might want to do a "sudo ln -s" of autocall.rb to one of your system's $PATH directories (e.g., /usr/bin) #
# to avoid typing out the path to autocall.rb every time you use it. Continuing the example from above,         #
# something like "sudo ln -s ~/scripts/autocall.rb /usr/bin/autocall" should do (make sure that                 #
# /usr/bin/autocall does not exist already, as the above comman will overwrite that file if it exists).         #
#                                                                                                               #
# Now you can just do:                                                                                          #
#                                                                                                               #
#     autocall [command] [file]                                                                                 #
#                                                                                                               #
# from anywhere in your system!                                                                                 #
#                                                                                                               #
# To exit, press CTRL-C.                                                                                        #
#===============================================================================================================#

if ARGV.size > 1
    file_data_orig = ""
    call = ARGV.shift
    file = ARGV.shift
    delay = 1
    if ARGV.size > 0
        delay = ARGV.shift.to_i
    end
    pathsize = file.split("/").size
    ls_call = "ls --full-time"

    # make sure that the "file" variable is a filename, and not mixed with its path
    if pathsize > 1
        path_to_file = file.split("/").first(pathsize - 1).join("/")
        file = file.split("/").last
        ls_call << " #{path_to_file}" # modify our `ls` command to reflect relative location of file
    end

    `#{ls_call}`.split("\n").each do |line|
        if line.split(/\s/).last == file
            file_data_orig = line
            break
        end
    end
    file_data_new = ""

    # enter infinite loop -- keep compiling the given lilypond file if it has changed in the past 1 second
    while true
        # detect the file size and also timestamp
        lsarr = `#{ls_call}`.split("\n")
        lsarr.shift # get rid of the first line, since that is the size of all the files in the directory

        # find our file from ls's output!
        lsarr.each do |line|
            if line.split(/\s/).last == file
                file_data_new = line
                break
            end
        end

        # if there is any change detected, run lilypond on it
        if file_data_orig != file_data_new
            puts "\n\e[1;38;5;226mautocall: change detected @ #{Time.now.ctime} in file `#{file}'; invoking `#{call}'...\e[0m\n"
            if pathsize > 1
            `#{call} "#{path_to_file}/#{file}"`
            else
            `#{call} "#{file}"`
            end
            file_data_orig = file_data_new
        end
        sleep delay
    end
else
    puts "Usage: autocall [command] [file]\n"
end
