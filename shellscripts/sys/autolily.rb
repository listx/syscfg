#!/usr/bin/ruby
#===============================================================================================================#
# Program name: Autolily                                                                                        #
# LICENSE: PUBLIC DOMAIN                                                                                        #
# This program takes 1 argument, the name of a lilypond file (*.ly), and watches it for changes every 1 second. #
# If there has been any change, it simply calls lilypond on it to create a new .pdf/.ps/.midi of it.            #
#                                                                                                               #
# Place this script somewhere, like in ~/scripts                                                                #
# Then, open up a terminal and call it like so: ~/scripts/autolily.rb [file]                                    #
# [file] must be a LilyPond file (.ly), but it can be located anywhere -- i.e., you may include paths in your   #
# file, such as "~/sheet-music/classical/bach2.ly" or "../../bach3.ly".                                         #
#                                                                                                               #
# You might want to do a "sudo ln -s" of autolily.rb to one of your system's $PATH directories (e.g., /usr/bin) #
# to avoid typing out the path to autolily.rb every time you use it. Continuing the example from above,         #
# something like "sudo ln -s ~/scripts/autolily.rb /usr/bin/autolily" should do (make sure that                 #
# /usr/bin/autolily does not exist already, as the above comman will overwrite that file if it exists).         #
#                                                                                                               #
# Now you can just do:                                                                                          #
#                                                                                                               #
#     autolily [file]                                                                                           #
#                                                                                                               #
# from anywhere in your system!                                                                                 #
#                                                                                                               #
# To exit, press CTRL-C.                                                                                        #
#===============================================================================================================#

if ARGV.size > 0
    file_data_orig = ""
    file = ARGV.shift
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
            puts "\n\e[1;4;38;5;226mAutolily: Change detected in given file; invoking lilypond...\e[0m\n"
            if pathsize > 1
            `lilypond "#{path_to_file}/#{file}"`
            else
            `lilypond "#{file}"`
            end
            file_data_orig = file_data_new
        end
        sleep 1
    end
else
    puts "No .ly file specified.\n"
end
