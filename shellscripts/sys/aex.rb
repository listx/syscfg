#!/usr/bin/ruby
# Author: Linus Arver (C) 2009
# License: GPL3
# Program Name: AEX (Auto-EXtractor)
# Description: Unarchive an archive based on its file extension in an intelligent way

#==================#
# Global variables #
#==================#
Clron = "\e[1;38;5;159m"
Clrof = "\e[0m"

#==============================================#
# Pretty string colors (ANSI escape sequences) #
#==============================================#
def aex_msg(dir_new, dir_predefined)
    if dir_new.empty?
        if File.exists?(dir_predefined) && File.directory?(dir_predefined)
            puts"#{Clron} --[ aex: Cancelling extraction because \`./#{dir_predefined}\' already exists ]--#{Clrof}"
        else
            puts "#{Clron} ,--[ aex: Extracting archive to \`./#{dir_predefined}\' ]--#{Clrof}"
        end
    else
        if File.exists?(dir_new) && File.directory?(dir_new)
            puts"#{Clron} --[ aex: Cancelling extraction because \`./#{dir_new}\' already exists ]--#{Clrof}"
        else
            puts "#{Clron} ,--[ aex: Extracting archive to \`./#{dir_new}\' ]--#{Clrof}"
        end
    end
end

#===============#
# Pretty output #
#===============#
def aex_show(arr)
    arr.each do |line|
        puts "#{Clron} |#{Clrof} #{line}\n"
    end

    print "#{Clron}"
    print " `"
    print "-" * (arr.last.size + 1)
    puts "#{Clrof}"
end

#==============================================#
# Checks if a directory does not already exist #
#==============================================#
def dne(dir)
    if File.exists?(dir) && File.directory?(dir)
        return false
    else
        return true
    end

end

#==========================#
# Archive-specific methods #
#==========================#
def unzip(archive)
    # let's be optimistic, and hope that this archive is NOT a zip bomb (an archive with multiple files/directories at
    # root level); we'll only set this flag to true if we detect a zip bomb
    failflag = false

    # array to prettify before it is shown
    aexarr = []

    # figure out what the archive's name is, without the extension
    leadname = archive.split(".").first(archive.split(".").size - 1).join(".")

    arr = `unzip -Z1 "#{archive}"`.split("\n") # list contents of archive

    #==================================================================================================================#
    # if the first listing is a DIRECTORY, then check that ALL subsequent files/directories in this archive are inside #
    # this directory; if not (i.e., there are multiple root-level archived files/directories), then just put them all  #
    # inside a NEW directory named after the archive                                                                   #
    #==================================================================================================================#

    dir_or_file_name = arr.shift
    # here, "dir_or_file_name" looks something like this:
    #     stockfish-151-ja/
    # if it has a final forward slash like above, it's a directory
    if dir_or_file_name.reverse[0] == "/" # if this is true, then it's a directory!
        dir = dir_or_file_name.chop # "dir" is now just the name of the directory, but without the last forward slash
        # now let's check if all the rest of the items in the archive are under this directory
        arr.each do |item|
            if item.split("/").shift.scan(dir).empty?
                # this means that the file name does NOT contain our dir name up in the very front (e.g., if "dir" is
                # GOODDIR, then something like "someotherdir/GOODDIR/file" will fail just as equally as "file")
                failflag = true
                break
            end
        end

        # if this is a zip bomb, unzip to a new directory, instead of the current working directory
        if failflag == true
            # we use quotes around the archive name, since spaces and other special characters are NOT escaped in the
            # _archive_ variable; likewise, the _leadname_ variable is placed in quotes as well
            aex_msg(leadname, "")
            if dne(leadname)
                `unzip "#{archive}" -d "#{leadname}"`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
        else
            # if no zip bomb, simply unzip as-is (a new directory will be created if it's a single file)
            aex_msg("", dir)
            if dne(dir)
                `unzip "#{archive}"`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
        end
    else
        # we know that the first item in the archive is NOT a directory, meaning that there are multiple files or
        # directories under the root area!
            aex_msg(leadname, "")
            if dne(leadname)
                `unzip "#{archive}" -d "#{leadname}"`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
    end
end

def unrar(archive)
    failflag = false
    aexarr = []

    leadname = archive.split(".").first(archive.split(".").size - 1).join(".")

    arr = []
    arrv = [] # verbose version
    content_listing = 0
    `unrar vt "#{archive}"`.split("\n").each do |line|
        # the "vt" option in the program "unrar" lists archived contents by preceding them with a single space
        if line[0] == " " && line[1] != " "
            # also, the directories are always listed last (opposite of unzip's behavior), so we have to ".pop" instead
            # of ".shift" below
            arr << line
        end

        # only store the various lines inside the content listing
        if content_listing == 0
            if line == "-------------------------------------------------------------------------------"
                content_listing = 1
            end
        else
            if line == "-------------------------------------------------------------------------------"
                content_listing = 0
            end
        end

        if content_listing == 1
            arrv << line
        end
    end


    dir_or_file_name = arr.pop # <-- pop!
    # do the same popping, but for arrv (arrv has 3 lines per item b/c of the verbose output)
    darr = []
    3.times {darr << arrv.pop}
    # now, darr[1] will look like below if it is a directory:
    #                     0        0   0% 12-11-09 01:55 drwxr-xr-x 00000000 m0  2.0
    # so we need to read the drwxr... part and if we find a 'd', it's a directory!

    if darr[1].strip.split(" ")[5][0] == 'd'
        dir = dir_or_file_name
        arr.each do |item|
            if item.split("/").shift.scan(dir).empty?
                failflag = true
                break
            end
        end

        if failflag == true
            aex_msg(leadname, "")
            if dne(leadname)
                `unrar x "#{archive}" "#{leadname}"/`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
        else
            aex_msg("", dir)
            if dne(dir)
                `unrar x "#{archive}"`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
        end
    else
            aex_msg(leadname, "")
            if dne(leadname)
                `unrar x "#{archive}" "#{leadname}"/`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
    end
end

def untargz(archive)
    failflag = false
    aexarr = []
    leadname = archive.split(".").first(archive.split(".").size - 2).join(".") # minus 2, since there's "tar" and "gz"
    arr = `tar tf "#{archive}"`.split("\n")
    dir_or_file_name = arr.shift
    if dir_or_file_name.reverse[0] == "/"
        dir = dir_or_file_name.chop
        arr.each do |item|
            if item.split("/").shift.scan(dir).empty?
                failflag = true
                break
            end
        end

        if failflag == true
            aex_msg(leadname, "")
            if dne(leadname)
                `mkdir "#{leadname}"`.split("\n").each {|line| aexarr << line}

                # two v's for even more verbosity than a single v
                `tar zxvvf "#{archive}" -C "#{leadname}"`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
        else # a pre-defined, single directory is inside the archive, so we test if this directory already exists
            aex_msg("", dir)
            if dne(dir)
                `tar zxvvf "#{archive}"`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
        end
    else
            aex_msg(leadname, "")
            if dne(leadname)
                `mkdir "#{leadname}"`.split("\n").each {|line| aexarr << line}
                # two v's for even more verbosity than a single v
                `tar zxvvf "#{archive}" -C "#{leadname}"`.split("\n").each {|line| aexarr << line}
                aex_show(aexarr)
            end
    end
end

#====================#
# BEGIN MAIN PROGRAM #
#====================#
if ARGV.size > 0
    archive = ARGV.shift
    if File.directory?(archive)
        puts "#{Clron}--[ aex: `#{archive}' is a directory, not an archive ]--#{Clrof}\n"
    else
        case archive
        when /\.tar\.gz$/i
            untargz(archive)
        when /zip$/i    # zip archive
            unzip(archive)
        when /rar$/i    # rar archive
            unrar(archive)
        else
            puts "#{Clron}--[ aex: `#{archive}' is not a recognized archive type ]--#{Clrof}\n"
            puts "#{Clron}--[ aex: .tar.gz, .zip, and .rar are supported ]--#{Clrof}\n"
        end
    end
else
    puts "#{Clron}--[ aex: No archive specified ]--#{Clrof}\n"
end
