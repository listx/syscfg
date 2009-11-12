#!/usr/bin/ruby
# Author: Linus Arver
# Date: 2009
# License: GPL3
# Program Name: AEX (Auto-EXtractor)
# Description: Unarchive an archive based on its file extension in an intelligent way

#==========================#
# Archive-specific methods #
#==========================#
def unzip(archive)
    # let's be optimistic, and hope that this archive is NOT a zip bomb (an archive with multiple files/directories at
    # root level); we'll only set this flag to true if we detect a zip bomb
    failflag = false

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
            puts `unzip "#{archive}" -d "#{leadname}"`
        else
            # if no zip bomb, simply unzip as-is
            puts `unzip "#{archive}"`
        end
    else
        # we know that the first item in the archive is NOT a directory, meaning that there are multiple files or
        # directories under the root area!
            puts `unzip "#{archive}" -d "#{leadname}"`
    end
end

def unrar(archive)
    failflag = false

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
            puts `unrar x "#{archive}" "#{leadname}"/`
        else
            puts `unrar x "#{archive}"`
        end
    else
            puts `unrar x "#{archive}" "#{leadname}"/`
    end
end

#====================#
# BEGIN MAIN PROGRAM #
#====================#
if ARGV.size > 0
    archive = ARGV.shift
    case archive.split(".").last
    when /^rar$/i    # rar archive
        unrar(archive)
    when /^zip$/i    # zip archive
        unzip(archive)
    end
else
    puts "No archived file specified.\n"
end
