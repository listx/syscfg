#!/bin/zsh
# Convert a .org file (only headings tested for now) into a bunch of .ogg files (each .ogg file is a
# single heading line from the .org file).

file=$1
out_dir=$2
num=0

# Error checking
if [[ ! -f $file ]]; then
    echo "File \`$file' does not exist."
    exit 1
fi

if [[ ! -d $out_dir ]]; then
    echo "Directory \`$out_dir' does not exist."
    exit 1
fi

# Read the .org file
<$file while read line; do
    # skip comment lines
    if [[ ! $line[1] == '#' ]]; then
        clean=$(echo "$line" | sed 's/\*\+//')
        line_arr+=("$clean")
    fi
done

# Convert each line into its own ogg file (and print $num 4 digits wide with leading zeroes)
for line in $line_arr; do
    let num+=1
    echo "$line" | text2wave | oggenc - -o $out_dir/${(l:4::0:)num}.ogg
done

echo "Done."
# vim: syntax=zsh
