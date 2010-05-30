#!/bin/zsh

# colors
c1="\033[1;38;5;120m" # bright green
c2="\033[1;38;5;228m" # bright yellow
c3="\033[1;38;5;214m" # bright orange
c4="\033[1;38;5;159m" # bright cyan
c5="\033[1;38;5;175m" # bright purple
c6="\033[1;38;5;160m" # bright red
ce="\033[0m"

if [[ $ARGC -gt 1 ]]; then
    com=$1
    # get command name (i.e., delete all options supplied to it)
    com_binary=$(echo $com | sed 's/ \+/ /g' | cut -d " " -f1)
    f=$2
    gap=1
    if [[ $ARGC -gt 2 && $(echo $3 | sed 's/^[1-9][0-9]*//' | wc -c) -eq 1 ]]; then
        gap=$3
    fi

    # make sure the specified command and file exists
    if [[ $(which $com_binary) == "$com_binary not found" ]]; then
        echo "autocall: invalid command \`$com_binary'"
        exit 1
    elif [[ ! -f $f ]]; then
        echo "autocall: file \`$f' does not exist"
    fi

    echo "autocall: press 'q' to quit"
    echo "autocall: press ENTER or SPACE to execute manually"
    # get current timestamp of file
    tstamp=$(ls --full-time "$f")
    key="xx"
    while true; do
        read -s -t $gap -k key # read a single key
        if [[ $key == "q" ]]; then
            echo "\nautocall: exiting..."
            exit 0
        # note the special notation $'\n' to detect an ENTER key
        elif [[ $key == $'\n' || $key == " " ]]; then
            echo "\nautocall:$c2 [$(date --rfc-3339=ns)]$ce$c4 manual execution$ce"
            echo "autocall: executing $c2$com \"$f\"$ce\n"
            $com "$f" 2>&1 | sed 's/^/  > /'
            key="xx"
        elif [[ "$tstamp" != $(ls --full-time "$f") ]]; then
            echo "\nautocall:$c2 [$(date --rfc-3339=ns)]$ce$c1 change detected$ce"
            echo "autocall: executing $c2$com \"$f\"$ce\n"
            tstamp=$(ls --full-time "$f")
            $com "$f" 2>&1 | sed 's/^/  > /'
        fi
    done
else
    echo "autocall: usage: autocall [command] [file]"
fi

# vim:syntax=zsh
