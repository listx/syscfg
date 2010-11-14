#!/bin/zsh
# utp.sh: urxvt theme picker
zmodload zsh/pcre

# usage: utp.sh [name] or just utp.sh (will select a random theme)

root_dir="/home/$USER/syscfg/script/sys/terms/"

pcre_compile "^c_(.+)=\"(#[0-9a-f]+)"

set_theme() {
    <"$root_dir$1.sh" while read line; do
        if [[ -n $line ]]; then
            pcre_match "$line"
            if [[ $#match -gt 1 ]]; then
                case $match[1] in
                    00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15)
                        echo -en "\x1b]4;$match[1];$match[2]\007"
                        ;;
                    fg)
                        echo -en "\x1b]10;$match[2]\007"
                        ;;
                    bg)
                        echo -en "\x1b]11;$match[2]\007"
                        echo -en "\x1b]708;$match[2]\007"
                        ;;
                    cursor)
                        echo -en "\x1b]12;$match[2]\007"
                        ;;
                    *)
                        echo "utp: invalid color format in $1.sh"
                        ;;
                esac
            fi
        fi
    done
}

if [[ $#@ -eq 1 ]]; then
    # if provided an argument, try to use it if valid; otherwise, spit out an error
    if [[ -f "$root_dir$1.sh" ]]; then
        set_theme $1
    else
        echo "utp: \`$1.sh' does not exist under ~/syscfg/script/sys/terms"
        echo "utp: available color themes:"
        echo $(ls "$root_dir")
        echo "utp: choose desired theme without the .sh extension"
    fi
else
    # randomly pick a color theme if no argument was provided
    theme=$(ls "$root_dir" | sort -R | head -n 1)
    set_theme $(basename $theme .sh)
fi

# vim:syntax=zsh
