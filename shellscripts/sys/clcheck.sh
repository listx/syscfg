#!/bin/zsh
# clcheck -- a craigslist checker
# Dependencies: msmtp
# we need zsh's pcre regex module
zmodload zsh/pcre

# Examples:
#
#   Send email updates (as well as error messages) to joe@domain.com, checking every 30 seconds.
#       clcheck.sh -a joe@domain.com -w 30
#   Same, but send the same email also to mike@domain.edu
#       clcheck.sh -a joe@domain.com -a mike@domain.edu -w 30
#   Same, but send error messages separately to bob@domain.net
#       clcheck.sh -a joe@domain.com -a mike@domain.edu -e bob@domain.net -w 30

msg () {
    case $1 in
        "help")
echo "
clcheck: Usage:

clcheck [OPTIONS]

Required parameters:
-a EMAIL_ADDRESS        The email address to send processed updates. Can be used multiple times.

Optional parameters:
-e EMAIL_ADDRESS        The email address to send error logs to. Can be used
                        multiple times. (Uses first address from -a flag by default.)
-w DELAY                Wait DELAY seconds before checking for updates. (60 by default.)

-h                      Show this page and exit (regardless of other parameters).
-v                      Show version number and exit (regardless of other parameters).
"
            exit 0
            ;;
        "version")
            echo "clcheck version 1.0"
            exit 0
            ;;
        *)
            echo "clcheck: $1"
            exit 1
            ;;
    esac
}

# colors
c1="\x1b[1;38;5;120m" # bright green
c2="\x1b[1;38;5;228m" # bright yellow
c3="\x1b[1;38;5;214m" # bright orange
c4="\x1b[1;38;5;159m" # bright cyan
c5="\x1b[1;38;5;175m" # bright purple
c6="\x1b[1;38;5;160m" # bright red
ce="\x1b[0m"

#----------------#
# PROGRAM START! #
#----------------#

addys=()
e_addys=()
delay=0
while getopts ":a:e:w:hv" opt; do
    case "$opt" in
    h)  msg "help" ;;
    v)  msg "version" ;;
    *) ;;
    esac
done
# re-parse from the beginning again if there were no -h or -v flags
OPTIND=1
while getopts ":a:e:w:" opt; do
    case "$opt" in
    a)
        addys+=("$OPTARG")
        ;;
    e)
        e_addys+=("$OPTARG")
        ;;
    w)
        delay=$OPTARG
        ;;
    *)
        ;;
    esac
done

#-----------------#
# Set misc values #
#-----------------#
if [[ -z $addys ]]; then
    msg "need at least 1 email address"
fi
if [[ -z $e_addys ]]; then
    e_addys+=($addys[1])
fi
if [[ $delay -eq 0 ]]; then
    delay=60 # 60 seconds by default
fi

# search all
#com="curl -s http://sfbay.craigslist.org/boa/ | grep \"http://sfbay.craigslist.org/\w\+/boa/[[:digit:]]\+\" -C 1 | sed -e 's/^\s\+<p\sclass.\+//' -e '/^$/d' -e 's/^--//' | sed 's/\t//g' | sed 'N;s/\n//' | sed 'N;s/\n//'"
# search only "outboard"
com="curl -s http://sfbay.craigslist.org/boa/ | grep \"http://sfbay.craigslist.org/\w\+/boa/[[:digit:]]\+\" -C 1 | sed -e 's/^\s\+<p\sclass.\+//' -e '/^$/d' -e 's/^--//' | sed 's/\t//g' | sed 'N;s/\n//' | sed 'N;s/\n//' | grep -i \"outboard\""
echo -n "clcheck: (initializing) getting data..."
rawdata_old=$(eval $com) # put each line as an element into an array
echo "done"

#------------#
# Begin loop #
#------------#
c=0
key=""
while true; do
    # read single key from user to see if we should exit gracefully or not (only enter this mode after 1st iteration (initialization) finishes)

    if [[ $c -gt 0 ]]; then
        echo "clcheck: looking for changes again in $delay seconds"
        read -s -t $delay -k key # emulate 'sleep $delay'
        if [[ $key == "q" ]]; then
            echo "\nclcheck: exiting..."
            exit 0
        fi
    fi
    textarr=() # data to send at end of this iteration -- if any

    echo -n "clcheck: getting data..."
    rawdata_new=$(eval $com) # put each line as an element into an array
    echo "done"
    echo "clcheck: looking for new posts..."
    changed=$(diff -u0 -B -d <(echo "$rawdata_old") <(echo "$rawdata_new"))
    if [[ $(echo -n $changed | wc -l) -gt 0 ]]; then
        echo -n "$c1"
        echo "clcheck: new posts detected$ce"
        # get only the newly-added lines!
        clines=("${(f)$(echo -n $changed | sed -e '/^---/d' -e '/^+++/d' -e '/^@/d' -e '/^-/d' | cut -c 2-)}")
        i=0
        for line in $clines; do
            # reset all variables every iteration
            link=""
            descr=""
            price=""
            location=""
            # this is the master "key" and should match all strings -- if it
            # does fail, then we email the maintainer right away
            pcre_compile "^<a\shref=\"http.+?\".+?\"(http.+?)\">(.+?)</a>(.+?)<br"
            pcre_match "$line"
            if [[ $#match -lt 3 ]]; then
                # email maintainer what the failed string was
                echo
                echo -n "$c6"
                echo "clcheck: error parsing line$ce"
                echo -n "clcheck: emailing maintainer..."
                for addr in $e_addys; do
                    echo "failed line: \"$line\"\nsize of match: $#match\nmatch contents: $match" | mail -s "error: failed line" $addr
                done
                echo "done"
                echo "clcheck: size of match array was: $#match"
                echo "clcheck: match array was: $match"
                echo "clcheck: failed line was \`$line'"
                echo
            else
                let "i++"
                echo -n "$c1"
                echo -n "clcheck: found item "
                # we have a working match!
                # first, get the link and description
                link=$match[1]
                descr=$match[2]

                echo "\`$descr' -> $link$ce"

                # now get the other stuff -- the goal is to extract the (1) price and/or (2) location from $match[3], if at all
                meta=$match[3]

                #echo "clcheck: parsing string \'$match[3]'"
                match=() # clear match array for next regex

                # first extract the location, if any
                pcre_compile "font.+?\((.+?)\)</font"
                pcre_match "$meta"
                if [[ -z $#match ]]; then
                    location="?"
                else
                    location=$match[1]
                fi
                match=() # clear match array for next regex

                # now look for price, if any
                pcre_compile "^.+?(\\\$\d+)"
                pcre_match "$meta"
                if [[ -z $#match ]]; then
                    price="?"
                else
                    price=$match[1]
                fi
                match=() # clear match array for next regex

                echo "  Price: $c2$price$ce"
                echo "  Where: $c3$location$ce"

                # now add all the data into a single line, and store it into our output array (we send all the new stuff we found in one email at the end of this loop)
                if [[ "$price" != "?" && "$location" != "?" ]]; then
                    textarr+=("$i. $descr for $price @ $location - $link\n")
                elif [[ "$price" == "?" && "$location" == "?" ]]; then
                    textarr+=("$i. $descr - $link\n")
                elif [[ "$price" != "?" && "$location" == "?" ]]; then
                    textarr+=("$i. $descr for $price - $link\n")
                else
                    textarr+=("$i. $descr @ $location - $link\n")
                fi
            fi
        done

        # only send email if textarr is not empty (it could be that all changed lines all led to errors, in which case textarr is empty at this point)
        if [[ $#textarr -gt 0 ]]; then
            echo -n "\n$c4"
            echo -n "clcheck: emailing data to client..."
            for addr in $addys; do
                echo "$textarr" | mail -s "post update" $addr
            done
            echo "done$ce"
        else
            echo -n "$c5"
            echo "clcheck: some posts were changed, but no new ones were added$ce"
        fi
    else
        echo "clcheck: no new posts detected"
    fi


    if [[ $c -eq 0 ]]; then
        # don't sleep at all, since it's our first iteration
        echo "clcheck: finished initializing\n"
        echo "clcheck: press 'q' to exit\n"
        let "c++"
    fi
    rawdata_old=$rawdata_new
done

# vim:syntax=zsh
