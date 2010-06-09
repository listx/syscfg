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
#   Same, but filter results (items) with word "boat"
#       clcheck.sh -a joe@domain.com -a mike@domain.edu -e bob@domain.net -w 30 -k boat
#   Same, but disable emails (debug mode)
#       clcheck.sh -a joe@domain.com -a mike@domain.edu -e bob@domain.net -w 30 -k boat -d
#   Same, but disable emails (no -a flag is required)
#       clcheck.sh -w 30 -k boat -d

msg () {
    case $1 in
        "help")
echo "
clcheck: Usage:

clcheck [OPTIONS]

Required parameters:
-a EMAIL_ADDRESS        The email address to send processed updates. Can be used multiple times.

Optional parameters:
-d                      Debug mode. This flag overrides the -a flag, and makes it so that emailing
                        is turned off for everything. The -a flag is not required if the -d flag is
                        present.
-e EMAIL_ADDRESS        The email address to send error logs to. Can be used
                        multiple times. (Uses first address from -a flag by default.)
-k WORD                 Filter results with this word (case insensitive) in the line. Can be used
                        multiple times to simulate 'OR' behavior of search. If only used once
                        without any other -k or -K parameters, then this behaves the same way as
                        a single -K paramter.
-K WORD                 Filter results with this word (case insensitive) in the line. Can be used
                        multiple times to simulate 'AND' behavior of search. (Same as -k paramter
                        if used once alone by itself.) Essentially, all results absolutely require
                        the WORD term given with -K.
-s                      Silent mode; clcheck will output fewer messages.
-u URL                  Use custom craigslist listing page URL instead of default
                        (sfbay.craigslist.org). Only give the address, without the \`http://' (no
                        ending slashes).
-U SUBDIR               The URL must have this word in it (default: boa).

-w DELAY                Wait DELAY seconds before checking for updates. (60 by default.)

-h                      Show this page and exit (regardless of other parameters).
-v                      Show version number and exit (regardless of other parameters).
"
            exit 0
            ;;
        "version")
            echo "clcheck version $clcheck_ver"
            exit 0
            ;;
        *)
            echo "clcheck: $1"
            exit 1
            ;;
    esac
}

parseline () {
    link=""
    descr=""
    price=""
    location=""
    line="$2"
    # this is the master "key" and should match all strings -- if it
    # does fail, then we email the maintainer right away
    pcre_compile "^<a\shref=\"(http.+?)\">(.+?)</a>(.+?)<br"
    pcre_match "$line"
    if [[ $#match -lt 3 ]]; then
        # email maintainer what the failed string was
        echo
        echo -n "$c6"
        echo "clcheck: error parsing line$ce"
        echo "clcheck: size of match array was: $#match"
        echo "clcheck: match array was: \`$match'"
        echo "clcheck: failed line was \`$line'"
        echo
        if [[ $3 -gt 0 ]]; then # only email if we're not on our very first iteration
            if [[ $debugflag == false ]]; then
                echo -n "clcheck: emailing maintainer..."
                for addr in $e_addys; do
                    echo "failed line: \"$line\"\nsize of match: $#match\nmatch contents: $match\n\nver. $clcheck_ver" | mail -s "error: failed line" $addr
                done
                echo "done"
            else
                # exit immediately since we encountered an error
                exit 1
            fi
        fi
    else
        echo -n "$c1"
        echo -n "clcheck: found item "
        # we have a working match!
        # first, get the link and description
        link=$match[1]
        descr=$match[2]

        echo "\`$descr' -> $link$ce"

        # now get the rest: (1) price and/or (2) location from $match[3], if at all
        meta=$match[3]
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

        # now add all the data into a single line, and store it into our output array (we send all the new stuff
        # we found in one email at the end of this loop)
        if [[ $3 -gt 0 ]]; then # only add to textarr if we're not on our very first iteration
            if [[ "$price" != "?" && "$location" != "?" ]]; then # if only both are true
                # textarr should be visible globally, even though it is not passed to this function explicitly
                textarr+=("$1. $descr for $price @ $location - $link")
            elif [[ "$price" != "?" ]]; then # if just price is true
                textarr+=("$1. $descr for $price - $link")
            elif [[ "$location" != "?" ]]; then # if just location is true
                textarr+=("$1. $descr @ $location - $link")
            else # if none are true
                textarr+=("$1. $descr - $link")
            fi
        fi
    fi
    match=()
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

clcheck_ver="1.1"
addys=()
e_addys=()
andflag=false
orflag=false
debugflag=false
silentflag=false
search_ands=()
search_ors=()
search_ands_orig=()
search_ors_orig=()
searchands_com=""
searchors_com=""
filtercom=""
com=""
url="sfbay.craigslist.org"
url_dir="boa"
urlflag=false
delay=0
while getopts ":a:de:k:K:su:U:w:hv" opt; do
    case "$opt" in
    h)  msg "help" ;;
    v)  msg "version" ;;
    *) ;;
    esac
done
# re-parse from the beginning again if there were no -h or -v flags
OPTIND=1
while getopts ":a:de:k:K:su:U:w:" opt; do
    case "$opt" in
    a)
        addys+=("$OPTARG")
        ;;
    d)
        debugflag=true
        ;;
    e)
        e_addys+=("$OPTARG")
        ;;
    k)
        orflag=true
        # should make sure that $OPTARG is made up of non-special characters (alphanumeric)
        search_ors+=("$OPTARG")
        search_ors_orig+=("$OPTARG")
        ;;
    K)
        andflag=true
        search_ands+=("$OPTARG")
        search_ands_orig+=("$OPTARG")
        ;;
    s)
        silentflag=true
        ;;
    u)
        urlflag=true
        url="$OPTARG"
        ;;
    U)
        urlflag=true
        url_dir="$OPTARG"
        ;;
    w)
        delay=$OPTARG
        ;;
    :)
        msg "missing argument for option \`$OPTARG'"
        ;;
    *)
        msg "unrecognized option \`$OPTARG'"
        ;;
    esac
done

#------------------#
# Check for errors #
#------------------#
if [[ -z $addys && $debugflag == false ]]; then
    msg "need at least 1 email address"
fi
#-----------------#
# Set misc values #
#-----------------#
if [[ -z $e_addys ]]; then
    e_addys+=($addys[1])
fi
if [[ $delay -eq 0 ]]; then
    delay=60 # 60 seconds by default
fi
# build "filtercom" regex/unix pipe commands
# the goal is to create a string that looks like " | ..." which can be appended to any other command string to filter
# those results

com_base="curl -s http://$url/$url_dir/ | iconv -cs -t ASCII | grep \"http://$url/.\+\?/[[:digit:]]\+.html\\\">[^<]\" -A1 | sed -e 's/^--//' -e '/^$/d' | sed 's/\t//g' | sed 'N;s/\n//'"

if [[ $orflag == true && $andflag == true ]]; then # both "and" and "or" search terms were given
    filtercom+=" | grep -i \"$search_ors[1]"
    shift search_ors
    for term in $search_ors; do
        filtercom+="\|$term"
    done
    # though it seeems redundant, adding the -K's search terms here and treating them as an OR is required because we
    # pass all $search_ands terms again one at a time in its own pipe
    # e.g., if you have -k a -k b -K C, then a line "C" will NOT get caught with the initial $search_ors pass, and the
    # -K flag will never even get to see it, unless you treat -K C as -k C, as we do here
    for term in $search_ands; do
        filtercom+="\|$term"
    done
    filtercom+="\""
    for term in $search_ands; do
        filtercom+=" | grep -i \"$term\""
    done
elif [[ $andflag == true ]]; then # only have "and" search term(s)
    for term in $search_ands; do
        filtercom+=" | grep -i \"$term\""
    done
elif [[ $orflag == true ]]; then # only have "or" search term(s)
    filtercom+=" | grep -i \"$search_ors[1]"
    shift search_ors
    for term in $search_ors; do
        filtercom+="\|$term"
    done
    filtercom+="\""
else # both search terms were empty
    filtercom=""
fi

com="$com_base$filtercom"

if [[ $debugflag == true ]]; then
    echo "clcheck: *** DEBUG MODE ***"
    echo "clcheck: clcheck will exit immediately after any parsing error"
    sleep 1
fi
if [[ $silentflag == false ]]; then
    echo -n "clcheck: (initializing) getting data..."
    # since we're initializing, use $com_base, not $com (don't filter any result just yet)
    rawdata_old=$(eval $com_base)
    echo "done"
else
    rawdata_old=$(eval $com_base)
fi

#------------#
# Begin loop #
#------------#
c=0
key=""
while true; do
    if [[ $c -gt 0 ]]; then

        # print search/filter terms for easier viewing
        if [[ $silentflag == false ]]; then
            if [[ $orflag == true || $andflag == true ]]; then
                echo -n "clcheck: search terms:"
            else
                echo "clcheck: no search terms set"
            fi
            if [[ $orflag == true ]]; then
                echo -n " -k: "
                for t in $search_ors_orig; do
                    echo -n "\`$t' "
                done
            fi
            if [[ $andflag == true ]]; then
                if [[ $orflag == true ]]; then
                    echo -n "-K: "
                else
                    echo -n " -K: "
                fi
                for t in $search_ands_orig; do
                    echo -n "\`$t' "
                done
            fi
            echo "; using URL \`http://$url/$url_dir/'"
            echo "clcheck: looking for changes again in $delay seconds..."
            echo
        fi

        # read single key from user to see if we should exit gracefully or not (only enter this mode after 1st iteration
        # (initialization) finishes)
        read -s -t $delay -k key # emulate 'sleep $delay'
        if [[ $key == "q" ]]; then
            echo "\nclcheck: exiting..."
            exit 0
        fi
    else
        # on first run, just return the top 10 items to STDOUT (no diffing, no emailing) that match a search term (if search terms
        # were provided), and continue on to next iteration
        rawdata_new=$(eval $com)
        displayme=()
        if [[ $orflag == false && $andflag == false ]]; then # no search flags -- so we just show off the top 10 lines to STDOUT
            echo "clcheck: showing latest 10 posts"
            displayme=("${(f)$(echo -n "$rawdata_new" | head -n 10)}")
        else # one or more search flags were used -- so filter output via those searches
            displayme=("${(f)$(echo -n "$rawdata_new")}")
            if [[ -z $displayme ]]; then # reset $displayme to latest 10 results if no matches were found
                echo "clcheck: front page has no successful search matches"
                echo "clcheck: defaulting to latest 10 posts"
                rawdata_new=$(eval $com_base) # don't filter our results!
                displayme=("${(f)$(echo -n "$rawdata_new" | head -n 10)}")
            else
                echo "clcheck: displaying matched terms"
            fi
        fi
        j=0
        for line in $displayme; do
            let "j++"
            parseline $j $line 0
        done
        rawdata_old=$rawdata_new
        echo "clcheck: finished initializing\n"
        echo "clcheck: press 'q' to exit\n"
        let "c++"
        continue
    fi
    textarr=() # data to send at end of this iteration -- if any
    clines=() # changed lines, if any

    if [[ $silentflag == false ]]; then
        echo -n "clcheck: getting data..."
        rawdata_new=$(eval $com)
        echo "done"
        echo -n "clcheck: looking for new posts..."
    else
        rawdata_new=$(eval $com)
    fi
    rawfresh=""
    # get only the newly-added lines!
    clines=("${(f)$(diff -u0 -B -d <(echo "$rawdata_old") <(echo "$rawdata_new") | sed -e '/^---/d' -e '/^+++/d' -e '/^@/d' -e '/^-/d' | cut -c 2-)}")
    if [[ -n $clines ]]; then
        echo -n "$c1"
        if [[ $silentflag == false ]]; then
            echo " new posts detected$ce"
        fi
        i=0
        for line in $clines; do
            let "i++"
            parseline "$i" "$line" 1
        done

        # only send email if textarr is not empty (it could be that all changed lines all led to errors, in which case
        # textarr is empty at this point)
        if [[ $#textarr -gt 0 ]]; then
            if [[ $debugflag == false ]]; then
                echo -n "\n$c4"
                echo -n "clcheck: emailing data to client..."
                for addr in $addys; do
                    print -C 1 "$textarr" | mail -s "post update" $addr
                done
                textarr=()
                echo "done$ce"
            else
                # if we're in debug mode, don't do anything extra since we didn't fail yet
            fi
        else
            echo -n "$c5"
            echo "clcheck: some posts were changed, but no new ones were added$ce"
        fi
    else
        if [[ $silentflag == false ]]; then
            echo " no new posts"
        fi
    fi

    rawdata_old=$rawdata_new
done

# vim:syntax=zsh
