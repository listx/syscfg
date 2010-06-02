#!/bin/zsh
# PROGRAM: autocall
# AUTHOR: Linus Arver <linus@ucla.edu>
# LICENSE: PUBLIC DOMAIN
#
#
# DESCRIPTION:
#
# Autocall watches (1) a single file, (2) directory, and/or (3) a text file
# containing a list of files/directories, and if the watched files and/or
# directories become modified, runs the given command.
#
#
# USAGE:
#
# See msg("help") function below -- read that portion first!
#
#
# USER INTERACTION:
#
# Pressing a SPACE or ENTER key forces execution of COMMAND immediately.
# To exit autocall gracefully, press "q".
#
#
# DEFAULT SETTINGS:
#
# (-w) DELAY    = 5
# (-x) FACTOR   = 4
#
#
# EXAMPLES:
#
# Execute "pdflatex article.tex" every time "article.tex" or "ch1.tex" is
# modified (if line count changes in either file; modification checked every 5
# seconds by default):
#    autocall -c "pdflatex article.tex" -f article.tex -f ch1.tex
#
# Same, but only look at "ch1.tex", and automatically execute every 4 seconds:
#    autocall -c "pdflatex article.tex" -f ch1.tex -w 1 -x 4
#       (-x 0 or -x 1 here would also work)
#
# Same, but also automatically execute every 20 (5 * 4) seconds:
#    autocall -c "pdflatex article.tex" -f ch1.tex -x 4
#
# Same, but automatically execute every 5 (5 * 1) seconds (-w is 5 by default):
#    autocall -c "pdflatex article.tex" -f ch1.tex -x 1
#
# Same, but automatically execute every 1 (1 * 1) second:
#    autocall -c "pdflatex article.tex" -f ch1.tex -w 1 -x 1
#
# Same, but automatically execute every 17 (1 * 17) seconds:
#    autocall -c "pdflatex article.tex" -f ch1.tex -w 1 -x 17
#
# Same, but for "ch1.tex", watch its byte size, not line count:
#    autocall -c "pdflatex article.tex" -b ch1.tex -w 1 -x 17
#
# Same, but for "ch1.tex", watch its timestamp instead (i.e., every time
# this file is saved, the modification timestamp will be different):
#    autocall -c "pdflatex article.tex" -F ch1.tex -w 1 -x 17
#
# Same, but also look at the contents of directory "images/ocean":
#    autocall -c "pdflatex article.tex" -F ch1.tex -d images/ocean -w 1 -x 17
#
# Same, but also look at the contents of directory "other" recursively:
#    autocall -c "pdflatex article.tex" -F ch1.tex -d images/ocean -D other -w 1 -x 17
#
# Same, but look at all files and/or directories (recursively) listed in file
# "watchlist" instead:
#    autocall -c "pdflatex article.tex" -l watchlist -w 1 -x 17
#
# Same, but also look at "newfile.tex":
#    autocall -c "pdflatex article.tex" -l watchlist -f newfile.tex -w 1 -x 17
#
###############################################################################
###############################################################################

#-----------------#
# Local functions #
#-----------------#

msg () {
    case $1 in
        "help")
echo "
autocall: Usage:

autocall [OPTIONS]

Required parameter:
-c COMMAND      The command to be executed (put this in quotes).

One or more required parameters (but see -x below):
-f FILE         File to be watched. Modification detected by line-size.
-F FILE         File to be watched. Modification detected by time.
-b FILE         File to be watched. Modification detected by bytes.
-d DIRECTORY    Directory to be watched. Modification detected by time.
-D DIRECTORY    Directory to be watched, recursively. Modification
                detected by time.
-l FILE         Text file containing a list of files/directories (each on
                its own line) to be watched (directories listed here are
                watched recursively). Modification is detected with 'ls'.

Optional parameters:
-w DELAY        Wait DELAY seconds before checking on the watched
                files/directories for modification; default 5.
-t TIMEOUT      If COMMAND does not finish execution after TIMEOUT seconds,
                send a SIGTERM signal to it (but do nothing else afterwards).
-k KDELAY       If COMMAND does not finish execution after TIMEOUT,
                then wait KDELAY seconds and send SIGKILL to it if COMMAND is
                still running. If only -k is given without -t, then -t is
                automatically set to the same value as TIMEOUT.
-x FACTOR       Automatically execute the command repeatedly every DELAY *
                FACTOR seconds, regardless of whether the watched
                files/directories were modified. If FACTOR is zero, it is set
                to 1. If -x is set, then -f, -d, and -l are not required (i.e.,
                if only the -c and -x options are specified, autocall will
                simply act as a while loop executing COMMAND every 20 (or
                more if FACTOR is greater than 1) seconds). Since the
                formula is (DELAY * FACTOR) seconds, if DELAY is 1,
                FACTOR's face value itself, if greater than 0, is the seconds
                amount.
-a              Same as \`-x 1'
-h              Show this page and exit (regardless of other parameters).
-v              Show version number and exit (regardless of other parameters).
"
            exit 0
            ;;
        "version")
            echo "autocall version 1.0"
            exit 0
            ;;
        *)
            echo "autocall: $1"
            exit 1
            ;;
    esac
}

is_number () {
    if [[ $(echo $1 | sed 's/^[0-9]\+//' | wc -c) -eq 1 ]]; then
        true
    else
        false
    fi
}

autocall_exec () {
    col=""
    case $4 in
        1) col=$c1 ;;
        2) col=$c2 ;;
        3) col=$c3 ;;
        4) col=$c4 ;;
        5) col=$c5 ;;
        6) col=$c6 ;;
        *) col=$c1 ;;
    esac
    echo "\nautocall:$c2 [$(date --rfc-3339=ns)]$ce$col $5$ce"
    if [[ $# -eq 7 ]]; then
        diff -u0 -B -d <(echo "$6") <(echo "$7") | tail -n +4 | sed -e "/^[@-].\+/d" -e "s/\(\S\+\s\+\S\+\s\+\S\+\s\+\S\+\s\+\)\(\S\+\s\+\)\(\S\+\s\+\S\+\s\+\S\+\s\+\)/\1$c1\2$ce$c2\3$ce/" -e "s/^/  $c1>$ce /"
        echo
    fi
    echo "autocall: calling command \`$c4$1$ce'..."
    # see the "z" flag under PARAMTER EXPANSION under "man ZSHEXPN" for more info
    if [[ $tflag == true || $kflag == true ]]; then
        # the 'timeout' command gives nice exit statuses -- it gives 124 if
        # command times out, but if the command exits with an error of its own,
        # it gives that error number (so if the command doesn't time out, but
        # exits with 4 or 255 or whatever, it (the timeout command) will exit
        # with that number instead)

        # note: if kflag is true, then tflag is always true
        com_exit_status=0
        if [[ $kflag == true ]]; then
            timeout -k $killdelay $timeout ${(Q)${(z)1}} 2>&1 | sed "s/^/  $col>$ce /"
            com_exit_status=$pipestatus[1]
        else
            timeout $timeout ${(Q)${(z)1}} 2>&1 | sed "s/^/  $col>$ce /"
            com_exit_status=$pipestatus[1]
        fi
        if [[ $com_exit_status -eq 124 ]]; then
            echo -n $c6
            echo "\nautocall: command timed out$ce"
        elif [[ $com_exit_status -ne 0 ]]; then
            echo -n $c6
            echo "\nautocall: command exited with error status $com_exit_status$ce"
        else
            echo -n $c1
            echo "\nautocall: command executed successfully $com_exit_status$ce"
        fi
    else
        ${(Q)${(z)1}} 2>&1 | sed "s/^/  $col>$ce /"
        com_exit_status=$pipestatus[1]
        if [[ $com_exit_status -ne 0 ]]; then
            echo -n $c6
            echo "\nautocall: command exited with error status $com_exit_status$ce"
        else
            echo -n $c1
            echo "\nautocall: command executed successfully $com_exit_status$ce"
        fi
    fi
}

change_detected () {
}

#------------------#
# Global variables #
#------------------#

# colors
c1="\x1b[1;38;5;120m" # bright green
c2="\x1b[1;38;5;228m" # bright yellow
c3="\x1b[1;38;5;214m" # bright orange
c4="\x1b[1;38;5;159m" # bright cyan
c5="\x1b[1;38;5;175m" # bright purple
c6="\x1b[1;38;5;160m" # bright red
ce="\x1b[0m"

com=""
delay=5
xdelay_factor=4
f=()
F=()
b=()
d=()
D=()
l=()
l_targets=()
wflag=false
xflag=false
tflag=false
kflag=false
timeout=0
killdelay=0

tstampf="" # used to DISPLAY modification only for -f flag
linestamp="" # used to DETECT modification only for -f flag
tstampF="" # used to detect AND display modifications for -F flag
tstampb="" # used to DISPLAY modification only for -b flag
bytestamp="" # used to DETECT modification only for -b flag
tstampd="" # used to detect AND display modifications for -d flag
tstampD="" # used to detect AND display modifications for -D flag
tstampl="" # used to detect AND display modifications for -l flag

tstampf_new=""
linestamp_new=""
tstampF_new=""
tstampb_new=""
bytestamp_new=""
tstampd_new=""
tstampD_new=""
tstampl_new=""

#----------------#
# PROGRAM START! #
#----------------#

#---------------#
# Parse options #
#---------------#

# the leading ":" in the opstring silences getopts's own error messages;
# the colon after a single letter indicates that that letter requires an
# argument

# first parse for the presence of any -h and -v flags (while silently ignoring
# the other recognized options)
while getopts ":c:w:f:F:b:d:D:l:t:k:x:ahv" opt; do
    case "$opt" in
    h)  msg "help" ;;
    v)  msg "version" ;;
    *) ;;
    esac
done
# re-parse from the beginning again if there were no -h or -v flags
OPTIND=1
while getopts ":c:w:f:F:b:d:D:l:t:k:x:a" opt; do
    case "$opt" in
    c)
        com="$OPTARG"
        com_binary=$(echo $com | sed 's/ \+/ /g' | cut -d " " -f1)
        if [[ $(which $com_binary) == "$com_binary not found" ]]; then
            msg "invalid command \`$com_binary'"
        fi
        ;;
    w)
        if $(is_number "$OPTARG"); then
            if [[ $OPTARG -gt 0 ]]; then
                wflag=true
                delay=$OPTARG
            else
                msg "DELAY must be greater than 0"
            fi
        else
            msg "invalid DELAY \`$OPTARG'"
        fi
        ;;
    f)
        if [[ ! -f "$OPTARG" ]]; then
            msg "file \`$OPTARG' does not exist"
        else
            f+=("$OPTARG")
        fi
        ;;
    F)
        if [[ ! -f "$OPTARG" ]]; then
            msg "file \`$OPTARG' does not exist"
        else
            F+=("$OPTARG")
        fi
        ;;
    b)
        if [[ ! -f "$OPTARG" ]]; then
            msg "file \`$OPTARG' does not exist"
        else
            b+=("$OPTARG")
        fi
        ;;
    d)
        if [[ ! -d "$OPTARG" ]]; then
            msg "directory \`$OPTARG' does not exist"
        else
            d+=("$OPTARG")
        fi
        ;;
    D)
        if [[ ! -d "$OPTARG" ]]; then
            msg "directory \`$OPTARG' does not exist"
        else
            D+=("$OPTARG")
        fi
        ;;
    l)
        if [[ ! -f $OPTARG ]]; then
            msg "file \`$OPTARG' does not exist"
        else
            l+=("$OPTARG")
        fi
        ;;
    t)
        tflag=true
        if $(is_number "$OPTARG"); then
            if [[ $OPTARG -gt 0 ]]; then
                timeout=$OPTARG
            else
                msg "TIMEOUT must be greater than 0"
            fi
        else
            msg "invalid TIMEOUT \`$OPTARG'"
        fi
        ;;
    k)
        kflag=true
        if $(is_number "$OPTARG"); then
            if [[ $OPTARG -gt 0 ]]; then
                killdelay=$OPTARG
            else
                msg "TIMEOUT must be greater than 0"
            fi
        else
            msg "invalid KDELAY \`$OPTARG'"
        fi
        ;;
    x)
        xflag=true
        if $(is_number "$OPTARG"); then
            if [[ $OPTARG -gt 0 ]]; then
                xdelay_factor=$OPTARG
            elif [[ $OPTARG -eq 0 ]]; then
                xdelay_factor=1
            else
                msg "invalid FACTOR \`$OPTARG'"
            fi
        fi
        ;;
    a) xflag=true ;;
    :)
        msg "missing argument for option \`$OPTARG'"
        ;;
    *)
        msg "unrecognized option \`$OPTARG'"
        ;;
    esac
done

#-----------------#
# Set misc values #
#-----------------#

if [[ $kflag == true && $tflag == false ]]; then
    tflag=true
    timeout=$killdelay
fi

#------------------#
# Check for errors #
#------------------#

# check that the given options are in good working order
if [[ -z $com ]]; then
    msg "help"
elif [[ (-n $f && -n $d && -n $D && -n $l) && $xflag == false ]]; then
    echo "autocall: see help with -h"
    msg "at least one or more of the (1) -f, -d, -D, or -l paramters, or (2) the -x parameter, required"
fi

#-------------------------------#
# Record state of watched files #
#-------------------------------#

if [[ -n $f ]]; then
    if [[ $#f -eq 1 ]]; then
        linestamp=$(wc -l $f)
    else
        linestamp=$(wc -l $f | head -n -1) # remove the last "total" line
    fi
    tstampf=$(ls --full-time $f)
fi
if [[ -n $F ]]; then
    tstampF=$(ls --full-time $F)
fi
if [[ -n $b ]]; then
    if [[ $#b -eq 1 ]]; then
        bytestamp=$(wc -c $b)
    else
        bytestamp=$(wc -c $b | head -n -1) # remove the last "total" line
    fi
    tstampb=$(ls --full-time $b)
fi
if [[ -n $d ]]; then
    tstampd=$(ls --full-time $d)
fi
if [[ -n $D ]]; then
    tstampD=$(ls --full-time -R $D)
fi
if [[ -n $l ]]; then
    for listfile in $l; do
        if [[ ! -f $listfile ]]; then
            msg "file \`$listfile ' does not exist"
        else
            while read line; do
                if [[ ! -e "$line" ]]; then
                    msg "\`$listfile': file/path \`$line' does not exist"
                fi
            done < $listfile
            l_targets+=("$line")
        fi
    done
    tstampl=$(ls --full-time -R $l_targets)
fi

#----------------------#
# Begin execution loop #
#----------------------#
# This is like Russian Roulette (where "firing" is executing the command),
# except that all the chambers are loaded, and that on every new turn, instead
# of picking the chamber randomly, we look at the very next chamber. After
# every chamber is given a turn, we reload the gun and start over.
#
# If we detect file/directory modification, we pull the trigger. We can also
# pull the trigger by pressing SPACE or ENTER. If the -x option is provided,
# the last chamber will be set to "always shoot" and will always fire (if the
# trigger hasn't been pulled by the above methods yet).

if [[ $xflag == true && $xdelay_factor -le 1 ]]; then
    xdelay_factor=1
fi
echo "autocall: command set to \`$c4$com$ce'"
if [[ $wflag == true ]]; then
    echo "autocall: modification check interval set to $delay sec"
else
    echo "autocall: modification check interval set to $delay sec (default)"
fi
if [[ $xflag == true ]]; then
    echo "autocall: auto-execution interval set to ($delay * $xdelay_factor) = $(($delay*$xdelay_factor)) sec"
fi
if [[ $tflag == true ]]; then
    echo "autocall: TIMEOUT set to $timeout"
    if [[ $kflag == true ]]; then
        echo "autocall: KDELAY set to $killdelay"
    fi
fi
echo "autocall: press 'q' to quit"
echo "autocall: press ENTER or SPACE to execute manually"
key=""
while true; do
    for i in {1..$xdelay_factor}; do
        #------------------------------------------#
        # Case 1: the user forces manual execution #
        #------------------------------------------#
        # read a single key from the user
        read -s -t $delay -k key
        if [[ $key == "q" ]]; then
            echo "\nautocall: exiting..."
            exit 0
        # note the special notation $'\n' to detect an ENTER key
        elif [[ $key == $'\n' || $key == " " ]]; then
            autocall_exec $com $timeout $killdelay 4 "manual execution"
            key=""
            continue
        fi

        #------------------------------------------------------------------#
        # Case 2: modification is detected among watched files/directories #
        #------------------------------------------------------------------#
        if [[ -n $f ]]; then
            if [[ $#f -eq 1 ]]; then
                linestamp_new=$(wc -l $f)
            else
                linestamp_new=$(wc -l $f | head -n -1) # remove the last "total" line
            fi
            tstampf_new=$(ls --full-time $f)
        fi
        if [[ -n $F ]]; then
            tstampF_new=$(ls --full-time $F)
        fi
        if [[ -n $b ]]; then
            if [[ $#b -eq 1 ]]; then
                bytestamp_new=$(wc -c $b)
            else
                bytestamp_new=$(wc -c $b | head -n -1) # remove the last "total" line
            fi
            tstampb_new=$(ls --full-time $b)
        fi
        if [[ -n $d ]]; then
            tstampd_new=$(ls --full-time $d)
        fi
        if [[ -n $D ]]; then
            tstampD_new=$(ls --full-time -R $D)
        fi
        if [[ -n $l ]]; then
            tstampl_new=$(ls --full-time -R $l_targets)
        fi
        if [[ -n $f && "$linestamp" != "$linestamp_new" ]]; then
            autocall_exec $com $timeout $killdelay 1 "change detected" "$tstampf" "$tstampf_new"
            linestamp=$linestamp_new
            tstampf=$tstampf_new
            continue
        elif [[ -n $F && "$tstampF" != "$tstampF_new" ]]; then
            autocall_exec $com $timeout $killdelay 1 "change detected" "$tstampF" "$tstampF_new"
            tstampF=$tstampF_new
            continue
        elif [[ -n $b && "$bytestamp" != "$bytestamp_new" ]]; then
            autocall_exec $com $timeout $killdelay 1 "change detected" "$tstampb" "$tstampb_new"
            bytestamp=$bytestamp_new
            tstampb=$tstampb_new
            continue
        elif [[ -n $d && "$tstampd" != "$tstampd_new" ]]; then
            autocall_exec $com $timeout $killdelay 1 "change detected" "$tstampd" "$tstampd_new"
            tstampd=$tstampd_new
            continue
        elif [[ -n $D && "$tstampD" != "$tstampD_new" ]]; then
            autocall_exec $com $timeout $killdelay 1 "change detected" "$tstampD" "$tstampD_new"
            tstampD=$tstampD_new
            continue
        elif [[ -n $l && "$tstampl" != "$tstampl_new" ]]; then
            autocall_exec $com $timeout $killdelay 1 "change detected" "$tstampl" "$tstampl_new"
            tstampl=$tstampl_new
            continue
        fi

        #-----------------------------------------------------#
        # Case 3: periodic, automatic execution was requested #
        #-----------------------------------------------------#
        if [[ $xflag == true && $i -eq $xdelay_factor ]]; then
            autocall_exec $com $timeout $killdelay 3 "commencing auto-execution ($(($delay*$xdelay_factor)) sec)"
        fi
    done
done

# vim:syntax=zsh
