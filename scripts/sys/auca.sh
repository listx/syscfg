#!/bin/zsh
# PROGRAM: auca
# AUTHOR: Linus Arver <linus@ucla.edu>
# LICENSE: PUBLIC DOMAIN
#
#
# DESCRIPTION:
#
# Auca (autocall) watches (1) a single file, (2) directory, and/or (3) a text file
# containing a list of files/directories, and if the watched files and/or
# directories become modified, runs the (first) given command string. Multiple
# commands can be provided (a total of 9 command strings are recognized) to
# manually execute different commands.
#
#
# USAGE:
#
# See msg("help") function below -- read that portion first!
#
#
# USER INTERACTION:
#
# Press "h" for help.
# Pressing a SPACE, ENTER, or "1" key forces execution of COMMAND immediately.
# Keys 2-9 are hotkeys to extra commands, if there are any.
# Press "c" for the command list.
# To exit auca gracefully, press "q".
#
#
# DEFAULT SETTINGS:
#
# (-w) DELAY    = 1
# (-x) FACTOR   = 10
#
#
# EXAMPLES:
#
# Execute "pdflatex -halt-on-error report.tex" every time "report.tex" or "ch1.tex" is
# modified (if line count changes in either file; modification checked every 1
# second by default):
#    auca -c "pdflatex -halt-on-error report.tex" -F report.tex -f ch1.tex
#
# Same, but only look at "ch1.tex" (useful, assuming that report.tex includes
# ch1.tex), and automatically execute every 4 seconds:
#    auca -c "pdflatex -halt-on-error report.tex" -F ch1.tex -x 4
#       (-x 0 or -x 1 here would also work)
#
# Same, but also automatically execute every 10 (1 * 10) seconds:
#    auca -c "pdflatex -halt-on-error report.tex" -F ch1.tex -x 10
#
# Same, but automatically execute every 5 (5 * 1) seconds (-w is 1 by default):
#    auca -c "pdflatex -halt-on-error report.tex" -F ch1.tex -x 5
#
# Same, but automatically execute every 17 (1 * 17) seconds:
#    auca -c "pdflatex -halt-on-error report.tex" -F ch1.tex -x 17
#
# Same, but for "ch1.tex", watch its byte size, not line count:
#    auca -c "pdflatex -halt-on-error report.tex" -b ch1.tex -x 17
#
# Same, but for "ch1.tex", watch its timestamp instead (i.e., every time
# this file is saved, the modification timestamp will be different):
#    auca -c "pdflatex -halt-on-error report.tex" -f ch1.tex -x 17
#
# Same, but also look at the contents of directory "images/ocean":
#    auca -c "pdflatex -halt-on-error report.tex" -f ch1.tex -d images/ocean -x 17
#
# Same, but also look at the contents of directory "other" recursively:
#    auca -c "pdflatex -halt-on-error report.tex" -f ch1.tex -d images/ocean -D other -x 17
#
# Same, but look at all files and/or directories (recursively) listed in file
# "watchlist" instead:
#    auca -c "pdflatex -halt-on-error report.tex" -l watchlist -x 17
#
# Same, but also look at "newfile.tex":
#    auca -c "pdflatex -halt-on-error report.tex" -l watchlist -f newfile.tex -x 17
#
# Same, but also allow manual execution of "make clean" with hotkey "2":
#    auca -c "pdflatex -halt-on-error report.tex" -c "make clean" -l watchlist -f newfile.tex -x 17
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
auca: Usage:

auca [OPTIONS]

Required parameter:
-c COMMAND      The command to be executed (put COMMAND in quotes). Note that
                COMMAND can be a set of multiple commands, e.g. \"make clean;
                make\". You can also specify multiple commands by invoking
                -c COMMAND multiple times -- the first 9 of these are set to
                hotkeys 1 through 9, if present. This is useful if you want to
                have a separate command that is available and can only be
                executed manually.

One or more required parameters (but see -x below):
-f FILE         File to be watched. Modification detected by time.
-F FILE         File to be watched. Modification detected by line-size.
-b FILE         File to be watched. Modification detected by bytes.
-d DIRECTORY    Directory to be watched. Modification detected by time.
-D DIRECTORY    Directory to be watched, recursively. Modification
                detected by time.
-l FILE         Text file containing a list of files/directories (each on
                its own line) to be watched (directories listed here are
                watched recursively). Modification is detected with 'ls'.

Optional parameters:
-w DELAY        Wait DELAY seconds before checking on the watched
                files/directories for modification; default 1.
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
                if only the -c and -x options are specified, auca will
                simply act as a while loop executing COMMAND every 10 (or
                more if FACTOR is greater than 10) seconds). Since the
                formula is (DELAY * FACTOR) seconds, if DELAY is 1 (default),
                FACTOR's face value itself, if greater than 0, is the seconds
                amount.
-a              Same as \`-x 10'
-h              Show this page and exit (regardless of other parameters).
-v              Show version number and exit (regardless of other parameters).
"
            exit 0
            ;;
        "version")
            echo "auca version 1.0"
            exit 0
            ;;
        *)
            echo "auca: $1"
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

auca_exec () {
    timeout=$2
    killdelay=$3
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
    echo "\nauca:$c2 [$(date --rfc-3339=ns)]$ce$col $5$ce"
    if [[ $# -eq 7 ]]; then
        diff -u0 -B -d <(echo "$6") <(echo "$7") | tail -n +4 | sed -e "/^[@-].\+/d" -e "s/\(\S\+\s\+\S\+\s\+\S\+\s\+\S\+\s\+\)\(\S\+\s\+\)\(\S\+\s\+\S\+\s\+\S\+\s\+\)/\1$c1\2$ce$c2\3$ce/" -e "s/^/  $c1>$ce /"
        echo
    fi
    echo "auca: calling command \`$c4$1$ce'..."
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
            eval timeout -k $killdelay $timeout $1 2>&1 | sed "s/^/  $col>$ce /"
            com_exit_status=$pipestatus[1]
        else
            eval timeout $timeout $1 2>&1 | sed "s/^/  $col>$ce /"
            com_exit_status=$pipestatus[1]
        fi
        if [[ $com_exit_status -eq 124 ]]; then
            echo "\n${c6}auca: command timed out$ce"
        elif [[ $com_exit_status -ne 0 ]]; then
            echo "\n${c6}auca: command exited with error status $com_exit_status$ce"
        else
            echo "\n${c1}auca: command executed successfully$ce"
        fi
    else
        eval $1 2>&1 | sed "s/^/  $col>$ce /"
        com_exit_status=$pipestatus[1]
        if [[ $com_exit_status -ne 0 ]]; then
            echo "\n${c6}auca: command exited with error status $com_exit_status$ce"
        else
            echo "\n${c1}auca: command executed successfully$ce"
        fi
    fi
}

#------------------#
# Global variables #
#------------------#

# colors
c1="\x1b[1;32m" # bright green
c2="\x1b[1;33m" # bright yellow
c3="\x1b[1;34m" # bright blue
c4="\x1b[1;36m" # bright cyan
c5="\x1b[1;35m" # bright purple
c6="\x1b[1;31m" # bright red
ce="\x1b[0m"

coms=()
delay=1
xdelay_factor=10
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
        com_binary=$(echo "$OPTARG" | sed 's/ \+/ /g' | sed 's/;/ /g' | cut -d " " -f1)
        if [[ $(which $com_binary) == "$com_binary not found" ]]; then
            msg "invalid command \`$com_binary'"
        else
            coms+=("$OPTARG")
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
if [[ -z $coms[1] ]]; then
    msg "help"
elif [[ (-n $f && -n $d && -n $D && -n $l) && $xflag == false ]]; then
    echo "auca: see help with -h"
    msg "at least one or more of the (1) -f, -d, -D, or -l paramters, or (2) the -x parameter, required"
fi

#-------------------------------#
# Record state of watched files #
#-------------------------------#

if [[ -n $F ]]; then
    if [[ $#F -eq 1 ]]; then
        linestamp=$(wc -l $F)
    else
        linestamp=$(wc -l $F | head -n -1) # remove the last "total" line
    fi
    tstampF=$(ls --full-time $F)
fi
if [[ -n $f ]]; then
    tstampf=$(ls --full-time $f)
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
            <$listfile while read line; do
                if [[ ! -e "$line" ]]; then
                    msg "\`$listfile': file/path \`$line' does not exist"
                else
                    l_targets+=("$line")
                fi
            done
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
com_num=1
for c in $coms; do
    echo "auca: command slot $com_num set to \`$c4$coms[$com_num]$ce'"
    let com_num+=1
done
echo "auca: press keys 1-$#coms to execute a specific command"
if [[ $wflag == true ]]; then
    echo "auca: modification check interval set to $delay sec"
else
    echo "auca: modification check interval set to $delay sec (default)"
fi
if [[ $xflag == true ]]; then
    echo "auca: auto-execution interval set to ($delay * $xdelay_factor) = $(($delay*$xdelay_factor)) sec"
fi
if [[ $tflag == true ]]; then
    echo "auca: TIMEOUT set to $timeout"
    if [[ $kflag == true ]]; then
        echo "auca: KDELAY set to $killdelay"
    fi
fi
echo "auca: press ENTER or SPACE to execute manually"
echo "auca: press \`c' for command list"
echo "auca: press \`h' for help"
echo "auca: press \`q' to quit"
key=""
while true; do
    for i in {1..$xdelay_factor}; do
        #------------------------------------------#
        # Case 1: the user forces manual execution #
        #------------------------------------------#
        # read a single key from the user
        read -s -t $delay -k key
        case $key in
            # note the special notation $'\n' to detect an ENTER key
            $'\n'|" "|1)
                auca_exec $coms[1] $timeout $killdelay 4 "manual execution"
                key=""
                continue
                ;;
            2|3|4|5|6|7|8|9)
                if [[ -n $coms[$key] ]]; then
                    auca_exec $coms[$key] $timeout $killdelay 4 "manual execution"
                    key=""
                    continue
                else
                    echo "auca: command slot $key is not set"
                    key=""
                    continue
                fi
                ;;
            c)
                com_num=1
                echo ""
                for c in $coms; do
                    echo "auca: command slot $com_num set to \`$c4$coms[$com_num]$ce'"
                    let com_num+=1
                done
                key=""
                continue
                ;;
            h)
                echo "\nauca: press \`c' for command list"
                echo "auca: press \`h' for help"
                echo "auca: press \`q' to exit"
                com_num=1
                for c in $coms; do
                    echo "auca: command slot $com_num set to \`$c4$coms[$com_num]$ce'"
                    let com_num+=1
                done
                echo "auca: press keys 1-$#coms to execute a specific command"
                echo "auca: press ENTER or SPACE or \`1' to execute first command manually"
                key=""
                continue
                ;;
            q)
                echo "\nauca: exiting..."
                exit 0
                ;;
            *) ;;
        esac

        #------------------------------------------------------------------#
        # Case 2: modification is detected among watched files/directories #
        #------------------------------------------------------------------#
        if [[ -n $f ]]; then
            tstampf_new=$(ls --full-time $f)
        fi
        if [[ -n $F ]]; then
            if [[ $#F -eq 1 ]]; then
                linestamp_new=$(wc -l $F)
            else
                linestamp_new=$(wc -l $F | head -n -1) # remove the last "total" line
            fi
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
        if [[ -n $f && "$tstampf" != "$tstampf_new" ]]; then
            auca_exec $coms[1] $timeout $killdelay 1 "change detected" "$tstampf" "$tstampf_new"
            tstampf=$tstampf_new
            continue
        elif [[ -n $F && "$linestamp" != "$linestamp_new" ]]; then
            auca_exec $coms[1] $timeout $killdelay 1 "change detected" "$tstampF" "$tstampF_new"
            linestamp=$linestamp_new
            tstampF=$tstampF_new
            continue
        elif [[ -n $b && "$bytestamp" != "$bytestamp_new" ]]; then
            auca_exec $coms[1] $timeout $killdelay 1 "change detected" "$tstampb" "$tstampb_new"
            bytestamp=$bytestamp_new
            tstampb=$tstampb_new
            continue
        elif [[ -n $d && "$tstampd" != "$tstampd_new" ]]; then
            auca_exec $coms[1] $timeout $killdelay 1 "change detected" "$tstampd" "$tstampd_new"
            tstampd=$tstampd_new
            continue
        elif [[ -n $D && "$tstampD" != "$tstampD_new" ]]; then
            auca_exec $coms[1] $timeout $killdelay 1 "change detected" "$tstampD" "$tstampD_new"
            tstampD=$tstampD_new
            continue
        elif [[ -n $l && "$tstampl" != "$tstampl_new" ]]; then
            auca_exec $coms[1] $timeout $killdelay 1 "change detected" "$tstampl" "$tstampl_new"
            tstampl=$tstampl_new
            continue
        fi

        #-----------------------------------------------------#
        # Case 3: periodic, automatic execution was requested #
        #-----------------------------------------------------#
        if [[ $xflag == true && $i -eq $xdelay_factor ]]; then
            auca_exec $coms[1] $timeout $killdelay 3 "commencing auto-execution ($(($delay*$xdelay_factor)) sec)"
        fi
    done
done

# vim:syntax=zsh
