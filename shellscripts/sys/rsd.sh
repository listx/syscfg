#!/bin/zsh
# rsd -- "remote system diff": diff either HOME directory or pacman (installed packages) either with master central
# "role model" system, or if that system is offline, with another system
#
# terminology: "ghost" means the central reference machine, and "clone" means all subsidiary machines

# by default, only check the HOME directory structure/file layout (with simple "ls"); otherwise, if there is
# an argument, only check installed pacman packages with "pacman -Q"

# Colors (comment out to disable)
c1="\x1b[1;32m" # bright green
c2="\x1b[1;33m" # bright yellow
c3="\x1b[1;34m" # bright blue
c4="\x1b[1;36m" # bright cyan
c5="\x1b[1;35m" # bright purple
c6="\x1b[1;31m" # bright red
ce="\x1b[0m"

# name of the central bare repo (we only pull/push to this machine if it's
# online; if it's offline, we sync only among the remotes themselves)
ghost=(exelion)

# list of machines that have the same git repo (that are ssh-able w/o a
# password)
connections=(listdata@exelion listdata@luxion listdata@aether)

# these must be connected with a ".e" appended to it, like "luxion.e" (of
# course, this is because luxion.e is a predefined address in the /etc/hosts
# file)
laptops=(luxion aether)

# identify remote connections
remotes=()
for c in $connections; do
    machine=$(echo -n $c | cut -d "@" -f2)
    if [[ $machine != $HOST ]]; then
        # if the remote is recognized by laptops() array, connect with an
        # appended ".e"
        [[ -n ${laptops[(r)$machine]} ]] && c+=".e"
        remotes+=($c)
    fi
done

# files/dirs to ignore when LS-ing (relative to $HOME)
dirs=(
    $HOME/download
    $HOME
)

snap=""
rsnap=""

differ() {
    case $1 in
        dir)
            msg1="${c4}rsd: folder \`$d': starting upstream diff...$ce"
            msg2="rsd: ${c5}no extra files detected$ce"
            d="$2"
            ;;
        *)
            msg1="${c4}rsd: pacman package list: starting upstream diff...$ce"
            msg2="rsd: ${c5}no package differences detected$ce"
            ;;
    esac

    echo $msg1

    # store username and hostname
    username=$USER
    machine_current=$HOST
    [[ -n ${laptops[(r)$machine_current]} ]] && machine_current+=".e"

    ghost_alive=false

    # load corect snapshot (try to make it so that snapshot is ghost machine's stuff)
    if [[ $HOST == $ghost ]]; then
        ghost_alive=true; # assume that this machine is connected to the net
        echo "\nrsd: already on ghost machine -- looking at clone systems"
        [[ $1 == "dir" ]] && snap=$(ls $d) || snap=$(pacman -Qq)
    else
        # we are not on the ghost machine, so try to get snapshot from ghost (if it's alive)
        echo "\nrsd: attempting to get snapshot from ghost \`$ghost'..."
        ghost_alive=false
        ping -c 1 -W 1 $ghost >/dev/null 2>&1
        if [[ $? -eq 0 ]]; then
            ghost_alive=true;
            # since ghost is online, get the snapshot from ghost
            echo "rsd: ghost repo ${c1}online$ce -- getting snapshot from ghost \`$ghost'"
            [[ $1 == "dir" ]] && snap=$(ssh $ghost "ls $d") || snap=$(ssh $ghost "pacman -Qq")
        else
            echo "rsd: ghost \`$ghost' ${c6}offline$ce -- setting snapshot to current machine \`$machine_current'"
            [[ $1 == "dir" ]] && snap=$(ls $d) || snap=$(pacman -Qq)
        fi
    fi

    echo "\n${c4}rsd: starting remote system diff...$ce"

    n=1
    for c in $remotes; do
        # check if remote is alive, and if so, connect to it and get that system's snapshot
        # otherwise, tell user that this remote was offline
        r=$(echo -n $c | cut -d "@" -f2)
        echo "\nrsd: ($n/$#remotes) attempting to diff remote \`$r'..."
        ping -c 1 -W 1 $r >/dev/null 2>&1
        if [[ $? -eq 0 ]]; then
            echo "rsd: remote \`$r' ${c1}online$ce"
            if [[ $ghost_alive == true ]]; then
                echo "rsd: diffing against ${c4}upstream$ce ($ghost <=> ${c3}$r$ce)"
            else
                echo "rsd: ghost repo ${c6}offline$ce -- diffing amongst clones ${c5}one-way$ce ($c2$machine_current$ce => ${c3}$r$ce)"
            fi
            [[ $1 == "dir" ]] && rsnap=$(ssh $c "ls $d") || rsnap=$(ssh $c "pacman -Qq")
            if [[ $1 == "dir" ]]; then
                # only look for _additional_ files when comparing directories
                diff=$(diff -u0 -B <(echo "$snap") <(echo "$rsnap") | grep -e "^+." | grep -v "^+++")
                [[ -n $diff ]] && echo $diff | sed "s/^+/  $c1>$ce /" || echo $msg2
            else
                # when comparing packages, look at both nonexistant (on ghost, but not in clone) and surplus (not on
                # ghost, but in clone) packages (and also use more colors)
                diff=$(diff -u0 -B <(echo "$snap") <(echo "$rsnap") | grep -e "^[-+]." | grep -v "^+++" | grep -v "^---")
                [[ -n $diff ]] && echo $diff | sed -e "s/^-\(.\+\)/$c6\1$ce/" -e "s/^+\(.\+\)/$c3\1$ce/" | sed "s/^/  $c1>$ce /" || echo $msg2
            fi
        else
            echo "rsd: remote \`$r' ${c6}offline$ce -- ${c5}skipping$ce"
        fi
        let n+=1
    done
}

# diff the dirs first, if no arguments
if [[ $#@ -eq 0 ]]; then
    for d in $dirs; do
        differ dir $d
    done
elif [[ $1 == "p" ]]; then
    differ pac
else
    echo "rsd: usage: \`rsd' or \`rsd p'"
fi


# vim:syntax=zsh
