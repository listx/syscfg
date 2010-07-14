#!/bin/zsh
# gsy -- sync/update $PWD's git repository across all predefined systems

# name of the central bare repo (we only pull/push to this if this machine is online; if this machine is offline, we simply try to pull manually from all the remotes)
ghost=(exelion)

# list of machines that have the same git repo (that are ssh-able w/o a password)
machines=(exelion luxion aether)

# these must be connected with a ".e" appended to it, like "luxion.e" (of course, this is because luxion.e is a predefined address in the /etc/hosts file)
laptops=(luxion aether)

# identify remotes
remotes=()
for m in $machines; do
    if [[ $m != $HOST ]]; then
        remote=$m
        # if the remote is recognized by laptops() array, connect with an appended ".e"
        if [[ -n ${laptops[(r)$remote]} ]]; then remote+=".e"; fi
        remotes+=($remote)
    fi
done

# store repo location -- this has to be the same across all remotes as well
repo=$PWD

# store username (only used if ghost if offline)
username=$USER

# store machine's LAN address name
machine_current=$HOST
if [[ -n ${laptops[(r)$machine_current]} ]]; then machine_current+=".e"; fi

# first check if ghost is alive
ghost_alive=false
ping -c 1 $ghost 2>&- 1>&-
if [[ $? -eq 0 ]]; then
    ghost_alive=true;
    # since ghost is online, update the ghost before proceeding
    git pull
    git push
fi

for r in $remotes; do
    # check if remote is alive, and if so, connect to it and pull/push; otherwise, tell user that this remote was offline
    ping -c 1 $r 2>&- 1>&-
    if [[ $? -eq 0 ]]; then
        echo "gsy: attempting to sync $r..."
        if [[ $ghost_alive == true ]]; then
            echo "gsy: ghost repo online -- syncing with ghost"
            ssh $r "cd $repo; git pull; git push; exit"
        else
            # since ghost is offline, we try to pull from the remote (i.e., connect to the remote, and then pull from this machine)
            echo "gsy: ghost repo offline -- syncing bidirectionally ($machine_current <=> $r)"
            ssh $r "cd $repo; git pull ssh://$username@$machine_current$repo master; exit"
        fi
    else
        echo "gsy: remote \`$r' offline -- skipping this remote"
    fi
done

# vim:syntax=zsh
