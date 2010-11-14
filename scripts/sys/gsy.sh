#!/bin/zsh
# gsy -- sync/update $PWD's git repository across all predefined systems

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
connections=(listdata@exelion listdata@luxion listdata@aether tiger@forest otter@ocean)

# these must be connected with a ".e" appended to it, like "luxion.e" (of
# course, this is because luxion.e is a predefined address in the /etc/hosts
# file)
laptops=(luxion aether)

# online clean remote connections
remotes_clean=()

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

# store repo location -- this has to be the same across all remotes as well;
# since we could be nested deep inside a directory somewhere in the repo, let's
# find the toplevel directory (this is required for pulling/pushing one-way)
repo=""
slash=""
while true; do
    # quit if we did not find a git directory
    [[ $PWD == "/" ]] && echo "gsy: no git repo found here or above" && exit
    ls .git >/dev/null 2>&1
    if [[ $? -eq 0 ]]; then
        # since we have different usenames in the remotes, we have to unify the
        # location of "repo" by using the common tilde "~" shell expansion
        # parameter; e.g., a repo named "/home/listdata/syscfg" can't be used
        # on "/home/tiger/syscfg", but using ~/syscfg ensures that it works
        # across all systems
        repo=${PWD/#${HOME}/\~}
        # this is used only during the ssh connection when syncing one-way
        [[ $repo[1] == "~" ]] && slash="/"
        break
    else
        cd ..
    fi
done

echo "${c4}gsy: starting initial upstream sync...$ce"

# store username and hostname (only for one-way sync)
username=$USER
machine_current=$HOST
[[ -n ${laptops[(r)$machine_current]} ]] && machine_current+=".e"

ghost_alive=false
# if we are already on the ghost machine, sync to ghost repo immediately
if [[ $HOST == $ghost ]]; then
    ghost_alive=true; # assume that this machine is connected to the net
    echo "\ngsy: already on ghost machine -- syncing ${c4}upstream$ce ($ghost <=> $c2$machine_current$ce)"
    # only pull and push if our working tree and staging area are both clean
    if [[ $(git diff 2>&1 | wc -l) -eq 0 && $(git diff --cached 2>&1 | wc -l) -eq 0 ]]; then
        git pull 2>&1 | sed -e "s/^/  $c1>$ce /" -e "s/error/${c6}error$ce/"
        git push 2>&1 | sed -e "s/^/  $c1>$ce /" -e "s/error/${c6}error$ce/"
    else
        echo "gsy: ${c6}error$ce: local repo unclean -- aborting upstream sync"
    fi
else
    # we are not on the ghost machine, so only sync with ghost if ghost is alive
    echo "\ngsy: attempting to sync upstream to ghost \`$ghost'..."
    ghost_alive=false
    ping -c 1 -W 1 $ghost >/dev/null 2>&1
    if [[ $? -eq 0 ]]; then
        ghost_alive=true;
        # since ghost is online, update the ghost before proceeding
        echo "gsy: ghost repo ${c1}online$ce -- syncing ${c4}upstream$ce ($ghost <=> $c2$machine_current$ce)"
        if [[ $(git diff 2>&1 | wc -l) -eq 0 && $(git diff --cached 2>&1 | wc -l) -eq 0 ]]; then
            git pull 2>&1 | sed -e "s/^/  $c1>$ce /" -e "s/error/${c6}error$ce/"
            git push 2>&1 | sed -e "s/^/  $c1>$ce /" -e "s/error/${c6}error$ce/"
        else
            echo "gsy: ${c6}error$ce: local repo unclean -- aborting sync"
        fi
    else
        echo "gsy: ghost \`$ghost' ${c6}offline$ce -- skipping sync"
    fi
fi

echo "\n${c4}gsy: starting remote sync...$ce"

n=1
for c in $remotes; do
    # check if remote is alive, and if so, connect to it and pull/push;
    # otherwise, tell user that this remote was offline
    r=$(echo -n $c | cut -d "@" -f2)
    echo "\ngsy: ($n/$#remotes) attempting to sync remote \`$r'..."
    ping -c 1 -W 1 $r >/dev/null 2>&1
    if [[ $? -eq 0 ]]; then
        echo "gsy: remote \`$r' ${c1}online$ce"
        if [[ $ghost_alive == true ]]; then
            echo "gsy: syncing ${c4}upstream$ce ($ghost <=> ${c3}$r$ce)"
            if ssh $c "[[ -d $repo ]]"; then
                if ssh $c "cd $repo && [[ \$(git diff 2>&1 | wc -l) -eq 0 && \$(git diff --cached 2>&1 | wc -l) -eq 0 ]] && echo"; then
                    # since remote is online and clean, we add it to our list of online remotes
                    remotes_clean+=($c)
                    ssh $c "
                    cd $repo;
                    git pull 2>&1 | sed -e \"s/^/  $c1>$ce /\" -e \"s/error/${c6}error$ce/\";
                    git push 2>&1 | sed -e \"s/^/  $c1>$ce /\" -e \"s/error/${c6}error$ce/\""
                else
                    echo "gsy: ${c6}error$ce: remote repo \`$r' unclean -- aborting sync"
                fi
            else
                echo "gsy: remote does not have this repo -- ${c5}skipping$ce"
            fi
        else
            # since ghost is offline, we try to pull from the local machine to
            # the remote (i.e., connect to the remote, and then pull from this
            # machine)
            echo "gsy: ghost repo ${c6}offline$ce -- syncing ${c5}one-way$ce ($c2$machine_current$ce => ${c3}$r$ce)"
            if ssh $c "[[ -d $repo ]]"; then
                if ssh $c "cd $repo && [[ \$(git diff 2>&1 | wc -l) -eq 0 && \$(git diff --cached 2>&1 | wc -l) -eq 0 ]]"; then
                    ssh $c "
                    cd $repo;
                    git pull ssh://$username@$machine_current$slash$repo master 2>&1 | sed -e \"s/^/  $c1>$ce /\" -e \"s/error/${c6}error$ce/\""
                else
                    echo "gsy: ${c6}error$ce: remote repo \`$r' unclean -- aborting sync"
                fi
            else
                echo "gsy: remote does not have this repo -- ${c5}skipping$ce"
            fi
        fi
    else
        echo "gsy: remote \`$r' ${c6}offline$ce -- ${c5}skipping$ce"
    fi
    let n+=1
done

echo "\n${c4}gsy: attempting upstream propagation...$ce"

# since we could have ended up merging upstream at any time we pushed from a
# remote to the ghost, let's pull across all remotes again to keep the remotes
# in line
if [[ $ghost_alive == true && (-n $remotes_clean || $(git diff 2>&1 | wc -l) -eq 0 && $(git diff --cached 2>&1 | wc -l) -eq 0) ]]; then
    echo "\n${c4}gsy: propagating upstream to all clean machines...$ce"
    if [[ $(git diff 2>&1 | wc -l) -eq 0 && $(git diff --cached 2>&1 | wc -l) -eq 0 ]]; then
        echo "\ngsy: propagating upstream ($ghost => $c2$machine_current$ce)"
        git pull 2>&1 | sed -e "s/^/  $c1>$ce /" -e "s/error/${c6}error$ce/"
    fi
    n=1
    for c in $remotes_clean; do
        r=$(echo -n $c | cut -d "@" -f2)
        echo "\ngsy: ($n/$#remotes_clean) propagating upstream ($ghost => ${c3}$r$ce)"
        if ssh $c "[[ -d $repo ]]"; then
            ssh $c "
            cd $repo;
            git pull 2>&1 | sed -e \"s/^/  $c1>$ce /\" -e \"s/error/${c6}error$ce/\""
        else
            echo "gsy: remote does not have this repo -- ${c5}skipping$ce"
        fi
        let n+=1
    done
else
    if [[ $ghost_alive == false ]]; then
        echo "\ngsy: ghost repo \`$ghost' ${c6}offline$ce -- aborting upstream propagation"
    else
        echo "\ngsy: no suitable clean remotes -- aborting upstream propagation"
    fi
fi

# vim:syntax=zsh
