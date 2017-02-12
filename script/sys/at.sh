#!/usr/bin/env zsh

setopt ERR_EXIT
setopt NO_UNSET
setopt PIPE_FAIL

duration_pretty()
{
	local t=$1

	local d=$((t/60/60/24))
	local h=$((t/60/60%24))
	local m=$((t/60%60))
	local s=$((t%60))

	if ((d)); then
		((d == 1)) && echo -n "$d day " || echo -n "$d days "
	fi
	if ((h)); then
		((h == 1)) && echo -n "$h hour " || echo -n "$h hours "
	fi
	if ((m)); then
		((m == 1)) && echo -n "$m minute " || echo -n "$m minutes "
	fi
	if ((s)); then
		((s == 1)) && echo -n "$s second " || echo -n "$s seconds "
	fi
}

# Run a command at a specified date. Works by simply converting the given
# timestamp to UNIX Epoch (seconds), then taking the diff of the current time in
# UNIX Epoch, and then sleeping X seconds (the difference between the two
# timestamps). Abort with an error if the given timestamp is not in the future.

# Colors.
cRed="\x1b[1;31m"
cGrn="\x1b[1;32m"
cYlo="\x1b[1;33m"
cBlu="\x1b[1;34m"
cPrp="\x1b[1;35m"
cCyn="\x1b[1;36m"
cx="\x1b[0m"

# Target time must be in HH:MM or HH:MM:SS format (where HH is 24-hour clock).
# Actually, it is even more flexible than this --- see
#
#   info '(coreutils) date invocation'
#
# in the section "Date input formats" for details. For simplicity we hardcode
# the desired timezone.
t_target=$1
shift

t1=$(date +%s)
t2=$(date --date="TZ=\"America/Los_Angeles\" ${t_target}" +%s)
t2_pretty=$(date --date="TZ=\"America/Los_Angeles\" ${t_target}" --iso-8601=seconds)

if (( $t2 < $t1 )); then
	echo "The desired time is in the past."
	echo "Aborting."
	exit 1
fi

com_pretty="$cPrp$@$cx"
echo -e "Will execute\n  $com_pretty\nat $t2_pretty.\n"

while true; do
	# Account for the possibility that sleeping in small increments can
	# introduce lag; recalculate `t1' and `sleep_amount' on every iteration.
	t1=$(date +%s)
	t1_pretty=$(date --iso-8601=seconds)
	sleep_amount=$(( $t2 - $t1 ))
	sleep_amount_pretty=$(duration_pretty $sleep_amount)

	if (( $sleep_amount <= 0 )); then
		echo -e "\033[2K\rDone waiting.\n"
		break
	fi

	echo -en "\033[2K\rCurrent time $t1_pretty; ${sleep_amount_pretty}to go."
	sleep 1
done

echo "Executing command: $cPrp$@$cx"
echo "-----------------"
eval "$@"
