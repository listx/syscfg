#!/usr/bin/env zsh
# change backlight settings based on system
# symlink to /usr/bin/brightness, and call with "sudo brightness"
# make sure to disable password prompt for it with "sudo visudo"

case $HOST in
	k1)
		# since xbacklight always returns a single floating point number, we need to
		# convert it to an integer
		float=`xbacklight`
		int=${float/\.*}
		if [[ $int -eq 0 ]]; then
			xbacklight -set 100
		else
			xbacklight -set 0
		fi
	;;
	k2)
		b=$(cat /sys/class/backlight/dell_backlight/actual_brightness)
		let "bi=$b"
		if [[ $b -lt 7 ]]; then
			# gradually raise backlight, just like xbacklight
			while [[ bi -lt 7 ]]; do
				let "bi=$bi+1"
				echo $bi > /sys/class/backlight/dell_backlight/brightness
			done
		else
			while [[ bi -gt 0 ]]; do
				let "bi=$bi-1"
				echo $bi > /sys/class/backlight/dell_backlight/brightness
			done
		fi
		;;
	*)
		;;
esac

# vim:syntax=zsh
