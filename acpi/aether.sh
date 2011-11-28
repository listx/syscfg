#!/bin/sh
# Default acpi script that takes an entry for all actions

minspeed=`cat /sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_min_freq`
maxspeed=`cat /sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_max_freq`
setspeed="/sys/devices/system/cpu/cpu0/cpufreq/scaling_setspeed"

set $*

case "$1" in
    button/power)
        #echo "PowerButton pressed!">/dev/tty5
        case "$2" in
            PBTN|PWRF)  logger "PowerButton pressed: $2" ;;
            *)          logger "ACPI action undefined: $2" ;;
        esac
        ;;
    button/sleep)
        case "$2" in
            SLPB)   echo -n mem >/sys/power/state ;;
            *)      logger "ACPI action undefined: $2" ;;
        esac
        ;;
    ac_adapter)
        case "$2" in
            AC|ACAD|ADP0)
                case "$4" in
                    00000000)
                        echo "powersave" >/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor
                        echo "powersave" >/sys/devices/system/cpu/cpu1/cpufreq/scaling_governor
                        echo -n $minspeed >$setspeed
                        #/etc/laptop-mode/laptop-mode start
                    ;;
                    00000001)
                        echo "ondemand" >/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor
                        echo "ondemand" >/sys/devices/system/cpu/cpu1/cpufreq/scaling_governor
                        echo -n $maxspeed >$setspeed
                        #/etc/laptop-mode/laptop-mode stop
                    ;;
                esac
                ;;
            *)  logger "ACPI action undefined: $2" ;;
        esac
        ;;
    battery)
        case "$2" in
            BAT0)
                case "$4" in
                    00000000)   #echo "offline" >/dev/tty5
                    ;;
                    00000001)   #echo "online"  >/dev/tty5
                    ;;
                esac
                ;;
            CPU0)
                ;;
            *)  logger "ACPI action undefined: $2" ;;
        esac
        ;;
    button/lid)
       	case "$3" in
	    close)
		#echo "LID closed!">/dev/tty5
		;;
	    open)
		#echo "LID opened!">/dev/tty5
        	;;
	esac
	;;

    *)
        logger "ACPI group/action undefined: $1 / $2"
        ;;
esac
