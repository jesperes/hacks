#!/bin/bash

if [ `whoami` != "root" ]; then
    echo "Executing script as root."
    exec sudo "$0" "$@"
fi

# Remove cisco_ipsec kernel module
if lsmod | grep -q cisco_ipsec; then
    rmmod -v cisco_ipsec
fi

function show_online_cores() {
    cores=`grep processor /proc/cpuinfo | wc -l`
    echo "Online cores ($cores):"
    grep "model name" /proc/cpuinfo
}

function resume_cores() {
    for f in /sys/devices/system/cpu/cpu*/online; do
	echo "Resuming $(basename $(dirname $f))."
	echo 1 > $f
    done
    show_online_cores
}

function disable_cores() {
    for f in /sys/devices/system/cpu/cpu*/online; do
	echo "Taking $(basename $(dirname $f)) offline."
	sudo echo 0 > $f
    done
    show_online_cores
}

disable_cores
trap resume_cores EXIT

/etc/init.d/vpnclient_init start
/usr/local/bin/vpnclient connect iar
