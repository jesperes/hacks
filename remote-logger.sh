#!/bin/sh
# View logs remotely via ssh

host=$1
logs="messages syslog"
cmd="tail -f "
for l in $logs; do
    cmd="$cmd /var/log/$l"
done
while true; do
    echo "Running $cmd"
    echo "On host $host"
    ssh $host $cmd
    echo "Remote session terminated."
    sleep 5
done
