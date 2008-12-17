#!/bin/bash

set -e

rm -fr vpnclient
TARBALL=vpnclient-linux-x86_64-4.8.01.0640-k9.tar.gz
[ -f $TARBALL ] || wget http://projects.tuxx-home.at/ciscovpn/clients/linux/4.8.01/$TARBALL

PATCHES=vpnclient-linux-2.6.24.diff

for p in $PATCHES; do
    [ -f $p ] || wget http://projects.tuxx-home.at/ciscovpn/patches/$p
done

tar zxf $TARBALL

(cd vpnclient; 
    for p in $PATCHES; do
	patch < ../$p
    done;
    yes "" | sudo apt-get install linux-headers-`uname -r`
    yes "" | sudo ./vpn_install
    )
