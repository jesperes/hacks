#!/bin/bash

if_verbose=:

for arg in $*; do
    case $arg in
	-v)
	    if_verbose=
	    ;;
	*)
	    echo "Ignoring unknown option $arg"
	    ;;
    esac
done

function print_header()
{
    wc=$1; shift
    url=$1; shift
    cols=$(tput cols)

    line=""
    for ((i=0; i < cols - 1; ++i)); do
	line="${line}="
    done

    echo "$(tput bold)${line}$(tput sgr0)"
    echo "$(tput setaf 6)$wc$(tput sgr0) checked out from $(tput setaf 2)$url$(tput sgr0)"
}

find -type d | while read f; do
    pushd . >/dev/null
    cd "$f"
    
    if [ -d .svn ]; then
	info="$(svn status -q)"
	if [ x"$info" != x ]; then
	    url="$(svn info | sed -ne 's/^URL: \(.*\)/\1/p')"
	    print_header $f $url
	    svn status -q
	    $if_verbose svn diff | colordiff
	fi
    elif [ -d .git ]; then
	info="$(git status -s)"
	if [ x"$info" != x ]; then
	    url="$(git remote get-url origin)"
	    print_header $f $url
	    git status -s
	    $if_verbose git diff
	fi
    fi
    
    popd >/dev/null
done
