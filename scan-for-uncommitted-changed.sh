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

function print_progress()
{
    cols=$(tput cols)
    progress=$((100*counter/numdirs))
    str=$(echo "Scanning (${progress}%): ${f}" | cut -b-$cols)
    tput sc # save cursor
    echo -n $str
    tput el # clear to EOL
    tput rc # restore cursor
}

function reset()
{
    echo
    tput sgr0
    tput cnorm
}

trap reset INT

tput civis

find_args="-path *.svn -o -path *.git"
dirs=$(find -type d $find_args 2>/dev/null)

numdirs=$(echo "$dirs" | wc -l)
counter=0

echo "Scanning $numdirs directories..."
echo "$dirs" | while read f; do
    # echo $f
    pushd . >/dev/null
    counter=$((counter + 1))

    d="$(dirname "$f")"
    cd $d
    if [ -d .svn ]; then
	print_progress $d
	info="$(svn status -q)"
	if [ x"$info" != x ]; then
	    url="$(svn info | sed -ne 's/^URL: \(.*\)/\1/p')"
	    print_header $d $url
	    svn status -q
	    $if_verbose svn diff | colordiff
	fi
    elif [ -d .git ]; then
	print_progress $d
	info="$(git status -s)"
	if [ x"$info" != x ]; then
	    url="$(git remote get-url origin)"
	    print_header $d $url
	    git status -s
	    $if_verbose git diff
	fi
    fi
    
    popd >/dev/null
done

tput cnorm

