#!/bin/bash

set -e
MPLAYER=e:/mplayer-1.0rc2/mplayer.exe
MENCODER=e:/mplayer-1.0rc2/mencoder.exe

function runcmd() {
    echo >&2
    echo "-----------------------------------------------" >&2
    echo "RUNNING: $@" >&2
    echo ">>>" >&2
    "$@"
    echo "<<<" >&2
    echo "-----------------------------------------------">&2
    echo >&2
}

if [ -z "$PROFILE" ]; then
    read -p "Profile: " PROFILE 
else
    echo "Profile: $PROFILE"
fi

INPUTFILE="$1"; shift
OUTPUTFILE="$1"; shift

if [ -f "$OUTPUTFILE" ]; then
    echo "Output file already exists."
    exit 1
fi

for f in "$@"; do
    if [ "$f" = "-dvd-device" ]; then
	mark=1
    fi

    if [ "$mark" = 1 ]; then
	DVDDEVICE=("-dvd-device" "$f")
    fi
done

MENC_OPTS="-priority idle -ffourcc XVID -idx"
SCALE=scale
PP=pp=default
ASPECT=autoaspect
PASSES=2

case $PROFILE in
    # For doing DVD rips
    dvd)			
	BITRATE=1600
	ABITRATE=
	CROP=
	;;
    
    *)
	echo "No profile set."
	exit 1
	;;
esac

function crop_detect() {
    INPUTFILE="$1"; shift
    TMP=cropdetect$$.log
    rm -f $TMP
    
    echo "Checking crop detection command..." >%2
    
    $MPLAYER "$INPUTFILE" -vf cropdetect,scale -vo directx -ao null \
	-sstep 120 -frames 20 "$@" >$TMP 2>&1
    
    CROP=$(grep "CROP" $TMP | tail -n 1 | sed -e 's/.*-vf \(crop.*\))\./\1/')
    if [ x$CROP = x ]; then
	echo "*** Crop detection failed!" >&2
	exit 1
    fi

    echo $CROP
    
    rm -f $TMP
}

if [ x$CROP = x ]; then
    echo
    echo "Checking cropping... "
    echo
    CROP=$(crop_detect "$INPUTFILE" "${DVDDEVICE[@]}")
    echo "CROP>> $CROP <<"
fi

if [ x$BITRATE = x ]; then
    OVC="-ovc copy"
    BITRATE=800
else
    OVC="-ovc lavc"
fi

if [ x$ABITRATE = x ] ; then
    OAC="-oac copy"
    LAVCOPTS="-lavcopts vcodec=mpeg4:vbitrate=$BITRATE:$ASPECT"
else
    OAC="-oac lavc"
    LAVCOPTS="-lavcopts vcodec=mpeg4:vbitrate=$BITRATE:acodec=mp3:abitrate=$ABITRATE:$ASPECT"
fi

export TIMEFORMAT="Elapsed time: %lR"


if [ $PASSES = 1 ]; then
    LAVCOPTS="$LAVCOPTS:v4mv:trell:mbd=2"

    echo "Encoding: $INPUTFILE"
    runcmd $MENCODER $MENC_OPTS "$INPUTFILE" -o "$OUTPUTFILE" \
	$OAC $OVC $LAVCOPTS -vf-pre $CROP,$PP,$SCALE,harddup "$@" 2>&1

else
    LAVCOPTS1="$LAVCOPTS:turbo:vpass=1"
    LAVCOPTS2="$LAVCOPTS:mbd=2:trell:v4mv:vpass=2"
    # LAVCOPTS2="$LAVCOPTS:mbd=2:trell:v4mv:last_pred=2:dia=-1:vmax_b_frames=2:vb_strategy=1:cmp=3:subcmp=3:precmp=0:vqcomp=0.6:turbo:vpass=2"

    echo "Encoding (pass 1): $INPUTFILE"
    runcmd $MENCODER $MENC_OPTS "$INPUTFILE" -o "$OUTPUTFILE" \
	$OAC $OVC $LAVCOPTS1 -vf-pre $CROP,$PP,$SCALE,harddup "$@" 2>&1
    
    echo "Encoding (pass 2): $INPUTFILE"
    runcmd $MENCODER $MENC_OPTS "$INPUTFILE" -o "$OUTPUTFILE" \
	$OAC $OVC $LAVCOPTS2 -vf-pre $CROP,$PP,$SCALE,harddup "$@" 2>&1

fi

rm -f divx2pass.log
