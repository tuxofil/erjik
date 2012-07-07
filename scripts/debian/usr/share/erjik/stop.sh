#!/bin/sh

###-------------------------------------------------------------------
### File    : stop.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 6 Jul 2012
### License : FreeBSD
### Description : stops erjik.
###
###-------------------------------------------------------------------

RANDOM=`date +%N`
exec erl -sname "erjik_stopper$RANDOM" \
    -noshell -noinput \
    -s erjik stop_remote > /dev/null 2>&1

