#!/bin/sh

###-------------------------------------------------------------------
### File    : ping.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 6 Jul 2012
### License : FreeBSD
### Description : erjik pinger tool
###
###-------------------------------------------------------------------

RANDOM=`date +%N`
exec erl -sname "erjik_pinger$RANDOM" \
    -noshell -noinput \
    -s erjik ping > /dev/null 2>&1

