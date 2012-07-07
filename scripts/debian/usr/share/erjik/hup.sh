#!/bin/sh

###-------------------------------------------------------------------
### File    : hup.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 6 Jul 2012
### License : FreeBSD
### Description : make erjik to reload configurations and reopen
###               log file without restart.
###-------------------------------------------------------------------

RANDOM=`date +%N`
exec erl -sname "erjik_hupper$RANDOM" \
    -noshell -noinput \
    -s erjik hup

