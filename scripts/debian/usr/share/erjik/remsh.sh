#!/bin/sh

###-------------------------------------------------------------------
### File    : remsh.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 6 Jul 2012
### License : FreeBSD
### Description : creates remote shell to Erlang node with erjik
###               (only for debug purposes)
###-------------------------------------------------------------------

RANDOM=`date +%N`
HOSTNAME=`hostname --short`
exec erl -sname "erjik_remsh$RANDOM" \
    -remsh "erjik@$HOSTNAME"

