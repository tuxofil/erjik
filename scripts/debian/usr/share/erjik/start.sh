#!/bin/sh

###-------------------------------------------------------------------
### File    : start.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 6 Jul 2012
### License : FreeBSD
### Description : erjik start script
###
###-------------------------------------------------------------------

exec erl -sname "erjik" \
    -noshell \
    -erjik_config "/etc/erjik.conf" \
    -erjik_log "/var/log/erjik/erjik.log" \
    -s erjik > /dev/null 2>&1

