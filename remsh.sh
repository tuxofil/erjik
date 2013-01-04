#!/bin/sh

exec erl \
    -sname "erjik-remsh-$RANDOM" \
    -setcookie erjik_secret_cookie \
    -pa ./ebin \
    -remsh "erjik@`hostname --short`"

