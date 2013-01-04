#!/bin/sh

exec erl -sname "erjik_pinger" \
    -setcookie erjik_secret_cookie \
    -noinput \
    -pa ./ebin \
    -s erjik ping

