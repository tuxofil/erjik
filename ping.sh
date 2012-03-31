#!/bin/sh

exec erl -sname "erjik_pinger" \
    -setcookie erjik_secret_cookie \
    -noshell -noinput \
    -pa ./ebin \
    -s erjik ping

