#!/bin/sh

exec erl -sname "erjik_hupper" \
    -setcookie erjik_secret_cookie \
    -noshell -noinput \
    -pa ./ebin \
    -s erjik hup

