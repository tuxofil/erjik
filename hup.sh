#!/bin/sh

exec erl -sname "erjik_hupper" \
    -setcookie erjik_secret_cookie \
    -noinput \
    -pa ./ebin \
    -s erjik hup

