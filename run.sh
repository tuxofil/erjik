#!/bin/sh

exec erl \
    -sname "erjik" \
    -setcookie "erjik_secret_cookie" \
    -noshell \
    -pa ./ebin \
    -erjik_config "./erjik.conf" \
    -s erjik

