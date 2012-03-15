#!/bin/sh

erl \
    -noshell \
    -s erjik \
    -s erjik redirector \
    -s init stop

