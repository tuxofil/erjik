#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 <config_filename>" >&2
    exit 1
fi

exec sudo -n -u erjik /usr/lib/erjik/erjik-wrapper --ping "$1"
