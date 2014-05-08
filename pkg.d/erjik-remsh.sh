#!/bin/sh

CONFIG="$1"

if [ -z "$CONFIG" ]; then
    echo "Usage: $0 <config_filename>" >&2
    exit 1
fi

if [ ! -r "$CONFIG" ]; then
    echo "error: Erjik config '$CONFIG' is not readable" >&2
    exit 1
fi

# Fetch requisites from the Erjik configurations
INSTANCE_ID=`grep -E '^instance_id\s+' "$CONFIG" | \
    tail --lines=1 | sed -r 's/^instance_id\s+//' | sed -r 's/\s+$//'`
COOKIE=`grep -E '^cookie\s+' "$CONFIG" | \
    tail --lines=1 | sed -r 's/^cookie\s+//' | sed -r 's/\s+$//'`

# Default values
[ -z "$INSTANCE_ID" ] && INSTANCE_ID="erjik"
[ -z "$COOKIE" ] && COOKIE="erjik"

# FIXME: do not publish the cookie in the command line
RND_INT=`date +%N`
exec erl -hidden -name "erjik_remsh_$RND_INT@127.0.0.1" \
    -setcookie "$COOKIE" -remsh "$INSTANCE_ID@127.0.0.1"
