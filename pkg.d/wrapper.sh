#!/bin/sh

## Erlang emulator can't start without HOME environment variable.
##
## Another reason to set HOME is the Erlang Distribution. When
## started, it tries to create a '.erlang.cookie' file in the
## directory pointed by HOME environment variable. If it fail,
## whole startup process will fail. Sad but true. Thats why we
## cannot set HOME to '/'.

export HOME=/var/log/erjik
exec /usr/lib/erjik/erjik /etc/erjik.conf
