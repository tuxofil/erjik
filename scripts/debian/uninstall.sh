#!/bin/sh

set -e
set -x

deluser erjik || :

rm -rvf -- \
    /etc/erjik.conf \
    /etc/sudoers.d/erjik \
    /usr/bin/erjik* \
    /usr/sbin/erjik \
    /usr/lib/erlang/lib/erjik* \
    /usr/share/erjik \
    /usr/share/doc/erjik \
    /var/lib/erjik \
    /var/log/erjik \
    /var/run/erjik

