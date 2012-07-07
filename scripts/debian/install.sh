#!/bin/sh

set -e
set -x

APPDIR=/usr/lib/erlang/lib/erjik-0.9.0

adduser --system --home /var/run/erjik --shell /bin/sh erjik || :
install --directory --mode=0755 \
    "$APPDIR"/ "$APPDIR"/ebin \
    /usr/share/erjik/www \
    /usr/share/doc/erjik/html \
    /var/lib/erjik \
    /var/lib/erjik/blacklists \
    /var/lib/erjik/regexps
install --directory --mode=0755 --owner=erjik \
    /var/log/erjik /var/run/erjik

install --mode=644 erjik.conf /etc/
sed --in-place --regexp-extended \
    's@ priv/@ /var/lib/erjik/@' \
    /etc/erjik.conf
sed --in-place --regexp-extended \
    's@^\s*#?www_root\s+.*$@www_root /usr/share/erjik/www@' \
    /etc/erjik.conf
install --mode=644 ebin/*.beam ebin/*.app "$APPDIR"/ebin/
install --mode=644 www/* /usr/share/erjik/www/
install --mode=644 priv/blacklists/* /var/lib/erjik/blacklists/
install --mode=644 priv/regexps/* /var/lib/erjik/regexps/

# documentation
install --mode=644 README LICENSE /usr/share/doc/erjik/
install --mode=644 doc/*.html doc/*.css doc/*.png /usr/share/doc/erjik/html/

# scripts
install --mode=440 scripts/debian/etc/sudoers.d/erjik /etc/sudoers.d/
install --mode=755 scripts/debian/usr/sbin/erjik /usr/sbin/erjik
install --mode=755 scripts/debian/usr/bin/erjik-hup /usr/bin/
install --mode=755 scripts/debian/usr/bin/erjik-ping /usr/bin/
install --mode=755 scripts/debian/usr/bin/erjik-remsh /usr/bin/
install --mode=755 scripts/debian/usr/bin/erjik-stop /usr/bin/
install --mode=755 scripts/debian/usr/share/erjik/*.sh /usr/share/erjik/

# generate Erlang cookie
echo $(cat /dev/urandom | tr -dc a-zA-Z0-9 | head -c 20) > \
    /var/run/erjik/.erlang.cookie
chown erjik:nogroup /var/run/erjik/.erlang.cookie
chmod 400 /var/run/erjik/.erlang.cookie

