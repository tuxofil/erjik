# Erjik - multithreaded URL rewriter for Squid proxy.

## Goals

* multithreaded request processing;
* high tolerance for configuration and runtime errors;
* integration with HTTPD handling redirection pages;
* on-the-fly reconfiguration (without Squid restarting).

## License

Erjik uses FreeBSD License. You can find full license text
on http://www.freebsd.org/copyright/freebsd-license.html or
in file LICENSE on the top of the Erjik source tree.

## Requirements

* mime-support;
* erlang;
* erlang-inets.

Build requires:
* make;
* erlang-dev;
* erlang-edoc (optional).

## Building

Erlang must be installed to build and run Erjik.
You always can obtain latest Erlang version on
http://www.erlang.org/download.html or use the one provided by
your software repository.

Erjik is developed and tested with Erlang R14A but probably
will work with older Erlang versions.

    $ make

## Installing

    $ make
    $ sudo make install

### Staged installation

Use DESTDIR environment variable:

    $ DESTDIR=rootfs make install
    $ tree rootfs
    rootfs/
    ├── etc
    │   └── erjik.conf
    ├── usr
    │   └── lib
    │       └── erjik
    │           ├── erjik
    │           └── erjik-wrapper
    └── var
        ├── lib
        │   └── erjik
        │       ├── domains
        │       │   ├── good
        │       │   └── porn
        │       ├── regexps
        │       │   ├── attraction
        │       │   ├── audio-video
        │       │   ├── fileserver
        │       │   ├── good
        │       │   ├── onlinegames
        │       │   ├── porn
        │       │   └── security
        │       └── www
        │           ├── denied-by-ip.html
        │           ├── denied-by-url.html
        │           └── porn.html
        └── log

### Uninstallation

To completely remove the Erjik from the system type:

    $ sudo make uninstall

## Configuring Squid

Add this lines to your squid.conf:

    url_rewrite_program /usr/lib/erjik/erjik-wrapper
    url_rewrite_concurrency 1
    url_rewrite_children 1

## Configuring Erjik

See comments in the /etc/erjik.conf file for description of
available configuration parameters.

## Maintainance

Normally Erjik is started by Squid so you don't need to
launch it manually.

Apply new configuration, reopen log file:

    $ /usr/lib/erjik/erjik --hup /etc/erjik.conf

Check if Erjik is alive or not:

    $ /usr/lib/erjik/erjik --ping /etc/erjik.conf

## Testing

### Under the hood

Requests must be separated by newlines ('\n' or '\n\r').
Each request must contain at least three tokens, separated by
spaces or TABs:

    RequestID URL SourceIP

RequestID - is identifier of request to distinguish multiple
requests in single pipe. An answer will be prefixed with the
same RequestID string. This is the way to use multithreaded URL
rewritings with one instance of URL rewriter process started in Squid.

URL - is the URL requested by the user's browser.

SourceIP - is the user's IP address.

The answer will look like:

    RequestID FinalURL

FinalURL is an URL of a destination page which will be provided to the
user. When the user is allowed to get the source URL, the FinalURL will be
the same as the source URL otherwise the FinalURL will point to so called
banpage.

### Testing

In order to test Erjik you can manually start it from the shell
and supply test requests on Erjik's stdin. The answers will be send
to the stdout:

    $ echo '1 http://www.github.com/ 10.0.0.1' | ./erjik ./erjik.conf
    1 http://www.github.com/
    $ echo '1 http://www.porn.com/ 10.0.0.1' | ./erjik ./erjik.conf
    1 http://127.0.0.1:8888/porn.html

-----------------------------------------------------------------
Aleksey Morarash <aleksey.morarash@gmail.com>, 2012
