# Erjik - multithreaded URL rewriter for Squid proxy.

## Goals

* multithreaded request processing;
* high tolerance for configuration and runtime errors;
* integration with HTTPD handling redirection pages;
* on-the-fly reconfiguration (without Squid restarting).

## License

erjik uses FreeBSD License. You can find full license text
on http://www.freebsd.org/copyright/freebsd-license.html or
in file LICENSE on the top of erjik sources tree.

## Requirements

* mime-support;
* sudo;
* erlang;
* erlang-inets.

Build requires:
* make;
* erlang-dev;
* erlang-edoc (optional).

## Building

Erlang must be installed to build and run erjik.
You always can obtain latest Erlang version on
http://www.erlang.org/download.html or use one provided by
your software repository.

erjik is developed and tested with Erlang R14A but probably
will work with older Erlang versions.

    $ make

## Installing

You need to build erjik and documentation to install it:

    $ make all doc

On Debian chdir to top of erjik sources tree and run:

    $ sudo make install

To remove erjik from system type:

    $ sudo make uninstall

## Configuring Squid

Add this lines to your squid.conf:

    url_rewrite_program /usr/sbin/erjik
    url_rewrite_concurrency 1
    url_rewrite_children 1

## Testing

Under the hood.

Requests must be separated by newlines ('\n' or '\n\r').
Each request must contain at least three tokens, separated by
spaces or TABs:

    RequestID URL SourceIP

RequestID - is identifier of request to distinguish multiple
requests in single pipe. Each erjik answer will be prefixed
with the same RequestID string. This is the way to use
multithreaded URL rewritings with one instance of URL
rewriter process started in Squid.

URL - is the URL requested by users browser.

SourceIP - is the user IP address.

Each answer will look like:

RequestID FinalURL

FinalURL - URL of destination page which will be provided to
user. When user is allowed to get source URL, FinalURL will be
the same as source URL otherwise FinalURL will point to so called
banpage.

Testing.

In order to test erjik you can manually start it from shell
using run.sh script and supply test requests on erjik`s stdin.
Answers will be send to erjik`s stdout. Do not forget to
supply RequestID token otherwise erjik will be unable to parse
incoming request.

## Advanced administration

After installation there will be a few erjik related tools in
/usr/sbin directory:

* erjik-hup - makes erjik to reload its configs and reopen
              log file (useful for log rotation);
* erjik-ping - tests if erjik alive or not.

* erjik-stop - stops erjik instance. This tool intended only
              for debug purposes, do not use it unless you
              know what are you doing;
* erjik-remsh - creates remote Erlang shell on Erlang node
              with erjik running. Only for debug purposes.

-----------------------------------------------------------------
Aleksey Morarash <aleksey.morarash@gmail.com>, 2012

