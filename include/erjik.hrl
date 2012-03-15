%%%-------------------------------------------------------------------
%%% File        : erjik.hrl
%%% Author      : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Description : erjik definitions file
%%% Created     : 07 Mar 2009
%%%-------------------------------------------------------------------

-ifndef(_ERJIK).
-define(_ERJIK, true).

-define(CONF_LOGLEVEL, loglevel).
-define(CONF_LOGLEVEL_DEFAULT, 8).
-define(CONF_LOGFILE, logfile).
-define(CONF_LOGFILE_DEFAULT, "/dev/null").
-define(CONF_BLACKLISTS_DIR, blacklists_dir).
-define(CONF_PRIVILEGED, privileged).
-define(CONF_ALLOW, allow).
-define(CONF_DENY, deny).
-define(CONF_ORDER, order).

-define(CONF_IP_DENY_URL, ip_deny_url).

-define(PRIV_DIR, "priv").

-define(log(LogLevel, Format, Args), erjik_log:log(LogLevel, Format, Args)).
-define(logf(Format, Args), erjik_log:log(Format, Args)).
-define(error(Format, Args), erjik_log:error(Format, Args)).

-endif.
