%%%-------------------------------------------------------------------
%%% File        : erjik.hrl
%%% Author      : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Description : erjik definitions file
%%% Created     : 07 Mar 2009
%%%-------------------------------------------------------------------

-ifndef(_ERJIK).
-define(_ERJIK, true).

%% various configuration tokens
-define(CFG_LOGLEVEL, loglevel).
-define(CFG_IP_DENY_REDIRECT, ip_deny_redirect).
-define(CFG_URL_DENY_REDIRECT, url_deny_redirect).
-define(CFG_ORDER, order).
-define(CFG_PRIVILEGED, privileged).
-define(CFG_ALLOW, allow).
-define(CFG_DENY, deny).
-define(CFG_IP_DEFAULT_POLICY, ip_default_policy).
-define(CFG_URL_DEFAULT_POLICY, url_default_policy).
-define(CFG_BIND_IP, bind_ip).
-define(CFG_BIND_PORT, bind_port).
-define(CFG_WWW_ROOT, www_root).
-define(CFG_MIME_TYPES, mime_types).

-define(CFGS_SIMPLE,
        [?CFG_LOGLEVEL, ?CFG_PRIVILEGED, ?CFG_ALLOW, ?CFG_DENY,
         ?CFG_ORDER, ?CFG_IP_DENY_REDIRECT,
         ?CFG_URL_DENY_REDIRECT, ?CFG_MIME_TYPES,
         ?CFG_IP_DEFAULT_POLICY, ?CFG_URL_DEFAULT_POLICY,
         ?CFG_BIND_IP, ?CFG_BIND_PORT, ?CFG_WWW_ROOT]).

-define(CFG_CLASS_DOMAINS, domains).
-define(CFG_CLASS_REGEXPS, regexps).
-define(CFG_CLASS_REDIRECT, redirect).

-define(CFGS_CLASS,
        [?CFG_CLASS_DOMAINS, ?CFG_CLASS_REGEXPS, ?CFG_CLASS_REDIRECT]).

-define(LOGLEVEL_NONE, none).
-define(LOGLEVEL_ERROR, error).
-define(LOGLEVEL_WARNING, warning).
-define(LOGLEVEL_INFO, info).
-define(LOGLEVEL_DEBUG, debug).

-define(LOGLEVELS,
        [?LOGLEVEL_NONE, ?LOGLEVEL_ERROR, ?LOGLEVEL_WARNING,
         ?LOGLEVEL_INFO, ?LOGLEVEL_DEBUG]).

%% logging
-define(logerr(Format, Args), erjik_log:log(1, Format, Args)).
-define(logwrn(Format, Args), erjik_log:log(2, Format, Args)).
-define(loginf(Format, Args), erjik_log:log(3, Format, Args)).
-define(logdbg(Format, Args), erjik_log:log(4, Format, Args)).

%% internal process signals
-define(SIG_RECONFIG, '#reconfig').
-define(SIG_STATE, '#state').
-define(SIG_FLUSH, '#flush').

%% ----------------------------------------------------------------------
%% eunit

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-endif.

