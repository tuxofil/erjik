%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc erjik application resource file

{application, erjik,
 [
  {description,  "ERJIK"},
  {vsn,          "0.9.0"},
  {modules,      [erjik,
                  erjik_app,
                  erjik_sup,
                  erjik_lib,
                  erjik_cfg,
                  erjik_log,
                  erjik_srv,
                  erjik_re,
                  erjik_httpd,
                  erjik_error_logger
                 ]},
  {registered,   [erjik_log,
                  erjik_cfg,
                  erjik_srv,
                  erjik_re,
                  erjik_httpd
                 ]},
  {applications, []},
  {env,          []},
  {mod,          {erjik_app, []}}
 ]}.

