%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc erjik application resource file

{application, erjik,
 [
  {description,  "ERJIK"},
  {vsn,          "0.2"},
  {modules,      [erjik,
                  erjik_app,
                  erjik_sup,
                  erjik_lib,
                  erjik_cfg,
                  erjik_log,
                  erjik_srv,
                  erjik_httpd
                 ]},
  {registered,   [erjik_log,
                  erjik_cfg,
                  erjik_srv,
                  erjik_httpd
                 ]},
  {applications, []},
  {env,          []},
  {mod,          {erjik_app, []}}
 ]}.

