%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc erjik application resource file

{application, erjik,
 [
  {description,  "ERJIK"},
  {vsn,          "0.1"},
  {modules,      [erjik,
                  erjik_app,
                  erjik_sup,
                  erjik_lib,
                  erjik_cfg,
                  erjik_srv,
                  erjik_log
                 ]},
  {registered,   [erjik_srv,
                  erjik_log,
                  erjik_cfg
                 ]},
  {applications, []},
  {env,          []},
  {mod,          {erjik_app, []}}
 ]}.

