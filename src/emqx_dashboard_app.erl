%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2020 下午6:19
%%%-------------------------------------------------------------------
-module(emqx_dashboard_app).
-author("root").

-behaviour(application).
-emqx_plugin(emqx_dashboard_app).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = emqx_dashboard_sup:start_link(),
  emqx_dashboard:start_listeners(),
  emqx_dashboard_cli:load(),
  {ok, Sup}.

stop(_State) ->
  emqx_dashboard_cli:unload(),
  emqx_dashboard:stop_listeners().




