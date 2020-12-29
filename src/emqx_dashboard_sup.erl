%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2020 下午6:25
%%%-------------------------------------------------------------------
-module(emqx_dashboard_sup).
-author("root").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
  supervisor:start_link({local, emqx_dashboard_sup}, emqx_dashboard_sup, []).

init([]) ->
  {ok,
    {{one_for_all, 10, 100},
      [{emqx_dashboard_admin, {emqx_dashboard_admin, start_link, []}, permanent, 5000, worker, [emqx_dashboard_admin]},
        {emqx_dashboard_collection, {emqx_dashboard_collection, start_link, [#{}]}, permanent, 5000, worker, [emqx_dashboard_collection]}]}}.


