%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2020 下午6:22
%%%-------------------------------------------------------------------
-module(emqx_dashboard_cluster_api).
-author("root").

-include("../include/emqx_dashboard.hrl").

-import(minirest, [return/1]).

-rest_api(#{descr => "Get a Cluster info",
  func => cluster_info, method => 'GET',
  name => cluster_info, path => "/cluster"}).

-rest_api(#{descr =>
"Invite a node to join the cluster",
  func => invite_node, method => 'POST',
  name => invite_node, path => "/cluster/invite_node"}).

-rest_api(#{descr =>
"Force a node to leave the cluster",
  func => force_leave, method => 'DELETE',
  name => force_leave,
  path => "/cluster/force_leave/:bin:node"}).

-export([cluster_info/2, invite_node/2, force_leave/2]).

-vsn("4.2.2").

cluster_info(_Bindings, _Params) ->
  ClusterName = application:get_env(ekka, cluster_name, emqxcl),
  {Type, ClusterConf} = application:get_env(ekka, cluster_discovery, {manual, []}),
  Info = [{name, ClusterName}, {type, Type}, {config, format(ClusterConf)}],
  return({ok, Info}).

invite_node(_Bindings, Params) ->
  Node = proplists:get_value(<<"node">>, Params),
  case
    rpc:call(ekka_node:parse_name(binary_to_list(Node)),
      ekka, join, [node()])
  of
    ok -> return(ok);
    ignore -> return({error, <<"Not invite self">>});
    {badrpc, Error} -> return({error, Error});
    {error, Error} ->
      return({error, iolist_to_binary(io_lib:format("~0p", [Error]))})
  end.

force_leave(#{node := Node}, _Params) ->
  case
    ekka:force_leave(ekka_node:parse_name(binary_to_list(Node)))
  of
    ok -> return(ok);
    ignore -> return({error, <<"Not force leave self">>});
    {error, Error} ->
      return({error,
        iolist_to_binary(io_lib:format("~0p", [Error]))})
  end.

format(ClusterConf) ->
  lists:reverse(format(ClusterConf, [])).

format([], Acc) -> Acc;
format([{ssl_options, Val} | Conf], Acc) ->
  format(Conf, [{ssl_options, format(Val, [])} | Acc]);
format([{Key, Val} | Conf], Acc)
  when Key =:= seeds orelse Key =:= ports ->
  format(Conf, [{Key, Val} | Acc]);
format([{Key, Val} | Conf], Acc)
  when Key =:= addr orelse Key =:= iface ->
  format(Conf,
    [{Key, list_to_binary(esockd_net:ntoa(Val))} | Acc]);
format([{Key, Val} | Conf], Acc) when is_list(Val) ->
  format(Conf, [{Key, list_to_binary(Val)} | Acc]);
format([{Key, Val} | Conf], Acc) ->
  format(Conf, [{Key, Val} | Acc]).

