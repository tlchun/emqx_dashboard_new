%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2020 下午6:25
%%%-------------------------------------------------------------------
-module(emqx_dashboard_license_api).
-author("root").

-include("../include/emqx_dashboard.hrl").

-import(minirest, [return/1]).

-rest_api(#{descr => "Get a license info", func => license_info, method => 'GET', name => license_info, path => "/license_info"}).

-export([license_info/2]).


license_info(_Bindings, _Params) ->
%%  Info = emqx_license_mgr:info(),
  Info = #{customer := {"_Text", "Customer"}, email := "513036862@qq.com",
    permits := "免费", product := "EMQX",
    validity := {"2020", "20200"}, vendor := "Vendor",
    version := "4.2.3"},
  return({ok, info_list_to_binary(Info)}).

info_list_to_binary(Info) -> list_to_binary(Info, []).

list_to_binary([], Acc) -> lists:reverse(Acc);
list_to_binary([{Key, Val} | Info], Acc) when is_list(Val) -> list_to_binary(Info, [{Key, list_to_binary(Val)} | Acc]);
list_to_binary([{Key, Val} | Info], Acc) -> list_to_binary(Info, [{Key, Val} | Acc]).
