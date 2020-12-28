%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2020 下午6:21
%%%-------------------------------------------------------------------
-module(emqx_dashboard_cli).
-author("root").

-export([load/0, admins/1, unload/0]).

load() ->
  emqx_ctl:register_command(admins, {emqx_dashboard_cli, admins}, []).

admins(["add", Username, Password]) ->
  admins(["add", Username, Password, ""]);
admins(["add", Username, Password, Tag]) ->
  case emqx_dashboard_admin:add_user(bin(Username),
    bin(Password), bin(Tag))
  of
    ok -> io:format("ok~n");
    {error, already_existed} ->
      io:format("Error: already existed~n");
    {error, Reason} -> io:format("Error: ~p~n", [Reason])
  end;
admins(["passwd", Username, Password]) ->
  Status =
    emqx_dashboard_admin:change_password(bin(Username),
      bin(Password)),
  io:format("~p~n", [Status]);
admins(["del", Username]) ->
  Status =
    emqx_dashboard_admin:remove_user(bin(Username)),
  io:format("~p~n", [Status]);
admins(_) ->
  emqx_ctl:usage([{"admins add <Username> <Password> <Tags>",
    "Add dashboard user"},
    {"admins passwd <Username> <Password>",
      "Reset dashboard user password"},
    {"admins del <Username>", "Delete dashboard user"}]).

unload() -> emqx_ctl:unregister_command(admins).

bin(S) -> iolist_to_binary(S).