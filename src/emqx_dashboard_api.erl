%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2020 下午6:15
%%%-------------------------------------------------------------------
-module(emqx_dashboard_api).
-author("root").

-include("../include/emqx_dashboard.hrl").


-import(minirest, [return/0, return/1]).

-rest_api(#{descr => "Authenticate an user", func => auth, method => 'POST', name => auth_user, path => "/auth"}).

-rest_api(#{descr => "Create an user", func => create, method => 'POST', name => create_user, path => "/users/"}).

-rest_api(#{descr => "List users", func => list, method => 'GET', name => list_users, path => "/users/"}).

-rest_api(#{descr => "Update an user", func => update, method => 'PUT', name => update_user, path => "/users/:bin:name"}).

-rest_api(#{descr => "Delete an user", func => delete, method => 'DELETE', name => delete_user, path => "/users/:bin:name"}).

-rest_api(#{descr => "Change password for an user", func => change_pwd, method => 'PUT', name => change_pwd, path => "/change_pwd/:bin:username"}).

-export([list/2, create/2, update/2, delete/2, auth/2, change_pwd/2]).


auth(_Bindings, Params) ->
  Username = proplists:get_value(<<"username">>, Params),
  Password = proplists:get_value(<<"password">>, Params),
  return(emqx_dashboard_admin:check(Username, Password)).

change_pwd(#{username := Username}, Params) ->
  OldPwd = proplists:get_value(<<"old_pwd">>, Params),
  NewPwd = proplists:get_value(<<"new_pwd">>, Params),
  return(emqx_dashboard_admin:change_password(Username, OldPwd, NewPwd)).

create(_Bindings, Params) ->
  Username = proplists:get_value(<<"username">>, Params),
  Password = proplists:get_value(<<"password">>, Params),
  Tags = proplists:get_value(<<"tags">>, Params),
  return(case (Username == undefined orelse
    Username == <<>>)
    orelse Password == undefined orelse Password == <<>>
         of
           true -> {error, <<"Username or password undefined">>};
           false ->
             emqx_dashboard_admin:add_user(Username, Password, Tags)
         end).

list(_Bindings, _Params) ->
  return({ok, [row(User) || User <- emqx_dashboard_admin:all_users()]}).

update(#{name := Username}, Params) ->
  Tags = proplists:get_value(<<"tags">>, Params),
  return(emqx_dashboard_admin:update_user(Username, Tags)).

delete(#{name := <<"admin">>}, _Params) ->
  return({error, <<"Cannot delete admin">>});
delete(#{name := Username}, _Params) ->
  return(emqx_dashboard_admin:remove_user(Username)).

row(#mqtt_admin{username = Username, tags = Tags}) ->
  [{username, Username}, {tags, Tags}].

