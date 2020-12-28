%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2020 下午6:24
%%%-------------------------------------------------------------------
-module(emqx_dashboard_collection).
-author("root").

-behaviour(gen_server).

-include("../include/emqx_dashboard.hrl").
-include("../include/ms_transform.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).

-export([get_collect/0]).

-export([get_local_time/0]).

-boot_mnesia({mnesia, [boot]}).

-copy_mnesia({mnesia, [copy]}).

-export([mnesia/1]).


mnesia(boot) ->
  ok = ekka_mnesia:create_table(emqx_collect,
    [{type, set}, {local_content, true},
      {disc_only_copies, [node()]},
      {record_name, mqtt_collect},
      {attributes,
        record_info(fields, mqtt_collect)}]);
mnesia(copy) ->
  mnesia:add_table_copy(emqx_collect, node(),
    disc_only_copies).

start_link(Opts) ->
  gen_server:start_link({local, emqx_dashboard_collection}, emqx_dashboard_collection, [Opts], []).

get_collect() ->
  gen_server:call(whereis(emqx_dashboard_collection), get_collect).

init([Opts]) ->
  timer(timer:seconds(10), collect),
  timer(get_today_remaining_seconds(), clear_expire_data),
  ExpireInterval = application:get_env(emqx_dashboard, expire_interval, 86400000 * 7),
  {ok,
    #{count => maps:get(count, Opts, 6),
      collect => {[], [], []}, temp_collect => {0, 0, 0, 0},
      expire_interval => ExpireInterval,
      last_collects => {0, 0, 0}}}.

handle_call(get_collect, _From,
    State = #{temp_collect := {Received, Sent, _, _}}) ->
  {reply,
    {Received, Sent, collect(subscriptions),
      collect(connections)},
    State, hibernate};
handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast(_Req, State) -> {noreply, State}.

handle_info(collect,
    State = #{collect := Collect, count := 1,
      temp_collect := TempCollect,
      last_collects := LastCollect}) ->
  NewLastCollect = flush(collect_all(Collect),
    LastCollect),
  TempCollect1 = temp_collect(TempCollect),
  timer(timer:seconds(10), collect),
  {noreply,
    State#{count => 6, collect => {[], [], []},
      temp_collect => TempCollect1,
      last_collects => NewLastCollect}};
handle_info(collect,
    State = #{count := Count, collect := Collect,
      temp_collect := TempCollect}) ->
  TempCollect1 = temp_collect(TempCollect),
  timer(timer:seconds(10), collect),
  {noreply,
    State#{count => Count - 1,
      collect => collect_all(Collect),
      temp_collect => TempCollect1},
    hibernate};
handle_info(clear_expire_data,
    State = #{expire_interval := ExpireInterval}) ->
  timer(86400000, clear_expire_data),
  T1 = get_local_time(),
  Spec = [{{'_', '$1', '$2'},
    [{'>', {'-', {const, T1}, '$1'},
      {const, ExpireInterval}}],
    ['$_']}],
  Collects = dets:select(emqx_collect, Spec),
  lists:foreach(fun (Collect) ->
    dets:delete_object(emqx_collect, Collect)
                end,
    Collects),
  {noreply, State, hibernate};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

temp_collect({_, _, Received, Sent}) ->
  Received1 = collect(received),
  Sent1 = collect(sent),
  {(Received1 - Received) div 10, (Sent1 - Sent) div 10,
    Received1, Sent1}.

collect_all({Connection, Route, Subscription}) ->
  {[collect(connections) | Connection],
    [collect(routes) | Route],
    [collect(subscriptions) | Subscription]}.

collect(connections) ->
  emqx_stats:getstat('connections.count');
collect(routes) -> emqx_stats:getstat('routes.count');
collect(subscriptions) ->
  emqx_stats:getstat('subscriptions.count');
collect(received) ->
  emqx_metrics:val('messages.received');
collect(sent) -> emqx_metrics:val('messages.sent');
collect(dropped) ->
  emqx_metrics:val('messages.dropped').

flush({Connection, Route, Subscription},
    {Received0, Sent0, Dropped0}) ->
  Received = collect(received),
  Sent = collect(sent),
  Dropped = collect(dropped),
  Collect = {avg(Connection), avg(Route),
    avg(Subscription), diff(Received, Received0),
    diff(Sent, Sent0), diff(Dropped, Dropped0)},
  Ts = get_local_time(),
  mnesia:dirty_write(emqx_collect,
    #mqtt_collect{timestamp = Ts, collect = Collect}),
  {Received, Sent, Dropped}.

avg(Items) -> lists:sum(Items) div 6.

diff(Item0, Item1) -> Item0 - Item1.

timer(Secs, Msg) ->
  erlang:send_after(Secs, self(), Msg).

get_today_remaining_seconds() ->
  86400000 - get_local_time() rem 86400000.

get_local_time() ->
  (calendar:datetime_to_gregorian_seconds(calendar:local_time())
    -
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1},
      {0, 0, 0}}))
    * 1000.
