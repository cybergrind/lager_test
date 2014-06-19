-module(cm_srv).
-author('cybergrind <cybergrind@gmail.com>').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).

-define(SERVER, ?MODULE).
-include("inc.hrl").

start_link() ->
  % gen_server:start_link(?MODULE, [], []). % for unnamed gen_server
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
  gen_server:cast(?SERVER, stop).

init(State) ->
  gen_server:cast(self(), lets_crash),
  {ok, State}.

handle_call(Req, _From, State) ->
  lager:debug("Unhandled call ~p~n", [Req]),
  {reply, State}.

handle_cast(lets_crash, State) ->
  supervisor:start_child(crashing_sup, [[self(), now()]]),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  lager:debug("Unhandled cast: ~p~n", [Req]),
  {noreply, State}.

handle_info(OldNow, State) ->
  ?DEBUG("Get OLDNOW ~p", [OldNow]),
  stats_srv:latency(timer:now_diff(now(), OldNow)),
  gen_server:cast(self(), lets_crash),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  ok.
