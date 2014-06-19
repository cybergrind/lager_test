-module(crashing_srv).
-author('cybergrind <cybergrind@gmail.com>').

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).
-include("inc.hrl").

-define(SERVER, ?MODULE).

start_link(Back) ->
  % gen_server:start_link(?MODULE, [], []). % for unnamed gen_server
  
  gen_server:start_link(?MODULE, [Back], []).

-spec stop() -> ok.
stop() ->
  gen_server:cast(?SERVER, stop).

init(Back) ->
  process_flag(trap_exit, true),
  ?DEBUG("Start crashing ~p ~p", [self(), Back]),
  [[Pid, Time]] = Back,
  Pid ! Time,
  {error, crash_me}.

handle_call(Req, _From, State) ->
  lager:debug("Unhandled call ~p~n", [Req]),
  {reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  lager:debug("Unhandled cast: ~p~n", [Req]),
  {noreply, State}.

handle_info(Info, State) ->
  lager:debug("Unhandled info: ~p~n", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  timer:sleep(1000),
  ok.
