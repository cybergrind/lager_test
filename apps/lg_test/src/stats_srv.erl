-module(stats_srv).
-author('cybergrind <cybergrind@gmail.com>').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).
-export([log_msg/0, tstart/1, tstop/0, latency/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, Pid} | ignore | {error, Error}
  when
      Pid :: pid(),
      Error :: {already_started, Pid} | term().
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
  gen_server:cast(?SERVER, stop).

log_msg() ->
  gen_server:cast(?SERVER, debug).
tstart(Desc) ->
  gen_server:cast(?SERVER, {test_start, Desc}).
tstop() ->
  gen_server:cast(?SERVER, test_stop).
latency(Lat) ->
  gen_server:cast(?SERVER, {latency, Lat}).


new_state(Desc) ->
  #{history => [],
    desc => Desc,
    requests => 0,
    crashes => 0,
    latency => 0,
    start => now(),
    file_size => 0}.

init([]) ->
  State = undefined,
  gen_server:cast(self(), loop_timer),
  {ok, State}.

handle_call(Req, _From, State) ->
  lager:debug("Unhandled call ~p~n", [Req]),
  {reply, State}.

handle_cast({test_start, Desc}, _) ->
  lager:debug("Start test ~s", [Desc]),
  State = new_state(Desc),
  {noreply, State};
handle_cast(loop_timer, State) ->
  lager:info('Loop timer'),
  case State of
    undefined -> ok;
    _ -> gen_server:cast(self(), loop) end,
  erlang:send_after(1000, self(), {'$gen_cast', loop_timer}),
  {noreply, State};
handle_cast(_O, undefined) ->
  {noreply, undefined};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(test_stop, State) ->
  lager:error("Stop test!!"),
  {H, M, S} = time(),
  FName = lists:flatten(io_lib:format("out_~p_~p_~p.log", [H, M, S])),
  Cont = io_lib:fwrite("~p", [State]),
  R = file:write_file(FName, Cont),
  lager:debug("Write resp ~p", [R]),
  {noreply, undefined};
handle_cast({latency, N}, State) ->
  #{latency := L} = State,
  {noreply, State#{latency => (N+L)/2}};
handle_cast(crash, State) ->
  #{crashes := R} = State,
  {noreply, State#{crashes => R+1}};
handle_cast(debug, State) ->
  #{requests := R} = State,
  {noreply, State#{requests => R+1}};
handle_cast(loop, State) ->
  T = calendar:universal_time(),
  #{requests := Msgs,
    start := Start,
    history := History} = State,
  MsgsSec = Msgs/(timer:now_diff(now(), Start)/1000000),
  LogMsg = (maps:remove(history, State))#{time => T,
                                          req_sec => MsgsSec},
  %lager:debug("Slice ~p", [LogMsg]),
  NewState = State#{history => [LogMsg | History],
                    requests => 0,
                    crashes => 0,
                    latency => 0,
                    start => now(),
                    file_size => filelib:file_size("edebug.log")},
  stats_srv:log_msg(),
  {noreply, NewState};
handle_cast(Req, State) ->
  lager:debug("Unhandled cast: ~p~n", [Req]),
  {noreply, State}.

handle_info(Info, State) ->
  lager:debug("Unhandled info: ~p~n", [Info]),
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
