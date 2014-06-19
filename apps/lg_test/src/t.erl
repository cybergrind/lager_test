-module(t).
-compile(export_all).
-include("inc.hrl").


start() -> application:start(lg_test).

simple() ->
  stats_srv:tstart("JUST TEST"),
  ?DEBUG("test", []),
  stats_srv:tstop().

adv(Time, NumGood, NumCrashers) ->
  M = lists:flatten(io_lib:format("Test ~p ~p ~p", [Time, NumGood, NumCrashers])),
  stats_srv:tstart(M),
  start_with_link(cm_sup, NumCrashers),
  start_with_link(good_sup, NumGood),
  timer:sleep(Time),
  stop_all_childs(cm_sup),
  stop_all_childs(good_sup),
  lager:info("Stopping"),
  stats_srv:tstop().
  

start_with_link(Sup, Num) ->
  [supervisor:start_child(Sup, []) || _ <- lists:seq(1, Num)].

stop_all_childs(Sup) ->
  [supervisor:terminate_child(Sup, Pid) || {_, Pid, _, _} <- supervisor:which_children(Sup)].
  
  
  
