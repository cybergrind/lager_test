-module(crashing_sup).

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  C = ?CHILD(crashing_srv, worker),
  {ok, { {simple_one_for_one, 5000000, 1}, [C]} }.
