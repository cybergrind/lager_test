lager_test
==========

Basic usage:

```erlang
% rebar get-deps
% make dev
% adv(Time, NumGood, NumCrashers)
1> t:adv(60000, 10, 10).

% cat out_*
