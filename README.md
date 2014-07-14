lager_test
==========

Basic usage:

```erlang
% rebar get-deps
% make dev
% adv(Time, NumGood, NumCrashers)
1> t:adv(60000, 10, 10).

% cat out_*
```

Cgroups commands:

**make cgroups_init** - create cgroup

**make cgroups_clean** - remove cgroup

**make cg_dev** - run erlang node with cgexec
