%%% @private Code used for escripts communicating with a recon
%%% node. Not to be used for any other reason.
-module(recon_escript).
-export([ping/2, stats/3, procs/5, inet/5]).

ping(To, Ref) ->
    To ! Ref.

stats(Interval, To, Ref) when is_pid(To) ->
    link(To),
    N = 999999999999999999999999999999999999, % roughly forever
    FoldFun = fun({Repeat,Intervals},_) ->
        To ! {Ref, Repeat++Intervals}
    end,
    recon:node_stats(N, Interval, FoldFun, ok).

procs(Key, Num, Interval, To, Ref) ->
    link(To),
    recon_lib:time_fold(
      999999999999999999999999999999999999, % roughly forever
      0, % interval taken care of by proc_window/3
      fun(_) -> {recon:proc_window(Key, Num, Interval), nostate} end,
      nostate,
      fun(List,_) -> To ! {Ref, List} end,
      noinit).

inet(Key, Num, Interval, To, Ref) ->
    link(To),
    recon_lib:time_fold(
      999999999999999999999999999999999999, % roughly forever
      0, % interval taken care of by proc_window/3
      fun(_) -> {recon:inet_window(Key, Num, Interval), nostate} end,
      nostate,
      fun(List,_) -> To ! {Ref, List} end,
      noinit).

