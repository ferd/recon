%%% @private Code used for escripts communicating with a recon
%%% node. Not to be used for any other reason.
-module(recon_escript).
-export([stats/3, procs/5, inet/5]).

stats(Interval, To, Ref) when is_pid(To) ->
    link(To),
    N = 999999999999999999999999999999999999, % roughly forever
    DictToIo = fun(L) ->
        [begin
            Spaces = lists:duplicate(flat(30-length(atom_to_list(Key))),$ ),
            io_lib:format("~p~s~p~n",[Key,Spaces,Val])
        end || {Key, Val} <- L]
    end,
    FoldFun = fun({Repeat,Intervals},_) ->
        IoList = [
         "Repeated\n",
         "========\n",
         DictToIo(Repeat),"\n",
         "Interval\n",
         "========\n",
         DictToIo(Intervals),
         "\n"],
        To ! {Ref, IoList}
    end,
    recon:node_stats(N, Interval, FoldFun, ok).

procs(Key, Num, Interval, To, Ref) ->
    link(To),
    recon_lib:time_fold(
      999999999999999999999999999999999999, % roughly forever
      0, % interval taken care of by proc_window/3
      fun(_) -> {recon:proc_window(Key, Num, Interval), nostate} end,
      nostate,
      fun(List,_) -> window_to_io_msg(List, Key, To, Ref) end,
      noinit).

inet(Key, Num, Interval, To, Ref) ->
    link(To),
    recon_lib:time_fold(
      999999999999999999999999999999999999, % roughly forever
      0, % interval taken care of by proc_window/3
      fun(_) -> {recon:inet_window(Key, Num, Interval), nostate} end,
      nostate,
      fun(List,_) -> window_to_io_msg(List, Key, To, Ref) end,
      noinit).

window_to_io_msg(List, Key, To, Ref) ->
    WindowToDict = fun(L) ->
        [begin
            PSpaces = lists:duplicate(flat(20-length(lists:flatten(io_lib:format("~p",[P])))),$ ),
            CSpaces = lists:duplicate(flat(10-length(integer_to_list(Count))),$ ),
            io_lib:format("~p~s~p~s ~P~n",[P,PSpaces,Count,CSpaces,Details,40])
        end || {P, Count, Details} <- L]
    end,
    IoList = [
        "Item\t    ",atom_to_list(Key),"\tDetails\n"
        "=================================================================\n",
        WindowToDict(List),
        "\n"],
    To ! {Ref, IoList}.

flat(N) when N < 0 -> 0;
flat(N) -> N.
