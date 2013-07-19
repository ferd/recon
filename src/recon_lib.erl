-module(recon_lib).
-export([sliding_window/2, sample/2, count/1,
         port_list/1, port_list/2,
         proc_attrs/1, proc_attrs/2,
         triple_to_pid/3,
         time_map/5, time_fold/6]).

-type diff() :: [{Key::term(), Diff::number(), Other::term()}].

%% @doc Compare two samples and return a list based on some key. The type mentioned
%% for the structure is `diff()' (`{Key,Val,Other}'), which is compatible with
%% the {@link recon:proc_attrs()} type.
-spec sliding_window(First::diff(), Last::diff()) -> diff().
sliding_window(First, Last) ->
    Dict = lists:foldl(
        fun({Key, {Current, Other}}, Acc) ->
            dict:update(Key,
                        fun({Old,_Other}) -> {Current-Old, Other} end,
                        {Current, Other},
                        Acc)
        end,
        dict:from_list([{K,{V,O}} || {K,V,O} <- First]),
        [{K,{V,O}} || {K,V,O} <- Last]
    ),
    [{K,V,O} || {K,{V,O}} <- dict:to_list(Dict)].

%% @doc Runs a fun once, waits `Ms', runs the fun again,
%% and returns both results.
-spec sample(Ms::non_neg_integer(), fun(() -> term())) ->
      {First::term(), Second::term()}.
sample(Delay, Fun) ->
    First = Fun(),
    timer:sleep(Delay),
    Second = Fun(),
    {First, Second}.

%% @doc Takes a list of terms, and counts how often each of
%% them appears in the list. The list returned is in no
%% particular order.
-spec count([term()]) -> [{Count::integer(), term()}].
count(Terms) ->
    Dict = lists:foldl(
        fun(Val, Acc) ->  dict:update_counter(Val, 1, Acc) end,
        dict:new(),
        Terms
    ),
    dict:to_list(Dict).

%% @doc Returns a list of all the open ports in the VM, coupled with
%% one of the properties desired from `erlang:port_info/1-2'.
-spec port_list(Attr::atom()) -> [{port(), term()}].
port_list(Attr) ->
    [{Port,Val} || Port <- erlang:ports(),
                   {_, Val} <- [erlang:port_info(Port, Attr)]].

%% @doc Returns a list of all the open ports in the VM, but only
%% if the `Attr''s resulting value matches `Val'. `Attr' must be
%% a property accepted by `erlang:port_info/2'.
-spec port_list(Attr::atom(), term()) -> [port()].
port_list(Attr, Val) ->
    [Port || Port <- erlang:ports(),
             {Attr, Val} =:= erlang:port_info(Port, Attr)].

%% @doc Returns the attributes ({@link recon:proc_attrs()}) of
%% all processes of the node, except the caller.
-spec proc_attrs(term()) -> [recon:proc_attrs()].
proc_attrs(AttrName) ->
    [Attrs || Pid <- processes() -- [self()],
              Attrs <- [proc_attrs(AttrName, Pid)]].

%% @doc Returns the attributes of a given process. This form of attributes
%% is standard for most comparison functions for processes in recon.
-spec proc_attrs(term(), pid()) -> recon:proc_attrs().
proc_attrs(AttrName, Pid) ->
    [{_, Attr}, {registered_name,Name}, Init, Cur] =
        process_info(Pid, [AttrName, registered_name,
                           current_function, initial_call]),
    {Pid, Attr, [Name || is_atom(Name)]++[Init, Cur]}.

%% @doc Equivalent of `pid(X,Y,Z)' in the Erlang shell.
-spec triple_to_pid(N,N,N) -> pid() when
    N :: non_neg_integer().
triple_to_pid(X, Y, Z) ->
    list_to_pid("<" ++ integer_to_list(X) ++ "." ++
                       integer_to_list(Y) ++ "." ++
                       integer_to_list(Z) ++ ">").

%% @doc Calls a given function every N milliseconds and supports
%% a map-like interface (each result is modified and returned)
-spec time_map(N, Interval, Fun, State, MapFun) -> [term()] when
    N :: non_neg_integer(),
    Interval :: pos_integer(),
    Fun :: fun((State) -> {term(), State}),
    State :: term(),
    MapFun :: fun((_) -> term()).
time_map(0, _, _, _, _) ->
    [];
time_map(N, Interval, Fun, State, MapFun) ->
    {Res, NewState} = Fun(State),
    timer:sleep(Interval),
    [MapFun(Res) | time_map(N-1,Interval,Fun,NewState,MapFun)].

%% @doc Calls a given function every N milliseconds and supports
%% a fold-like interface (each result is modified and accumulated)
-spec time_fold(N, Interval, Fun, State, FoldFun, Init) -> [term()] when
    N :: non_neg_integer(),
    Interval :: pos_integer(),
    Fun :: fun((State) -> {term(), State}),
    State :: term(),
    FoldFun :: fun((term(), Init) -> Init),
    Init :: term().
time_fold(0, _, _, _, _, Acc) ->
    Acc;
time_fold(N, Interval, Fun, State, FoldFun, Init) ->
    {Res, NewState} = Fun(State),
    timer:sleep(Interval),
    Acc = FoldFun(Res,Init),
    time_fold(N-1,Interval,Fun,NewState,FoldFun,Acc).

