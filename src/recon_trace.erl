%%% @doc
%%% `recon_trace' is a module that handles tracing in a safe manner for single
%%% Erlang nodes.
%%% Functionality includes:
%%% - Nicer to use interface
%%% - Protection against dumb decisions (matching all calls being traced,
%%%   adding rate or trace limits)
%%% - Nicer formatting
%%%
%%% The advantages are given by taking the following structure for tracing:
%%%
%%% [IO/Group leader] <---------------------,
%%%   |                                     |
%%% [shell] ---> [tracer process] ----> [formatter]
%%%
%%% The tracer process is linked to the shell, and the formatter to the
%%% tracer process. The formatter also traps exits to be able to handle
%%% all received trace messages until the tracer termination, but will then
%%% shut down as soon as possible.
-module(recon_trace).
-export([clear/0, calls/2, calls/4, calls/5]).
%% internal exports
-export([count_tracer/1, rate_tracer/2, formatter/3]).

clear() ->
    erlang:trace(all, false, [all]),
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({'_','_','_'}, false, []). % unsets global

%% TODO: - handle options
%%       - allow multiple calls to be traced

calls(Mod, Fun, Args, Max) ->
    calls([{Mod,Fun,Args}], Max).

calls(Mod, Fun, Args, Max, Time) ->
    calls([{Mod,Fun,Args}], {Max, Time}).

calls({Mod, Fun, Args}, Max) ->
    calls([{Mod,Fun,Args}], Max);
calls(MFAs = [_|_], {Max, Time}) ->
    Pid = setup(rate_tracer, [Max, Time]),
    trace_calls(MFAs, Pid);
calls(MFAs = [_|_], Max) ->
    Pid = setup(count_tracer, [Max]),
    trace_calls(MFAs, Pid).

trace_calls(MFAs, Pid) ->
    Matches = [begin
                {Arity, Spec} = validate_mfa(Mod, Fun, Args),
                erlang:trace_pattern({Mod, Fun, Arity}, Spec, [])
               end || {Mod, Fun, Args} <- MFAs],
    erlang:trace(all, true, [call, {tracer, Pid}]),
    lists:sum(Matches).

validate_mfa(Mod, Fun, Args) when is_function(Args) ->
    validate_mfa(Mod, Fun, fun_to_ms(Args));
validate_mfa(Mod, Fun, Args) ->
    BannedMods = ['_', io, lists],
    %% The banned mod check can be bypassed by using
    %% match specs if you really feel like being dumb.
    case {lists:member(Mod, BannedMods), Args} of
        {true, '_'} -> error({dangerous_combo, {Mod,Fun,Args}});
        {true, []} -> error({dangerous_combo, {Mod,Fun,Args}});
        _ -> ok
    end,
    case Args of
        '_' -> {'_', true};
        _ when is_list(Args) -> {'_', Args};
        _ when Args >= 0, Args =< 255 -> {Args, true}
    end.

setup(TracerFun, TracerArgs) ->
    clear(),
    maybe_kill(recon_trace_tracer),
    maybe_kill(recon_trace_formatter),
    Ref = make_ref(),
    Tracer = spawn_link(?MODULE, TracerFun, TracerArgs),
    register(recon_trace_tracer, Tracer),
    Format = spawn(?MODULE, formatter, [Tracer, self(), Ref]),
    register(recon_trace_formatter, Format),
    receive
        {Ref, linked} -> Tracer
    after 5000 ->
        error(setup_failed)
    end.

%% Stops when N trace messages have been received
count_tracer(0) ->
    exit(normal);
count_tracer(N) ->
    receive
        Msg ->
            recon_trace_formatter ! Msg,
            count_tracer(N-1)
    end.

%% Stops whenever the trace message rates goes higher than
%% `Max' messages in `Time' milliseconds. Note that if the rate
%% proposed is higher than what the IO system of the formatter
%% can handle, this can still put a node at risk.
%%
%% It is recommended to try stricter rates to begin with.
rate_tracer(Max, Time) -> rate_tracer(Max, Time, 0, os:timestamp()).

rate_tracer(Max, Time, Count, Start) ->
    receive
        Msg ->
            recon_trace_formatter ! Msg,
            Now = os:timestamp(),
            Delay = timer:now_diff(Now, Start) div 1000,
            if Delay > Time -> rate_tracer(Max, Time, 0, Now)
             ; Max > Count -> rate_tracer(Max, Time, Count+1, Start)
             ; Max =:= Count -> exit(normal)
            end
    end.

formatter(Tracer, Parent, Ref) ->
    process_flag(trap_exit, true),
    link(Tracer),
    Parent ! {Ref, linked},
    formatter(Tracer, group_leader()).

formatter(Tracer, Leader) ->
    receive
        {'EXIT', Tracer, normal} ->
            io:format("Recon tracer rate limit tripped.~n"),
            exit(normal);
        {'EXIT', Tracer, Reason} ->
            exit(Reason);
        TraceMsg ->
            io:format(Leader, format(TraceMsg), []),
            formatter(Tracer, Leader)
    end.


format(TraceMsg) ->
    {Type, Pid, {Hour,Min,Sec}, TraceInfo} = extract_info(TraceMsg),
    {FormatStr, FormatArgs} = case {Type, TraceInfo} of
        %% {trace, Pid, 'receive', Msg}
        {'receive', [Msg]} ->
            {"< ~p", [Msg]};
        %% {trace, Pid, send, Msg, To}
        {send, [Msg, To]} ->
            {" > ~p: ~p", [To, Msg]};
        %% {trace, Pid, send_to_non_existing_process, Msg, To}
        {send_to_non_existing_process, [Msg, To]} ->
            {" > (non_existent) ~p: ~p", [To, Msg]};
        %% {trace, Pid, call, {M, F, Args}}
        {call, [{M,F,Args}]} ->
            {"~p:~p(~s)", [M,F,format_args(Args)]};
        %% {trace, Pid, return_to, {M, F, Arity}}
        {return_to, [{M,F,Arity}]} ->
            {"~p:~p/~p", [M,F,Arity]};
        %% {trace, Pid, return_from, {M, F, Arity}, ReturnValue}
        {return_from, [{M,F,Arity}, Return]} ->
            {"~p:~p/~p --> ~p", [M,F,Arity, Return]};
        %% {trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}
        {exception_from, [{M,F,Arity}, {Class,Val}]} ->
            {"~p:~p/~p ~p ~p", [M,F,Arity, Class, Val]};
        %% {trace, Pid, spawn, Spawned, {M, F, Args}}
        {spawn, [Spawned, {M,F,Args}]}  ->
            {"spawned ~p as ~p:~p(~s)", [Spawned, M, F, format_args(Args)]};
        %% {trace, Pid, exit, Reason}
        {exit, [Reason]} ->
            {"EXIT ~p", [Reason]};
        %% {trace, Pid, link, Pid2}
        {link, [Linked]} ->
            {"link(~p)", [Linked]};
        %% {trace, Pid, unlink, Pid2}
        {unlink, [Linked]} ->
            {"unlink(~p)", [Linked]};
        %% {trace, Pid, getting_linked, Pid2}
        {getting_linked, [Linker]} ->
            {"getting linked by ~p", [Linker]};
        %% {trace, Pid, getting_unlinked, Pid2}
        {getting_unlinked, [Unlinker]} ->
            {"getting unlinked by ~p", [Unlinker]};
        %% {trace, Pid, register, RegName}
        {register, [Name]} ->
            {"registered as ~p", [Name]};
        %% {trace, Pid, unregister, RegName}
        {unregister, [Name]} ->
            {"no longer registered as ~p", [Name]};
        %% {trace, Pid, in, {M, F, Arity} | 0}
        {in, [{M,F,Arity}]} ->
            {"scheduled in for ~p:~p/~p", [M,F,Arity]};
        {in, [0]} ->
            {"scheduled in", []};
        %% {trace, Pid, out, {M, F, Arity} | 0}
        {out, [{M,F,Arity}]} ->
            {"scheduled out from ~p:~p/~p", [M, F, Arity]};
        {out, [0]} ->
            {"scheduled out", []};
        %% {trace, Pid, gc_start, Info}
        {gc_start, [Info]} ->
            HeapSize = proplists:get_value(heap_size, Info),
            {"gc beginning -- heap ~p bytes", [HeapSize]};
        %% {trace, Pid, gc_end, Info}
        {gc_end, [Info]} ->
            [Info] = TraceInfo,
            HeapSize = proplists:get_value(heap_size, Info),
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            {"gc finished -- heap ~p bytes (recovered ~p bytes)",
             [HeapSize, OldHeapSize-HeapSize]};
        _ ->
            {"unknown trace type ~p -- ~p", [Type, TraceInfo]}
    end,
    io_lib:format("~n~p:~p:~9.6.0f ~p " ++ FormatStr ++ "~n",
                  [Hour, Min, Sec, Pid] ++ FormatArgs).

extract_info(TraceMsg) ->
    case tuple_to_list(TraceMsg) of
        [trace_ts, Pid, Type | Info] ->
            {TraceInfo, [Timestamp]} = lists:split(length(Info)-1, Info),
            {Type, Pid, to_hms(Timestamp), TraceInfo};
        [trace, Pid, Type | TraceInfo] ->
            {Type, Pid, to_hms(os:timestamp()), TraceInfo}
    end.

to_hms(Stamp = {_, _, Micro}) ->
    {_,{H, M, Secs}} = calendar:now_to_local_time(Stamp),
    Seconds = Secs rem 60 + (Micro / 1000000),
    {H,M,Seconds};
to_hms(_) ->
    {0,0,0}.

maybe_kill(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            exit(Pid, kill),
            wait_for_death(Pid)
    end.

wait_for_death(Pid) ->
    case is_process_alive(Pid) of
        true ->
            timer:sleep(10),
            wait_for_death(Pid);
        false ->
            ok
    end.

format_args(Args) ->
    string:join([io_lib:format("~p", [Arg]) || Arg <- Args], ", ").

%% Borrowed from dbg
fun_to_ms(ShellFun) when is_function(ShellFun) ->
    case erl_eval:fun_data(ShellFun) of
        {fun_data,ImportList,Clauses} ->
            case ms_transform:transform_from_shell(
                   dbg,Clauses,ImportList) of
                {error,[{_,[{_,_,Code}|_]}|_],_} ->
                    io:format("Error: ~s~n",
                              [ms_transform:format_error(Code)]),
                    {error,transform_error};
                Else ->
                    Else
            end;
        false ->
            exit(shell_funs_only)
    end. 
