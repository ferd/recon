%%% @author Fred Hebert <mononcqc@ferd.ca>
%%%  [http://ferd.ca/]
%%% @doc
%%% `recon_trace' is a module that handles tracing in a safe manner for single
%%% Erlang nodes, currently for function calls only. Functionality includes:
%%% @end
-module(recon_trace_use_dbg).
-compile(export_all).
-include_lib("stdlib/include/ms_transform.hrl").
%% API
-export([clear/0, calls/2, calls/3]).

-export([format/1]).
 
%% Internal exports
-export([count_tracer/4, formatter/5, format_trace_output/1, format_trace_output/2]).

-type matchspec()    :: [{[term()] | '_', [term()], [term()]}].
-type shellfun()     :: fun((_) -> term()).
-type formatterfun() :: fun((_) -> iodata()).
-type millisecs()    :: non_neg_integer().
-type pidspec()      :: all | existing | new | recon:pid_term().
-type max_traces()   :: non_neg_integer().
-type max_rate()     :: {max_traces(), millisecs()}.

                   %% trace options
-type options()      :: [ {pid, pidspec() | [pidspec(),...]} % default: all
                        | {timestamp, formatter | trace}     % default: formatter
                        | {args, args | arity}               % default: args
                        | {io_server, pid() | atom()}        % default: group_leader()
                        | {formatter, formatterfun()}        % default: internal formatter
                        | return_to | {return_to, boolean()} % default: false
                   %% match pattern options
                        | {scope, global | local}            % default: global
                        ].

-type mod()          :: '_' | module().
-type fn()           :: '_' | atom().
-type args()         :: '_' | 0..255 | return_trace | matchspec() | shellfun().
-type tspec()        :: {mod(), fn(), args()}.
-type max()          :: max_traces() | max_rate().
-type num_matches()  :: non_neg_integer().

-export_type([mod/0, fn/0, args/0, tspec/0, num_matches/0, options/0,
              max_traces/0, max_rate/0]).

%%%%%%%%%%%%%%
%%% PUBLIC %%%
%%%%%%%%%%%%%%

%% @doc Stops all tracing at once.
-spec clear() -> ok.
clear() ->
    dbg:p(all,clear),
    dbg:ctp('_'),
    dbg:stop(),
    erlang:trace(all, false, [all]),
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({'_','_','_'}, false, []), % unsets global
    maybe_kill(recon_trace_tracer),
    maybe_kill(recon_trace_formatter),
    maybe_kill(recon_trace_dbg_printer),
    ok.

%% @equiv calls({Mod, Fun, Args}, Max, [])
-spec calls(tspec() | [tspec(),...], max()) -> num_matches().
calls({Mod, Fun, Args}, Max) ->
    calls([{Mod,Fun,Args}], Max, []);
calls(TSpecs = [_|_], Max) ->
    calls(TSpecs, Max, []).


%% @doc Allows to set trace patterns and pid specifications to trace
%% function calls.
%%
%% @end
-spec calls(tspec() | [tspec(),...], max(), options()) -> num_matches().

calls({Mod, Fun, Args}, Max, Opts) ->
            calls([{Mod,Fun,Args}], Max, Opts);
calls(TSpecs, Max, Opts) ->
    case proplists:get_bool(use_dbg, Opts) of
            true -> calls_dbg(TSpecs, Max, Opts);
            _ -> recon_trace:calls(TSpecs, Max, Opts)
    end.

% calls_dbg(TSpecs = [_|_], {Max, Time}, Opts) ->
%     Pid = setup(rate_tracer, [Max, Time],
%                 validate_formatter(Opts), validate_io_server(Opts)),
%     trace_calls(TSpecs, Pid, Opts);


calls_dbg(TSpecs = [{M,F,A}|_], Max, Opts) ->
    case trace_function_type(A) of
        trace_fun ->
            io:format("Warning: TSpecs contain erlang trace template function"++
                          "use_dbg flag ignored, falling back to default recon_trace behaviour~n"),
            recon_trace:calls(TSpecs, Max, proplists:delete(use_dbg, Opts));
        standard_fun ->
            FunTSpecs = tspecs_normalization(TSpecs),
            Formatter = validate_formatter(Opts),
            IoServer = validate_io_server(Opts),

            PatternsFun =
                generate_pattern_filter(count_tracer, FunTSpecs,
                                        Max, IoServer, Formatter),

            dbg:tracer(process,{PatternsFun, 0}),
            dbg:p(all,[c]),
            dbg:tpl({M, F, '_'},[{'_', [], []}]),
            dbg:tp({M, F, '_'},[{'_', [], []}])
    end.

 tspecs_normalization(TSpecs) ->
    %% Normalizes the TSpecs to be a list of tuples
    %% {Mod, Fun, Args} where Args is a function.
 [case Args of
    '_' -> {Mod, Fun, fun pass_all/1};
    _ ->  TSpec 
  end ||  {Mod, Fun, Args} = TSpec <- TSpecs].

pass_all(V) -> V.       

%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE EXPORTS %%%
%%%%%%%%%%%%%%%%%%%%%%%
%% starts the tracer and formatter processes, and
%% cleans them up before each call.
generate_pattern_filter(count_tracer,
    [{M,F,PatternFun}] = TSpecs, Max, IoServer, Formater) ->
    clear(),
    %%Ref = make_ref(),
    count_tracer(Max, {M,F,PatternFun},IoServer, Formater).

%% @private Stops when N trace messages have been received
count_tracer(Max, {M, F, PatternFun}, IoServer, Formatter) ->
fun
    (_Trace, N) when N > Max -> 
        io:format("Recon tracer rate limit tripped.~n"),
        dbg:stop();
    (Trace, N) when (N =< Max) and is_tuple(Trace) ->
    %%  Type = element(1, Trace),
        Print = filter_call(Trace, M, F, PatternFun),                  
        case Print of
            reject -> N;
            print ->
                Output = Formatter(Trace),
                io:format(IoServer, Output, []),
                N+1;
            _ -> N
        end
    %%(Trace, N) when N =< Max ->
    %% io:format("Unexpexted trace~p", [Trace]), N
end.

trace_function_type(Patterns) when is_list(Patterns) ->
    trace_function_type(Patterns, trace_fun);
trace_function_type(PatternFun) when is_function(PatternFun, 1) ->
    try fun_to_ms(PatternFun) of
        Patterns -> trace_function_type(Patterns, not_decided)
    catch
        _:_ -> standard_fun
    end.

%% all function clauses are '_'
trace_function_type([], trace_fun) -> trace_fun;
trace_function_type([], standard_fun) -> standard_fun;

trace_function_type([Clause | Rest], Type) ->
    ClauseType = clause_type(Clause),
    case Type of
        not_decided ->
            trace_function_type(Rest, ClauseType);
        _ ->
            if ClauseType =/= Type -> exit(mixed_clauses_types);
               true -> trace_function_type(Rest, Type)
            end
    end.
%% actually, it should not be possible since the thirdlist has at least return value
clause_type({_head,_guard, []}) -> standard_fun;
clause_type({_head,_guard, Return}) ->
    case lists:last(Return) of
       %% can return_trace, current_stacktrace, exception_trace
      {_return_trace} -> trace_fun;
      _ -> standard_fun
    end.    



% @doc
%% @private Filters the trace messages
%% and calls the pattern function
%% @end

filter_call(TraceMsg, M, F, PatternFun) ->
    case extract_info(TraceMsg) of
        {call, _, _, [{TraceM,TraceF, Args}]} ->
            test_match(M, F, TraceM, TraceF, Args, PatternFun);
        {call, _, _, [{TraceM, TraceF, Args}, _Msg]} ->
            test_match(M, F, TraceM, TraceF, Args, PatternFun);
        _ -> print
    end.

test_match(M, F, TraceM, TraceF, Args, PatternFun) ->
    Match = 
        case {M==TraceM, ((F=='_') or (F==TraceF)), PatternFun} of
            {true, true, '_'} -> true;
            {true, true, _} -> check;
            _ -> false
    end,
   
    case Match of
       true -> print; 
       false -> reject;
       check ->
           try erlang:apply(PatternFun, [Args]) of
               _ -> print
           catch
               error:function_clause ->
                   reject;
               error:E ->
                   reject
           end
  end.

%% @private Formats traces to be output
formatter(Tracer, Parent, Ref, FormatterFun, IOServer) ->
    process_flag(trap_exit, true),
    link(Tracer),
    Parent ! {Ref, linked},
    formatter(Tracer, IOServer, FormatterFun).

formatter(Tracer, IOServer, FormatterFun) ->
    receive
        {'EXIT', Tracer, normal} ->
            io:format("Recon tracer rate limit tripped.~n"),
            exit(normal);
        {'EXIT', Tracer, Reason} ->
            exit(Reason);
        TraceMsg ->
            case FormatterFun(TraceMsg) of
                "" -> ok;
                Formatted -> io:format(IOServer, Formatted, [])
            end,
            formatter(Tracer, IOServer, FormatterFun)
    end.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
%% Sets the traces in action
trace_calls(TSpecs, Pid, Opts) ->
    {PidSpecs, TraceOpts, MatchOpts} = validate_opts(Opts),
    Matches = [begin
                {Arity, Spec} = validate_tspec(Mod, Fun, Args),
                erlang:trace_pattern({Mod, Fun, Arity}, Spec, MatchOpts)
               end || {Mod, Fun, Args} <- TSpecs],
    [erlang:trace(PidSpec, true, [call, {tracer, Pid} | TraceOpts])
     || PidSpec <- PidSpecs],
    lists:sum(Matches).

  
%%%%%%%%%%%%%%%%%%
%%% VALIDATION %%%
%%%%%%%%%%%%%%%%%%

validate_opts(Opts) ->
    PidSpecs = validate_pid_specs(proplists:get_value(pid, Opts, all)),
    Scope = proplists:get_value(scope, Opts, global),
    TraceOpts = case proplists:get_value(timestamp, Opts, formatter) of
                    formatter -> [];
                    trace -> [timestamp]
                 end ++
                 case proplists:get_value(args, Opts, args) of
                    args -> [];
                    arity -> [arity]
                 end ++
                 case proplists:get_value(return_to, Opts, undefined) of
                     true when Scope =:= local ->
                         [return_to];
                     true when Scope =:= global ->
                         io:format("Option return_to only works with option {scope, local}~n"),
                         %% Set it anyway
                         [return_to];
                     _ ->
                         []
                 end,
    MatchOpts = [Scope],
    {PidSpecs, TraceOpts, MatchOpts}.

%% Support the regular specs, but also allow `recon:pid_term()' and lists
%% of further pid specs.
-spec validate_pid_specs(pidspec() | [pidspec(),...]) ->
    [all | new | existing | pid(), ...].
validate_pid_specs(all) -> [all];
validate_pid_specs(existing) -> [existing];
validate_pid_specs(new) -> [new];
validate_pid_specs([Spec]) -> validate_pid_specs(Spec);
validate_pid_specs(PidTerm = [Spec|Rest]) ->
    %% can be "<a.b.c>" or [pidspec()]
    try
        [recon_lib:term_to_pid(PidTerm)]
    catch
        error:function_clause ->
            validate_pid_specs(Spec) ++ validate_pid_specs(Rest)
    end;
validate_pid_specs(PidTerm) ->
    %% has to be `recon:pid_term()'.
    [recon_lib:term_to_pid(PidTerm)].

validate_tspec(Mod, Fun, Args) when is_function(Args) ->
    validate_tspec(Mod, Fun, fun_to_ms(Args));
%% helper to save typing for common actions
validate_tspec(Mod, Fun, return_trace) ->
    validate_tspec(Mod, Fun, [{'_', [], [{return_trace}]}]);
validate_tspec(Mod, Fun, Args) ->
    BannedMods = ['_', ?MODULE, io, lists],
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

validate_formatter(Opts) ->
    case proplists:get_value(formatter, Opts) of
        F when is_function(F, 1) -> F;
        _ -> fun format/1
    end.

validate_io_server(Opts) ->
    proplists:get_value(io_server, Opts, group_leader()).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% TRACE FORMATTING %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% Thanks Geoff Cant for the foundations for this.
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
            {"~p:~p~s", [M,F,format_args(Args)]};
        %% {trace, Pid, call, {M, F, Args}, Msg}
        {call, [{M,F,Args}, Msg]} ->
            {"~p:~p~s ~s", [M,F,format_args(Args), format_trace_output(Msg)]};
        %% {trace, Pid, return_to, {M, F, Arity}}
        {return_to, [{M,F,Arity}]} ->
            {" '--> ~p:~p/~p", [M,F,Arity]};
        %% {trace, Pid, return_from, {M, F, Arity}, ReturnValue}
        {return_from, [{M,F,Arity}, Return]} ->
            {"~p:~p/~p --> ~s", [M,F,Arity, format_trace_output(Return)]};
        %% {trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}
        {exception_from, [{M,F,Arity}, {Class,Val}]} ->
            {"~p:~p/~p ~p ~p", [M,F,Arity, Class, Val]};
        %% {trace, Pid, spawn, Spawned, {M, F, Args}}
        {spawn, [Spawned, {M,F,Args}]}  ->
            {"spawned ~p as ~p:~p~s", [Spawned, M, F, format_args(Args)]};
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
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            MbufSize = proplists:get_value(mbuf_size, Info),
            {"gc beginning -- heap ~p bytes",
             [HeapSize + OldHeapSize + MbufSize]};
        %% {trace, Pid, gc_end, Info}
        {gc_end, [Info]} ->
            HeapSize = proplists:get_value(heap_size, Info),
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            MbufSize = proplists:get_value(mbuf_size, Info),
            {"gc finished -- heap ~p bytes",
             [HeapSize + OldHeapSize + MbufSize]};
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

format_args(Arity) when is_integer(Arity) ->
    [$/, integer_to_list(Arity)];
format_args(Args) when is_list(Args) ->
    [$(, join(", ", [format_trace_output(Arg) || Arg <- Args]), $)].


%% @doc formats call arguments and return values - most types are just printed out, except for
%% tuples recognised as records, which mimic the source code syntax
%% @end
format_trace_output(Args) ->
    format_trace_output(recon_rec:is_active(), recon_map:is_active(), Args).

format_trace_output(Recs, Args) ->
    format_trace_output(Recs, recon_map:is_active(), Args).

format_trace_output(true, _, Args) when is_tuple(Args) ->
    recon_rec:format_tuple(Args);
format_trace_output(false, true, Args) when is_tuple(Args) ->
    format_tuple(false, true, Args);
format_trace_output(Recs, Maps, Args) when is_list(Args), Recs orelse Maps ->
    case io_lib:printable_list(Args) of
        true ->
            io_lib:format("~p", [Args]);
        false ->
            format_maybe_improper_list(Recs, Maps, Args)
    end;
format_trace_output(Recs, true, Args) when is_map(Args) ->
    {Label, Map} = case recon_map:process_map(Args) of
                       {L, M} -> {atom_to_list(L), M};
                       M -> {"", M}
                   end,
    ItemList = maps:to_list(Map),
    [Label,
     "#{",
        join(", ", [format_kv(Recs, true, Key, Val) || {Key, Val} <- ItemList]),
    "}"];
format_trace_output(Recs, false, Args) when is_map(Args) ->
    ItemList = maps:to_list(Args),
    ["#{",
        join(", ", [format_kv(Recs, false, Key, Val) || {Key, Val} <- ItemList]),
    "}"];
format_trace_output(_, _, Args) ->
    io_lib:format("~p", [Args]).

format_kv(Recs, Maps, Key, Val) ->
    [format_trace_output(Recs, Maps, Key), "=>", format_trace_output(Recs, Maps, Val)].


format_tuple(Recs, Maps, Tup) ->
    [${ | format_tuple_(Recs, Maps, tuple_to_list(Tup))].

format_tuple_(_Recs, _Maps, []) ->
    "}";
format_tuple_(Recs, Maps, [H|T]) ->
    [format_trace_output(Recs, Maps, H), $,,
     format_tuple_(Recs, Maps, T)].


format_maybe_improper_list(Recs, Maps, List) ->
    [$[ | format_maybe_improper_list_(Recs, Maps, List)].

format_maybe_improper_list_(_, _, []) ->
    "]";
format_maybe_improper_list_(Recs, Maps, [H|[]]) ->
    [format_trace_output(Recs, Maps, H), $]];
format_maybe_improper_list_(Recs, Maps, [H|T]) when is_list(T) ->
    [format_trace_output(Recs, Maps, H), $,,
     format_maybe_improper_list_(Recs, Maps, T)];
format_maybe_improper_list_(Recs, Maps, [H|T]) when not is_list(T) ->
    %% Handling improper lists
    [format_trace_output(Recs, Maps, H), $|,
     format_trace_output(Recs, Maps, T), $]].


%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

maybe_kill(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            exit(Pid, kill),
            wait_for_death(Pid, Name)
    end.

wait_for_death(Pid, Name) ->
    case is_process_alive(Pid) orelse whereis(Name) =:= Pid of
        true ->
            timer:sleep(10),
            wait_for_death(Pid, Name);
        false ->
            ok
    end.

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

-ifdef(OTP_RELEASE).
-spec join(term(), [term()]) -> [term()].
join(Sep, List) ->
    lists:join(Sep, List).
-else.
-spec join(string(), [string()]) -> string().
join(Sep, List) ->
    string:join(List, Sep).
-endif.
