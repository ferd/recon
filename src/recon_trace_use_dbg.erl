%%% @author <flmathematic@gmail.com>
%%%  [https://flmath.github.io]
%%% @doc
%%% `recon_trace_use_dbg' is a module that allows API of recon use dbg module
%%% The added value of this solution is more flexibility in the pattern matching
%%% you can pattern match any structure you BEAM can put into function guard.
%%% @end
-module(recon_trace_use_dbg).
-include_lib("stdlib/include/ms_transform.hrl").

-import(recon_trace, [formatter/5, validate_opts/1, trace_to_io/2,
                      format/1, extract_info/1, fun_to_ms/1, clear/0]).

%% API
-export([calls_dbg/3]).
 
%% Internal exports
-export([count_tracer/4, rate_tracer/4]).

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
%% @doc 
%% Allows to set trace patterns and pid specifications to trace
%% function calls using dbg module functions.
%% @end
-spec calls_dbg(tspec() | [tspec(),...], max(), options()) -> num_matches().
calls_dbg(TSpecs, Boundaries, Opts) ->
    case trace_function_types(TSpecs) of
        shell_fun ->
            io:format("Warning: TSpecs contain erlang trace template function "++
                          "use_dbg flag ignored, falling back to default recon_trace behaviour~n"),
            recon_trace:calls(TSpecs, Boundaries, proplists:delete(use_dbg, Opts));
        standard_fun ->
            clear(),
            FunTSpecs = tspecs_normalization(TSpecs),
            Formatter = validate_formatter(Opts),
            IoServer = validate_io_server(Opts),

            {PidSpecs, TraceOpts, MatchOpts} = validate_opts(Opts),
            PatternsFun =
                generate_pattern_filter(FunTSpecs, Boundaries, IoServer, Formatter),
            dbg:tracer(process,{PatternsFun, startValue(Boundaries)}),
            %% we want to receive full traces to match them then we can calculate arity
            ProcessOpts = [c]++proplists:delete(arity, TraceOpts),
            dbg:p(hd(PidSpecs), ProcessOpts),
            dbg_tp(TSpecs, MatchOpts)
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE EXPORTS %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TSPECS NORMALIZATION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tspecs_normalization(TSpecs) ->
    %% Normalizes the TSpecs to be a list of tuples
    %% {Mod, Fun, Args} where Args is a function.
    [case Args of
         '_' -> {Mod, Fun, fun pass_all/1};
         N when is_integer(N) ->
             ArgsNoFun = args_no_fun(N),
             {Mod, Fun, ArgsNoFun};
         _ ->  TSpec
     end ||  {Mod, Fun, Args} = TSpec <- TSpecs].

pass_all(V) -> V.

args_no_fun(N) ->
    fun(V) ->
            case erlang:length(V) of
                N -> V;
                _ -> throw(arity_no_match)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VALIDATE FORMATTER %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_formatter(Opts) ->
    Formatter = proplists:get_value(formatter, Opts),
    ArgsOrArity = proplists:get_value(args, Opts),
    case {ArgsOrArity, Formatter} of
        {arity, Formatter} when is_function(Formatter, 1) ->
             io:format("Custom formater, arity option ignored ~n"),
             Formatter;
        {_args, Formatter} when is_function(Formatter, 1) -> Formatter;
        {Args, _Formatter} -> default_formatter(Args)
    end.

default_formatter(arity) ->
    fun(TraceMsg) ->
            {Type, Pid, {Hour,Min,Sec}, TraceInfo} = extract_info(TraceMsg),
            {_, UpdTrace} = trace_calls_to_arity({Type, TraceInfo}),
            {FormatStr, FormatArgs} =
                trace_to_io(Type, UpdTrace),
            io_lib:format("~n~p:~p:~9.6.0f ~p " ++ FormatStr ++ "~n",
                          [Hour, Min, Sec, Pid] ++ FormatArgs)
    end;
default_formatter(_) ->
    fun recon_trace:format/1.

trace_calls_to_arity(TypeTraceInfo) ->
    case TypeTraceInfo of
        {call, [{M,F,Args}]} ->
            {call, [{M,F,length(Args)}]};
        {call, [{M,F,Args}, Msg]} ->
            {call, [{M,F,length(Args)}, Msg]};
        Trace -> Trace
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VALIDATE IO SERVER %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_io_server(Opts) ->
    proplists:get_value(io_server, Opts, group_leader()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GENERATE PATTERN FILTER %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_pattern_filter(TSpecs, {Max, Time}, IoServer, Formatter) ->
    clear(),
    rate_tracer({Max, Time}, TSpecs, IoServer, Formatter);
generate_pattern_filter(TSpecs, Max, IoServer, Formatter) ->
    clear(),
    count_tracer(Max, TSpecs, IoServer, Formatter).

count_tracer(Max, TSpecs, IoServer, Formatter) ->
    fun
        (_Trace, {N, _}) when N > Max ->
            io:format("Recon tracer rate limit tripped.~n"),
            clear();
        (Trace, N) when (N =< Max) and is_tuple(Trace) ->
            %%  Type = element(1, Trace),
            handle_trace(Trace, N, TSpecs, IoServer, Formatter)
    end.

rate_tracer({Max, Time}, TSpecs, IoServer, Formatter) ->
    fun(Trace, {N, Timestamp}) ->
            Now = os:timestamp(),
            Delay = timer:now_diff(Now, Timestamp) div 1000,

            if Delay > Time ->
                    NewN = handle_trace(Trace, 0, TSpecs, IoServer, Formatter),
                    {NewN, Now};
               Max >= N ->
                    NewN = handle_trace(Trace, N, TSpecs, IoServer, Formatter),
                    {NewN, Timestamp};
               true ->
                   io:format("Recon tracer rate limit tripped.~n"),
                   clear()
            end
    end.

handle_trace(Trace, N, TSpecs, IoServer, Formatter) ->
    Print = filter_call(Trace, TSpecs),
    case Print of
        reject -> N;
        print ->
            case Formatter(Trace) of
                "" -> ok;
                Formatted ->
                    case is_process_alive(IoServer) of
                        true -> io:format(IoServer, Formatted, []);
                        false -> io:format("Recon tracer formater stopped.~n"),
                                 clear()
                    end
            end,
            N+1;
        _ -> N
    end.

filter_call(TraceMsg, TSpecs) ->
    filter_call(TraceMsg, TSpecs, reject).

filter_call(_TraceMsg, _, print) -> print;
filter_call(_TraceMsg, [], Answer) -> Answer;
filter_call(TraceMsg, [{M, F, PatternFun} | TSpecs], reject) ->
    NewAnswer = case extract_info(TraceMsg) of
                    {call, _, _, [{TraceM,TraceF, Args}]} ->
                        test_match(M, F, TraceM, TraceF, Args, PatternFun);
                    {call, _, _, [{TraceM, TraceF, Args}, _Msg]} ->
                        test_match(M, F, TraceM, TraceF, Args, PatternFun);
                    _ -> print
                end,
    filter_call(TraceMsg, TSpecs, NewAnswer).

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
               suppress -> reject;
               _        -> print
           catch
               error:function_clause ->
                   reject;
               error:arity_no_match ->
                   reject;
               error:_E ->
                   reject
           end
  end.

%%% Start value for the dbg tracer process state
startValue({_, _}) ->
    {0, os:timestamp()};
startValue(_Max) ->
    0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEBUG TRACE PATTERN %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% The function is used to establish call trace patterns
%%% If the matchspec {M,F,Args} is on the list all calls module M
%%% and function F will be sending traces to the tracer process which will use
%%% the function to print the matching traces.
%%% @end

-spec dbg_tp([{atom(),atom(),term()}], [local] | [global]) -> num_matches().
dbg_tp(MFAList, MatchOpts) ->
    case MatchOpts of
        [local] ->
            Matches = [dbg:tpl({M, F, '_'}, [{'_', [], []}]) || {M, F, _A} <- MFAList],
            lists:sum([Cnt || {ok,[{_,_,Cnt},_]}<- Matches]);
        [global] ->
            Matches = [dbg:tp({M, F, '_'}, [{'_', [], []}]) || {M, F, _A} <- MFAList],
            lists:sum([Cnt || {ok,[{_,_,Cnt},_]}<- Matches])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHECK IF FUNCTION IS NOT STANDARD RECON %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% In the list of TSpecs, the third element of the tuple
%% is responsible for matching arguments.
%% In standard recon_trace, the third element is a template function
%% transformed to a matchspec.
%% If use_dbg is set to true, the third element is used as actual functor and interpreted
%% in a totally different way.
%% The function is used to determine the type of the functions to be traced.
%% They should be possible to interpret all functions in the same way.
%% For example '_' can be interpreted as a dbg trace function or a functor.
%% Since use_dbg is set to true, the function is considered by default a functor.
%% The function is considered a trace function if its every clause
%% ends with a functions like return_trace(),
%% that are translated into a {return_trace} in the matchspecs list.
%% ----------------------------------------------------
trace_function_types(TSpecs) ->
FunTypes= [trace_function_type(Args) || {_, _, Args} <- TSpecs],
    
    HasShell = lists:any(fun(T) -> T == shell_fun end, FunTypes),
    HasStandard = lists:any(fun(T) -> T == standard_fun end, FunTypes),
    case {HasShell, HasStandard} of
        {true, true} -> exit(mixed_function_types);
        {true, false} -> shell_fun;
        {false, _true_or_false} -> standard_fun
    end.

%% in case of fun_to_ms, the function is transformed to '_'
%% for this implementation it is transformed to fun(A) -> A end
trace_function_type('_') -> undecided_fun;
trace_function_type(N) when is_integer(N) -> undecided_fun;

%% shorthand used by shell functions 
trace_function_type(return_trace) -> shell_fun;
%% if the function is a matchspec, we check if every clause has *_trace()
trace_function_type(Patterns) when is_list(Patterns) ->
    trace_function_type(Patterns, shell_fun);

%% if function transforms it still can be proper functor, 
%% check if the is *_trace() is absent
%% if every clause has *_trace() it is a shell function
trace_function_type(PatternFun) when is_function(PatternFun, 1) ->
    try fun_to_ms(PatternFun) of
        Patterns -> trace_function_type(Patterns, undecided)
    catch
        _:_ -> standard_fun
    end.

%% all function clauses are '_'
trace_function_type([], shell_fun) -> shell_fun;
trace_function_type([], standard_fun) -> standard_fun;
trace_function_type([], undecided_fun) -> undecided_fun;

trace_function_type([Clause | Rest], Type) ->
    ClauseType = clause_type(Clause),
    case Type of
        undecided ->
            trace_function_type(Rest, ClauseType);
        _ ->
            if ClauseType =/= Type -> exit(mixed_clauses_types);
               true -> trace_function_type(Rest, Type)
            end
    end.

clause_type({_head,_guard, Return}) ->
    case lists:last(Return) of
       %% can return_trace, current_stacktrace, exception_trace
      {_return_trace} -> shell_fun;
      _ -> standard_fun
    end.    


