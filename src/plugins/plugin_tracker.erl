-module(plugin_tracker).
-behaviour(recon_plugins).

-export([filter_fun/5, start_value/2, is_plugin/0]).
-import(recon_trace, [formatter/5, validate_opts/1, trace_to_io/2,
                      format/1, extract_info/1, clear/0]).

is_plugin() ->
    true.

filter_fun(TSpecs, Boundaries, IoServer, Formatter, _Opts) ->
    generate_pattern_filter(Boundaries, TSpecs, IoServer, Formatter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GENERATE PATTERN FILTER %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_pattern_filter(TSpecs, {Max, Time}, IoServer, Formatter) ->
    clear(),
    rate_tracer({Max, Time}, TSpecs, IoServer, Formatter);
generate_pattern_filter(TSpecs, Max, IoServer, Formatter) ->
    clear(),
    count_tracer(Max, TSpecs, IoServer, Formatter).

count_tracer(Max, _TSpecs, IoServer, Formatter) ->
    fun
        (_Trace, {_, _, N}) when N >= Max ->
            IoServer ! rate_limit_tripped, clear();
        (Trace, {TSpecs, Session,N}) when (N < Max) and is_tuple(Trace) ->
            %%  Type = element(1, Trace),
            handle_trace(Trace, N, TSpecs, IoServer, Formatter, Session)
    end.

rate_tracer({_Max, _Time}, _TSpecs, _IoServer, _Formatter) ->
    io:format("Rate tracer is not supported for module: ~p~n", [?MODULE]).

handle_trace(_, _, [], IoServer, _, _) ->
     IoServer ! rate_limit_tripped, clear();
handle_trace(Trace, N, [TSpec | TSpecRest] = TSpecs, IoServer, Formatter, Session) ->
    Print = filter_call(Trace, TSpec, Session),
    case Print of
        reject -> {TSpecs, Session, N};
        {print, Value, NewSession} ->
            IoServer ! {print_value, Value},
            {TSpecRest, NewSession, N+1};
        {print, NewSession} ->
            case Formatter(Trace) of
                "" -> ok;
                Formatted -> IoServer ! {msg, Formatted}                 
            end,
            {TSpecRest, NewSession, N+1};
        _ -> {TSpecs, Session, N}
    end.


filter_call(TraceMsg, {M, F, PatternFun}, Session) ->
    case extract_info(TraceMsg) of
                    {call, _, _, [{TraceM,TraceF, Args}]} ->
                        test_match(M, F, TraceM, TraceF, Args, PatternFun, Session);
                    {call, _, _, [{TraceM, TraceF, Args}, _Msg]} ->
                        test_match(M, F, TraceM, TraceF, Args, PatternFun, Session);
                    %% if the trace is not a call, we just print it
                    _ -> {print, Session}
    end.

test_match(M, F, TraceM, TraceF, Args, PatternFun, Session) ->
    Match = 
        case {M==TraceM, ((F=='_') or (F==TraceF)), PatternFun} of
            {true, true, '_'} -> true;
            {true, true, _} -> check;
            _ -> false
    end,
   
    case Match of
       true -> {print, Session};
       false -> reject;
       check ->
           try erlang:apply(PatternFun, [Args, Session]) of
               suppress -> reject;
               print    -> {print, Session};
               {print, Value} -> {print, Value, Session};
               {print_session, NewSession} -> {print, maps:merge(Session, NewSession)};
               {print_session, Value, NewSession} -> {print, Value, maps:merge(Session, NewSession)};
               _   -> reject
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
start_value(_, {_, _}) ->
  {0, undefined, 0};
start_value(Specs, _Max) ->
    {Specs, #{}, 0}.

