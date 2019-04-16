%%%-------------------------------------------------------------------
%%% @author bartlomiej.gorny@erlang-solutions.com
%%% @doc
%%% This module handles formatting maps.
%% It allows for trimming output to selected fields, or to nothing at all. It also adds a label
%% to a printout.
%% To set up a limit for a map, you need to give recon a way to tell the map you want to
%% trim from all the other maps, so you have to provide something like a 'type definition'.
%% It can be either another map which is compared to the arg, or a fun.
%%% @end
%%%-------------------------------------------------------------------
-module(recon_map).
-author("bartlomiej.gorny@erlang-solutions.com").
%% API

-export([limit/3]).
-export([list/0]).
-export([process_map/1]).
-export([is_active/0]).
-export([clear/0]).

-type map_label() :: atom().
-type pattern() :: map().
-type field() :: atom().
-type limit() :: all | none | field() | [field()].

%% @doc quickly check if we want to do any record formatting
-spec is_active() -> boolean().
is_active() ->
    case whereis(recon_ets_maps) of
        undefined -> false;
        _ -> true
    end.

%% @doc remove all imported definitions, destroy the table, clean up
clear() ->
    maybe_kill(recon_ets_maps),
    ok.

%% @doc Limit output to selected keys of a map (can be 'none', 'all', a key or a list of keys).
%% Pattern selects maps to process: a "pattern" is just a map, and if all key/value pairs of a pattern
%% are present in a map (in other words, the pattern is a subset), then we say the map matches
%% and we process it accordingly (apply the limit).
%%
%% Instead of a pattern you can also provide a function which will take a map and return a boolean.
%% @end
-spec limit(map_label(), pattern(), limit()) -> ok | {error, any()}.
limit(Label, #{} = Pattern, Limit) when is_atom(Label) ->
    store_pattern(Label, Pattern, Limit);
limit(Label, Pattern, Limit) when is_atom(Label), is_function(Pattern) ->
    store_pattern(Label, Pattern, Limit).

list() ->
    ensure_table_exists(),
    io:format("~nmap definitions and limits:~n"),
    list(lists:sort(ets:tab2list(patterns_table_name()))).

list([]) ->
    io:format("~n"),
    ok;
list([{Label, Pattern, Limit} | Rest]) ->
    io:format("~p: ~p -> ~p~n", [Label, Pattern, Limit]),
    list(Rest).

%% @doc given a map, scans saved patterns for one that matches; if found, returns a label
%% and a map with limits applied; otherwise returns 'none' and original map.
%% Pattern can be:
%% <ul>
%% <li> a map - then each key in pattern is checked for equality with the map in question</li>
%% <li> a fun(map()) -> boolean()</li>
%% </ul>
-spec process_map(map()) -> map() | {atom(), map()}.
process_map(M) ->
    process_map(M, ets:tab2list(patterns_table_name())).

process_map(M, []) ->
    M;
process_map(M, [{Label, Pattern, Limit} | Rest]) ->
    case map_matches(M, Pattern) of
        true ->
            {Label, apply_map_limits(Limit, M)};
        false ->
            process_map(M, Rest)
    end.

map_matches(#{} = M, Pattern) when is_function(Pattern) ->
    Pattern(M);
map_matches(_, []) ->
    true;
map_matches(M, [{K, V} | Rest]) ->
    case maps:is_key(K, M) of
        true ->
            case maps:get(K, M) of
                V ->
                    map_matches(M, Rest);
                _ ->
                    false
            end;
        false ->
            false
    end.

apply_map_limits(none, M) ->
    M;
apply_map_limits(all, _) ->
    #{};
apply_map_limits(Fields, M) ->
    maps:with(Fields, M).

patterns_table_name() -> recon_map_patterns.

store_pattern(Label, Pattern, Limit) ->
    ensure_table_exists(),
    ets:insert(patterns_table_name(), {Label, maps:to_list(Pattern), Limit}),
    ok.

ensure_table_exists() ->
    case ets:info(patterns_table_name()) of
        undefined ->
            case whereis(recon_ets_maps) of
                undefined ->
                    Parent = self(),
                    Ref = make_ref(),
                    %% attach to the currently running session
                    {Pid, MonRef} = spawn_monitor(fun() ->
                        register(recon_ets_maps, self()),
                        ets:new(patterns_table_name(), [set, public, named_table]),
                        Parent ! Ref,
                        ets_keeper()
                    end),
                    receive
                        Ref ->
                            erlang:demonitor(MonRef, [flush]),
                            Pid;
                        {'DOWN', MonRef, _, _, Reason} ->
                            error(Reason)
                    end;
                Pid ->
                    Pid
            end;
        Pid ->
            Pid
    end.

ets_keeper() ->
    receive
        stop -> ok;
        _ -> ets_keeper()
    end.

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

