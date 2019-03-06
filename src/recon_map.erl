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
-export([patterns_table_name/0]).

-type map_label() :: atom().
-type pattern() :: map().
-type field() :: atom().
-type limit() :: all | none | field() | [field()].

%% @doc Limit output to selected keys of a map (can be 'none', 'all', a key or a list of keys).
%% Pattern selects maps to process.
%% @end
-spec limit(map_label(), pattern(), limit()) -> ok | {error, any()}.
limit(Label, #{} = Pattern, Limit) when is_atom(Label) ->
    store_pattern(Label, Pattern, Limit);
limit(Label, Pattern, Limit) when is_atom(Label), is_function(Pattern) ->
    store_pattern(Label, Pattern, Limit);
limit(_, _, _) ->
    {error, "Bad argument - the spec is limit(atom(), map(), limit())"}.

list() ->
    io:format("~nmap definitions and limits:~n"),
    list(lists:sort(ets:tab2list(patterns_table_name()))).

list([]) ->
    io:format("~n"),
    ok;
list([{Label, Pattern, Limit} | Rest]) ->
    io:format("~p: ~p -> ~p~n", [Label, Pattern, Limit]),
    list(Rest).

%% @doc given a map, scans saved patterns for one that matches; if found, returns a label
%% and a map with limits applied; otherwise returns 'none' and original map
%% pattern can be:
%% - a map - then each key in pattern is check for equality with the map in question
%% - a fun(map()) -> boolean()
-spec process_map(map()) -> {atom(), map()}.
process_map(M) ->
    process_map(M, ets:tab2list(patterns_table_name())).

process_map(M, []) ->
    {none, M};
process_map(M, [{Label, Pattern, Limit} | Rest]) ->
    case map_matches(M, Pattern) of
        true ->
            {Label, apply_map_limits(Limit, M)};
        false ->
            process_map(M, Rest)
    end.

map_matches(#{} = M, Pattern) when is_function(Pattern) ->
    Pattern(M);
map_matches(#{} = M, #{} = Pattern) ->
    map_matches(M, maps:to_list(Pattern));
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
    maps:from_list(lists:foldl(fun(F, Flist) -> get_value_from_map(F, M, Flist) end, [], Fields)).

get_value_from_map(F, M, Flist) ->
    case maps:is_key(F, M) of
        true ->
            [{F, maps:get(F, M)} | Flist];
        false ->
            Flist
    end.

patterns_table_name() -> recon_map_patterns.

store_pattern(Label, Pattern, Limit) ->
    recon_rec:ensure_table_exists(),
    ets:insert(patterns_table_name(), {Label, Pattern, Limit}),
    ok.
