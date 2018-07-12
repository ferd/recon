%%%-------------------------------------------------------------------
%%% @author bartlomiej.gorny@erlang-solutions.com
%%% @doc
%%% This module handles formatting records for known record types.
%%% Record definitions are imported from modules by user. Definitions are
%%% distinguished by record name and its arity, if you have multiple records
%%% of the same name and size, you must be careful.
%%% @end
%%%-------------------------------------------------------------------
-module(recon_rec).
-author("bartlomiej.gorny@erlang-solutions.com").
%% API

-export([import/1, format_tuple/1, clear/1, clear/0, list/0, limit/3]).

-export([lookup_record/2]). %% for testing

%% @doc import record definitions from a module. If a record definition of the same name
%% and arity has already been imported from another module then a warning is issued and the new
%% definition is ignored. You have to choose one and possibly remove the old one using
%% clear/1. Supports importing multiple modules at once (by giving a list of atoms as
%% an argument).
%% @end
import(Modules) when is_list(Modules) ->
    lists:map(fun import/1, Modules);
import(Module) ->
    ensure_table_exists(),
    Res = lists:map(fun(Rec) -> store_record(Rec, Module) end,
                    get_record_defs(Module)),
    lists:all(fun(I) -> I == ok end, Res).

%% @private if a tuple is a known record, formats is as "#recname{field=value}", otherwise returns
%% just a printout of a tuple.
format_tuple(Tuple) ->
    ensure_table_exists(),
    First = element(1, Tuple),
    lists:flatten(format_tuple(First, Tuple)).

%% @doc remove definitions imported from a module.
clear(Module) ->
    lists:map(fun(R) -> rem_for_module(R, Module) end, ets:tab2list(ets_table_name())).

%% @doc remove all imported definitions
clear() ->
    catch ets:delete_all_objects(ets_table_name()).

%% @doc prints out all "known" (imported) record definitions and their limit settings.
%% Print out tells module a record originates from, its name and a list of field names,
%% plus the record's arity (may be handy if handling big records) and a list of field it
%% limits its output to, if set.
%% @end
list() ->
    ensure_table_exists(),
    FmtLimit = fun([]) -> all; (List) -> List end,
    F = fun({Module, Name, Fields, Limits}) ->
            Fnames = lists:map(fun atom_to_list/1, field_names(Fields)),
            Flds = string:join(Fnames, ", "),
            io:format("~p: #~p{~s} (~p) ~p~n", [Module, Name, Flds, length(Fields), FmtLimit(Limits)])
        end,
    Lst = [{Module, Name, Fields, Limits} || {{Name, _}, Fields, Module, Limits} <- ets:tab2list(ets_table_name())],
    lists:foreach(F, lists:sort(Lst)).

%% @doc Limit output to selected fields of a record (set to 'all' to reset).
limit(Name, Arity, all) ->
    limit(Name, Arity, []);
limit(Name, Arity, Field) when is_atom(Field) ->
    limit(Name, Arity, [Field]);
limit(Name, Arity, FieldList) ->
    case lookup_record(Name, Arity) of
        [] ->
            io:format("~nRecord ~p/~p not imported~n~n", [Name, Arity]),
            {error, record_unknown};
        [{Key, Fields, Mod, _}] ->
            ets:insert(ets_table_name(), {Key, Fields, Mod, FieldList}),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_record(Rec, Module) ->
    {Name, Fields} = Rec,
    Arity = length(Fields),
    case lookup_record(Name, Arity) of
        [] ->
            io:format("importing ~p:~p/~p~n", [Module, Name, Arity]),
            ets:insert(ets_table_name(), rec_info(Rec, Module)),
            ok;
        [{_, _, Module, _}] ->
            io:format("importing ~p:~p/~p~n", [Module, Name, Arity]),
            ets:insert(ets_table_name(), rec_info(Rec, Module)),
            ok;
        [{_, _, Mod, _}] ->
            io:format("~nWARNING: record ~p/~p already present (imported from ~p), ignoring~n~n",
                      [Name, Arity, Mod]),
            failed
    end.

get_record_defs(Module) ->
    Path = code:which(Module),
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Path, [abstract_code]),
    lists:foldl(fun get_record/2, [], AC).

get_record({attribute, _, record, Rec}, Acc) -> [Rec | Acc];
get_record(_, Acc) -> Acc.

%% @private
lookup_record(RecName, FieldCount) ->
    ensure_table_exists(),
    ets:lookup(ets_table_name(), {RecName, FieldCount}).

ensure_table_exists() ->
    case ets:info(ets_table_name()) of
        undefined ->
            Pid = case whereis(recon_ets) of
                      undefined ->
                          P = spawn(fun() -> ets_keeper() end),
                          register(recon_ets, P),
                          P;
                      P -> P
                  end,
            ets:new(ets_table_name(), [set, public, named_table]),
            ets:give_away(ets_table_name(), Pid, none);
        _ -> ok
    end.

ets_table_name() -> recon_record_definitions.

rec_info({Name, Fields}, Module) ->
    {{Name, length(Fields)}, Fields, Module, []}.

rem_for_module({_, _, Module, _} = Rec, Module) ->
    ets:delete_object(ets_table_name(), Rec);
rem_for_module(_, _) -> ok.

ets_keeper() ->
    receive
        stop -> ok;
        _ -> ets_keeper()
    end.

field_names(Fields) ->
    lists:map(fun field_name/1, Fields).

field_name({record_field, _, {atom, _, Name}}) -> Name;
field_name({record_field, _, {atom, _, Name}, _Default}) -> Name;
field_name({typed_record_field, Field, _Type}) -> field_name(Field).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FORMATTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_tuple(Name, Rec) when is_atom(Name) ->
    case lookup_record(Name, size(Rec) - 1) of
        [RecDef] -> format_record(Rec, RecDef);
        _ ->
            List = tuple_to_list(Rec),
            "{" ++ string:join([recon_lib:format(El) || El <- List], ", ") ++ "}"
    end;
format_tuple(_, Tuple) ->
    format_default(Tuple).

format_default(Val) ->
    io_lib:format("~p", [Val]).

format_record(Rec, {{Name, _}, Fields, _, Limits}) ->
    ExpectedLength = length(Fields) + 1,
    case size(Rec) of
        ExpectedLength ->
            [_ | Values] = tuple_to_list(Rec),
            FieldNames = field_names(Fields),
            List = lists:zip(FieldNames, Values),
            LimitedList = apply_limits(List, Limits),
            "#" ++ atom_to_list(Name) ++ "{"
                ++ string:join([format_kv(Key, Val) || {Key, Val} <- LimitedList], ", ") ++ "}";
        _ ->
            format_default(Rec)
    end.

format_kv(Key, Val) ->
    recon_lib:format(Key) ++ "=" ++ recon_lib:format(Val).

apply_limits(List, []) -> List;
apply_limits(List, Limits) ->
    lists:filter(fun({K, _}) -> lists:member(K, Limits) end, List) ++ [{more, '...'}].
