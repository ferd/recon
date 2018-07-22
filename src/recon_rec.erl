%%%-------------------------------------------------------------------
%%% @author bartlomiej.gorny@erlang-solutions.com
%%% @doc
%%% This module handles formatting records for known record types.
%%% Record definitions are imported from modules by user. Definitions are
%%% distinguished by record name and its arity, if you have multiple records
%%% of the same name and size, you have to choose one of them and some of your
%% records may be wrongly labelled. You can manipulate your definition list by
%% using import/1 and clear/1, and check which definitions are in use by executing
%% list/0.
%%% @end
%%%-------------------------------------------------------------------
-module(recon_rec).
-author("bartlomiej.gorny@erlang-solutions.com").
%% API

-export([import/1, format_tuple/1, clear/1, clear/0, list/0, get_list/0, limit/3]).

-export([lookup_record/2]). %% for testing

%% @doc import record definitions from a module. If a record definition of the same name
%% and arity has already been imported from another module then a warning is issued and the new
%% definition is ignored. You have to choose one and possibly remove the old one using
%% clear/1. Supports importing multiple modules at once (by giving a list of atoms as
%% an argument).
%% @end
import(Modules) when is_list(Modules) ->
    lists:foldl(fun import/2, [], Modules);
import(Module) ->
    import(Module, []).

%% @private if a tuple is a known record, formats is as "#recname{field=value}", otherwise returns
%% just a printout of a tuple.
format_tuple(Tuple) ->
    ensure_table_exists(),
    First = element(1, Tuple),
    lists:flatten(format_tuple(First, Tuple)).

%% @doc remove definitions imported from a module.
clear(Module) ->
    lists:map(fun(R) -> rem_for_module(R, Module) end, ets:tab2list(ets_table_name())).

%% @doc remove all imported definitions, destroy the table, clean up
clear() ->
    catch ets:delete_all_objects(ets_table_name()),
    catch whereis(recon_ets) ! stop,
    ok.

%% @doc prints out all "known" (imported) record definitions and their limit settings.
%% Print out tells module a record originates from, its name and a list of field names,
%% plus the record's arity (may be handy if handling big records) and a list of field it
%% limits its output to, if set.
%% @end
list() ->
    F = fun({Module, Name, Fields, Limits}) ->
            Fnames = lists:map(fun atom_to_list/1, Fields),
            Flds = string:join(Fnames, ", "),
            io:format("~p: #~p(~p){~s} ~p~n", [Module, Name, length(Fields), Flds, Limits])
        end,
    lists:foreach(F, get_list()).

get_list() ->
    ensure_table_exists(),
    Lst = lists:map(fun make_list_entry/1, ets:tab2list(ets_table_name())),
    lists:sort(Lst).

%% @doc Limit output to selected fields of a record (set to 'all' to reset).
limit(Name, Arity, all) ->
    limit(Name, Arity, []);
limit(Name, Arity, Field) when is_atom(Field) ->
    limit(Name, Arity, [Field]);
limit(Name, Arity, FieldList) ->
    case lookup_record(Name, Arity) of
        [] ->
            {error, record_unknown};
        [{Key, Fields, Mod, _}] ->
            ets:insert(ets_table_name(), {Key, Fields, Mod, FieldList}),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_list_entry({{Name, _}, Fields, Module, Limits}) ->
    FmtLimit = case Limits of
                   [] -> all;
                   Other -> Other
               end,
    {Module, Name, field_names(Fields), FmtLimit}.

import(Module, ResultList) ->
    ensure_table_exists(),
    lists:foldl(fun(Rec, Res) -> store_record(Rec, Module, Res) end,
                ResultList,
                get_record_defs(Module)).

store_record(Rec, Module, ResultList) ->
    {Name, Fields} = Rec,
    Arity = length(Fields),
    Result = case lookup_record(Name, Arity) of
        [] ->
            ets:insert(ets_table_name(), rec_info(Rec, Module)),
            {imported, Module, Name, Arity};
        [{_, _, Module, _}] ->
            ets:insert(ets_table_name(), rec_info(Rec, Module)),
            {overwritten, Module, Name, Arity};
        [{_, _, Mod, _}] ->
            {ignored, Module, Name, Arity, Mod}
    end,
    [Result | ResultList].

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
            "{" ++ string:join([recon_lib:format_trace_output(El) || El <- List], ", ") ++ "}"
    end;
format_tuple(_, Tuple) ->
    format_default(Tuple).

format_default(Val) ->
    io_lib:format("~p", [Val]).

format_record(Rec, {{Name, Arity}, Fields, _, Limits}) ->
    ExpectedLength = Arity + 1,
    case tuple_size(Rec) of
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
    recon_lib:format_trace_output(Key) ++ "=" ++ recon_lib:format_trace_output(Val).

apply_limits(List, []) -> List;
apply_limits(List, Limits) ->
    lists:filter(fun({K, _}) -> lists:member(K, Limits) end, List) ++ [{more, '...'}].
