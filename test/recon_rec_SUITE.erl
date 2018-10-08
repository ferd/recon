-module(recon_rec_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).


%%%%%%%%%% SETUP

all() -> [record_defs, lists_and_limits].

init_per_testcase(_, Config) ->
    Res = recon_rec:import(records1),
    [{imported, records1, another, 3}, {imported, records1, state, 3}] = lists:sort(Res),
    Config.

end_per_testcase(_, Config) ->
    recon_rec:clear(),
    Config.

%%%%%%%%%% TESTS

record_defs(_Config) ->
    has_record(state, 3), % make sure table was not wiped out
    has_record(another, 3),
    ImportRes = recon_rec:import(records2), %% one record is a duplicate
    [{imported,records2,another,4}, {ignored,records2,state,3,records1}] = lists:sort(ImportRes),
    has_record(state, 3),
    has_record(another, 3),
    has_record(another, 4),
    [Res] = recon_rec:lookup_record(state, 3),
    check_first_field(aaa, Res),
    recon_rec:clear(records1),
    no_record(state, 3),
    no_record(another, 3),
    has_record(another, 4),
    ImportRes2 = recon_rec:import(records2),
    [{imported,records2,state,3}, {overwritten,records2,another,4}] = lists:sort(ImportRes2),
    [Res1] = recon_rec:lookup_record(state, 3),
    check_first_field(one, Res1),
    recon_rec:clear(),
    no_record(state, 3),
    no_record(another, 3),
    no_record(another, 4),
    ok.

lists_and_limits(_Config) ->
    recon_rec:import(records1),
    recon_rec:import(records2),
    List = recon_rec:get_list(),
    [{records1,another,[ddd,eee,fff],none},
     {records1,state,[aaa,bbb,ccc],none},
     {records2,another,[one,two,three,four],none}] = List,
    recon_rec:limit(another, 3, ddd),
    {records1,another,[ddd,eee,fff], ddd} = hd(recon_rec:get_list()),
    recon_rec:limit(another, 3, [ddd, eee]),
    {records1,another,[ddd,eee,fff], [ddd, eee]} = hd(recon_rec:get_list()),
    recon_rec:limit(another, 3, all),
    {records1,another,[ddd,eee,fff], all} = hd(recon_rec:get_list()),
    recon_rec:clear(records2),
    {error, record_unknown} = recon_rec:limit(another, 4, none),
    ok.

%%%%%%%%%% HELPERS

has_record(Name, Count) ->
    [_] = recon_rec:lookup_record(Name, Count).

no_record(Name, Count) ->
    [] = recon_rec:lookup_record(Name, Count).

check_first_field(F, Rec) ->
    {_, Fields, _, _} = Rec,
    F = hd(Fields).
