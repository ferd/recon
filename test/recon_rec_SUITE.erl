-module(recon_rec_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).


%%%%%%%%%% SETUP

all() -> [record_defs].

init_per_suite(Config) ->
    true = recon_rec:import(records1),
    Config.

end_per_suite(C) ->
    whereis(recon_ets) ! stop,
    C.

init_per_testcase(_, Config) -> Config.

end_per_testcase(_, Config) ->
    recon_rec:clear(),
    Config.

%%%%%%%%%% TESTS

record_defs(_Config) ->
    has_record(state, 3), % make sure table was not wiped out
    has_record(another, 3),
    false = recon_rec:import(records2), %% one record is a duplicate
    has_record(state, 3),
    has_record(another, 3),
    has_record(another, 4),
    [Res] = recon_rec:lookup_record(state, 3),
    check_first_field(aaa, Res),
    recon_rec:clear(records1),
    no_record(state, 3),
    no_record(another, 3),
    has_record(another, 4),
    true = recon_rec:import(records2),
    [Res1] = recon_rec:lookup_record(state, 3),
    check_first_field(one, Res1),
    recon_rec:clear(),
    no_record(state, 3),
    no_record(another, 3),
    no_record(another, 4),
    ok.

%%%%%%%%%% HELPERS

has_record(Name, Count) ->
    [_] = recon_rec:lookup_record(Name, Count).

no_record(Name, Count) ->
    [] = recon_rec:lookup_record(Name, Count).

check_first_field(F, Rec) ->
    {_, Fields, _, _} = Rec,
    {record_field, _, {atom, _, F}} = hd(Fields).
