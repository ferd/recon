-module(recon_lib_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [scheduler_usage_diff, sublist_top_n, term_to_pid].

scheduler_usage_diff(_Config) ->
    {Active0, Total0} = {1000, 2000},
    SchedStat0 = {1, Active0, Total0},
    % No active or total time has elapsed. Make sure we don't divide by zero.
    [{1, 0.0}] = recon_lib:scheduler_usage_diff([SchedStat0], [SchedStat0]),
    % Total time has elapsed, but no active time. Make sure we get 0 usage back.
    SchedStat1 = {1, Active0, Total0 * 2},
    [{1, 0.0}] = recon_lib:scheduler_usage_diff([SchedStat0], [SchedStat1]),
    % Check for 100% usage
    SchedStat2 = {1, Active0 + 1000, Total0 + 1000},
    [{1, 1.0}] = recon_lib:scheduler_usage_diff([SchedStat0], [SchedStat2]).

sublist_top_n(_Config) ->
    L0 = [1,1,2,4,5,6,0,8,7,4,5,2,1,8,agbg,{t},3,[bah],"te",<<"bin">>,23.0, 23],
    L = [{make_ref(), Val, [{meta,data}]} || Val <- L0],
    %% former sort function used prior to integraton of sublist_top_n
    Sorted = lists:usort(fun({_,A,_},{_,B,_}) -> A > B end, L),
    [begin
        Sub = (catch lists:sublist(Sorted, N)),
        ct:pal("Sub ~p: ~p", [N, Sub]),
        Sub = (catch recon_lib:sublist_top_n_attrs(L, N))
     end || N <- lists:seq(0, length(L)+1)],
    ok.

term_to_pid(_Config) ->
    Pid = self(),
    Pid = recon_lib:term_to_pid(Pid),
    List = pid_to_list(Pid),
    Pid = recon_lib:term_to_pid(List),
    Binary = list_to_binary(List),
    Pid = recon_lib:term_to_pid(Binary),
    Name = foo,
    register(Name, Pid),
    Pid = recon_lib:term_to_pid(Name),
    yes = global:register_name(Name, Pid),
    Pid = recon_lib:term_to_pid({global, Name}),
    Sublist = lists:sublist(List, 2, length(List)-2),
    Ints = [ element(1, string:to_integer(T)) || T <- string:tokens(Sublist, ".")],
    Triple = list_to_tuple(Ints),
    Pid = recon_lib:term_to_pid(Triple),
    ok.
