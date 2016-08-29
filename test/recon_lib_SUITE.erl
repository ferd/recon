-module(recon_lib_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [scheduler_usage_diff, sublist_top_n].

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
