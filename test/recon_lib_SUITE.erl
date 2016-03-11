-module(recon_lib_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [scheduler_usage_diff].

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
