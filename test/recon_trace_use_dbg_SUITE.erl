%%%-------------------------------------------------------------------
%%% @author 2023, Mathias Green
%%% @doc
%%%   Common Test Suite for recon_trace functionality.
%%%   Tests various scenarios based on the recon_trace documentation.
%%% @end
%%%-------------------------------------------------------------------

-module(recon_trace_use_dbg_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-compile({parse_transform, ms_transform}).

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

-export([
         count_trace_match/3,
         assert_trace_match/2,
         assert_trace_no_match/2
        ]).

%% Test cases
-export([
         dummy_basic_test/1,
         dummy_basic_trace_test/1,
         trace_full_module_test/1,
         trace_one_function_test/1,
         trace_rate_limit_test/1,
         trace_even_arg_test/1,
         trace_iolist_to_binary_with_binary_test/1,
         trace_specific_pid_test/1,
         trace_arity_test/1,
         trace_spec_list_new_procs_only_test/1,
         trace_handle_call_new_and_custom_registry_test/1,
         trace_return_shellfun_test/1,
         trace_return_matchspec_test/1,
         trace_return_shorthand_test/1
        ]).

%%--------------------------------------------------------------------
%% Suite Configuration
%%--------------------------------------------------------------------

%% @doc Returns list of test cases and/or groups to be executed.
all() ->
    [{group, basic_test}, {group, doc_based_test}].

%% @doc Defines the test groups.
groups() ->
    [
     {basic_test, [sequence], [
                               dummy_basic_test
                              ]},
     {doc_based_test, [sequence], [
                                   trace_full_module_test,
                                   trace_one_function_test,
                                   trace_rate_limit_test,
                                   trace_even_arg_test,
                                   trace_iolist_to_binary_with_binary_test,
                                   trace_specific_pid_test,
                                   trace_arity_test,
                                   trace_spec_list_new_procs_only_test,
                                   trace_handle_call_new_and_custom_registry_test,
                                   trace_return_shellfun_test,
                                   trace_return_matchspec_test,
                                   trace_return_shorthand_test
                                  ]
     }
    ].

%%--------------------------------------------------------------------
%% Init and Teardown Functions
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, Pid} = test_statem:start(),
    ct:log("Starting test_statem process with PID: ~p", [Pid]),
    State = test_statem:get_state(),
    ct:log("Init per suite state: ~p", [State]),
    ct:log("Init per suite config: ~p", [Config]),
    Config.


end_per_suite(_Config) ->
    %% Cleanup after all tests run
    test_statem:stop().


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestName, Config) ->
    LogFileName = "test_statem_"++atom_to_list(TestName)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    [{file, {FH, LogFileName}} | Config].

end_per_testcase(_TestName, Config) ->
    {FH, _FileName} = proplists:get_value(file, Config),
    file:close(FH),
    recon_trace:clear(), % Ensure traces are cleared between tests
    proplists:delete(file, Config).

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

assert_trace_match(RegexString, TraceOutput) ->
    ct:log("Asserting match for ~p in output:~n~p", [RegexString, TraceOutput]),
    case re:run(TraceOutput, RegexString, [{capture, none}]) of
        match -> ok;
        nomatch -> ct:fail({regex_did_not_match, RegexString})
    end.
count_trace_match(RegexString, TraceOutput, ExpCnt) ->
    ct:log("Counting if ~p matches for ~p in output:~n~p", [ExpCnt, RegexString, TraceOutput]),

    case re:run(TraceOutput, RegexString, [global]) of
        {match, List} when length(List) == ExpCnt -> ok;
        {match, List} -> ct:fail({wrong_match_count, RegexString, length(List), ExpCnt});
        nomatch -> ct:fail({regex_did_not_match, RegexString});
        _ -> ct:fail({unexpected, RegexString, ExpCnt})

    end.

assert_trace_no_match(RegexString, TraceOutput) ->
    ct:log("Asserting match for ~p in output:~n~p", [RegexString, TraceOutput]),
    case re:run(TraceOutput, RegexString, [{capture, none}]) of
        match -> ct:fail({regex_unexpectedly_matched, RegexString});
        nomatch -> ok
    end.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

dummy_basic_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),

    recon_trace:calls({test_statem, light_state, fun([cast, switch_state, _]) -> anything end}, 10,
                              [{io_server, FH}, {use_dbg, true}, {scope,local}]),

    test_statem:switch_state(),
    S = test_statem:get_state(),
    ct:log("State: ~p", [S]),
    timer:sleep(100),
    {ok, TraceOutput} = file:read_file(FileName),
    assert_trace_match("test_statem:light_state\\(cast, switch_state, #{iterator=>0", TraceOutput),
    ok.

dummy_basic_trace_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),

    MatchSpec = dbg:fun2ms(fun(_) -> return_trace() end),
    recon_trace:calls({test_statem, light_state, MatchSpec}, 10,
                              [{io_server, FH}, {use_dbg, true}, {scope,local}]),

    test_statem:switch_state(),
    S = test_statem:get_state(),
    ct:log("State: ~p", [S]),
    timer:sleep(100),
    {ok, TraceOutput} = file:read_file(FileName),
    assert_trace_match("test_statem:light_state\\(cast, switch_state, #{iterator=>0", TraceOutput),
    ok.



%%======================================================================
%% ---------------------------------------------------------------------------
%% Test cases based on https://ferd.github.io/recon/recon_trace.html#calls/3
%% ---------------------------------------------------------------------------
%%======================================================================

%%======================================================================
%% Documentation: All calls from the queue module, with 10 calls printed at most:
%% recon_trace:calls({queue, '_', '_'}, 10)
%%---
%% Test: All calls from the test_statem module, with 10 calls printed at most.
%%---
%% Function: recon_trace:calls({test_statem, '_', '_'}, 10)
%%======================================================================
trace_full_module_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),
    recon_trace:calls({test_statem, '_', '_'}, 100,
                              [{io_server, FH}, {use_dbg, true}, {scope,local}]),

    timer:sleep(100),
    ok = test_statem:switch_state(),
    _ = test_statem:get_state(),
    timer:sleep(100),
    ok = test_statem:switch_state(),
    timer:sleep(100),
    lists:foreach(fun(_)->test_statem:get_state(), timer:sleep(50) end, lists:seq(1, 7)), % Call get_state multiple times

    {ok, TraceOutput} = file:read_file(FileName),
    %% there are race conditions when test ends, so 
    count_trace_match("test_statem:get_state\\(\\)", TraceOutput,8),
    count_trace_match("test_statem:light_state\\(cast, switch_state, #{iterator=>", TraceOutput,1),
    count_trace_match("test_statem:heavy_state\\(cast, switch_state, #{iterator=>", TraceOutput,1),
    ok.
%%======================================================================
%% Documentation: All calls to lists:seq(A,B), with 100 calls printed at most: recon_trace:calls({lists, seq, 2}, 100)
%%---
%% Test: All calls from the test_statem:get_state module, with 10 calls printed at most.
%%---
%% Function: recon_trace:calls({test_statem, get_state, '_'}, 10)
%%======================================================================
trace_one_function_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),
    recon_trace:calls({test_statem, get_state, 0}, 100,
                              [{io_server, FH}, {use_dbg, true}, {scope,local}]),

    timer:sleep(100),
    ok = test_statem:switch_state(),
    _ = test_statem:get_state(),
    timer:sleep(100),
    ok = test_statem:switch_state(),
    timer:sleep(100),
    lists:foreach(fun(_)->test_statem:get_state(), timer:sleep(50) end, lists:seq(1, 7)), % Call get_state multiple times

    {ok, TraceOutput} = file:read_file(FileName),

    count_trace_match("test_statem:get_state\\(\\)", TraceOutput,8),
    ok.

%%======================================================================
%% Documentation: All calls to lists:seq(A,B), with 100 calls per second at most: recon_trace:calls({lists, seq, 2}, {100, 1000})
%%---
%% Test: All calls to test_statem:heavy_state(A,B), with 1 call per second printed at most:
%%---
%% Function: recon_trace:calls({test_statem, heavy_state, 2}, 10)
%%======================================================================

trace_rate_limit_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),

    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
    end,

    recon_trace:calls({test_statem, heavy_state, 3}, {1, 1000},
                              [{io_server, FH}, {use_dbg, true}, {scope,local}]),

    timer:sleep(2200), % Allow more time for potential rate limiting delays
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(FileName),

    count_trace_match("test_statem:heavy_state", TraceOutput, 2),
    ok.

%%======================================================================
%% Documentation: All calls to lists:seq(A,B,2) (all sequences increasing by two) with 100 calls at most:
%%                recon_trace:calls({lists, seq, fun([_,_,2]) -> ok end}, 100)
%%---
%% Test: All calls to test_statem:heavy_state(A,B) where B is even, with 10 calls at most:
%%---
%% Function:  recon_trace:calls({test_statem, heavy_state, fun([_, B]) when is_integer(B), B rem 2 == 0 -> ok end}, 10)
%%======================================================================

trace_even_arg_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),
    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok

    end,
    MatchSpec = dbg:fun2ms(fun([_,_,#{iterator:=N}]) when N rem 2 == 0 -> return_trace() end),

    recon_trace:calls({test_statem, heavy_state, MatchSpec}, 10,
                              [{io_server, FH}, {use_dbg, true}, {scope,local}]),
    timer:sleep(1900),
    recon_trace:clear(),
    {ok, TraceOutput} = file:read_file(FileName),

    count_trace_match("test_statem:heavy_state\\(timeout", TraceOutput, 3),
    ok.
%%======================================================================
%% Documentation: All calls to iolist_to_binary/1 made with a binary as an argument already (kind of useless conversion!):
%%                recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> ok end}, 10)
%%---
%% Test: All calls to iolist_to_binary/1 made with a binary argument.
%%---
%% Function: recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> ok end}, 10)
%%---
%% NOTE: Maybe there is a way to transform fun_shell directly in recon as in erlang shell
%%======================================================================

trace_iolist_to_binary_with_binary_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),

    MatchSpec = dbg:fun2ms(fun([X]) when is_binary(X) -> return_trace() end),
    recon_trace:calls({erlang, iolist_to_binary, MatchSpec}, 10,
                              [{io_server, FH}, {use_dbg, true}, {scope,local}]),

    _ = erlang:iolist_to_binary(<<"already binary">>), % Should trace
    _ = erlang:iolist_to_binary(["not binary"]),      % Should NOT trace
    _ = erlang:iolist_to_binary([<<"mix">>, "ed"]),   % Should NOT trace
    _ = erlang:iolist_to_binary(<<"another binary">>), % Should trace

    timer:sleep(100),
    {ok, TraceOutput} = file:read_file(FileName),

    recon_trace:clear(),

    assert_trace_match("erlang:iolist_to_binary\\(<<\"already binary\">>\\)", TraceOutput),
    assert_trace_match("erlang:iolist_to_binary\\(<<\"another binary\">>\\)", TraceOutput),
    assert_trace_no_match("erlang:iolist_to_binary\\(\\[\"not binary\"\\]\\)", TraceOutput),
    assert_trace_no_match("erlang:iolist_to_binary\\(\\[<<\"mix\">>,\"ed\"\\]\\)", TraceOutput),
    ok.

%%======================================================================
%% Documentation: Calls to the queue module only in a given process Pid,
%%                at a rate of 50 per second at most: recon_trace:calls({queue, '_', '_'}, {50,1000}, [{pid, Pid}])
%%---
%% Test: Calls to the test_statem module only in the test_statem process Pid, at a rate of 10 per second at most.
%%---
%% Function: recon_trace:calls({test_statem, '_', '_'}, {10,1000}, [{pid, Pid}])
%%======================================================================
trace_specific_pid_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),

    %%default statem state is heavy_state
    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
    end,
    %% new statem in light state
    {ok, Pid} = gen_statem:start(test_statem, [], []),

    recon_trace:calls({test_statem, '_', '_'}, {10,1000},
                              [{pid, Pid},  {io_server, FH}, {use_dbg, true}, {scope,local}]),

    gen_statem:call(Pid, get_value),
    gen_statem:call(Pid, get_value),
    %% Call from another process - should NOT trace
    test_statem:get_state(),
    test_statem:get_state(),
    test_statem:get_state(),
    timer:sleep(100),
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(FileName),
    %% Check calls originating from  are traced (e.g., handle_event)
    count_trace_match(".*test_statem:light_state", TraceOutput,2),
    %% Check calls from the other process are NOT traced
    assert_trace_no_match(".*test_statem:heavy_state", TraceOutput),
    is_process_alive(Pid) andalso exit(Pid, kill), % Cleanup spawned proc
    ok.

%%======================================================================
%% Documentation: Print the traces with the function arity instead of literal arguments:
%%                recon_trace:calls(TSpec, Max, [{args, arity}])
%%---
%% Test: Print traces for test_statem calls with arity instead of arguments.
%%---
%% Function: recon_trace:calls({test_statem, '_', '_'}, 10, [{args, arity}])
%%======================================================================
trace_arity_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),

    recon_trace:calls({test_statem, '_', '_'}, 10,
                              [{args, arity}, {io_server, FH}, {use_dbg, true}, {scope,local}]),

    test_statem:get_state(),
    ok = test_statem:switch_state(),
    ok = test_statem:switch_state(),
    timer:sleep(100),
    recon_trace:clear(),
    {ok, TraceOutput} = file:read_file(FileName),
    %% Check for arity format, e.g., module:function/arity
    assert_trace_match("test_statem:get_state/0", TraceOutput), % gen_statem callback arity
    assert_trace_match("test_statem:light_state/3", TraceOutput), % gen_statem callback arity
    %% Ensure literal args are not present
    assert_trace_no_match("switch_state\\(", TraceOutput),
    assert_trace_no_match("iterator", TraceOutput),
    ok.

%%======================================================================
%% Documentation: Matching the filter/2 functions of both dict and lists modules, across new processes only:
%%                recon_trace:calls([{dict,filter,2},{lists,filter,2}], 10, [{pid, new}])
%%---
%% Test: Matching light_state/2 and heavy_state/2 calls in test_statem across new processes only.
%%---
%% Function: recon_trace:calls([{test_statem, light_state, 2}, {test_statem, heavy_state, 2}], 10, [{pid, new}])
%%======================================================================
trace_spec_list_new_procs_only_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),

    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
    end,

    recon_trace:calls([{test_statem, light_state, '_'}, {test_statem, heavy_state, '_'}], 10,
                              [{pid, new},  {io_server, FH}, {use_dbg, true}, {scope,local}]),

    {ok, heavy_state,_} = test_statem:get_state(),
    %% Call from a *new* process - should trace
    {ok, NewPid} = gen_statem:start(test_statem, [], []),

    gen_statem:call(NewPid, get_value),
    gen_statem:call(NewPid, get_value),

    %% Call from old process - should NOT trace
    test_statem:get_state(),
    test_statem:get_state(),
    test_statem:get_state(),
    timer:sleep(1000),
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(FileName),
    %% Check calls from the new process ARE traced
    count_trace_match("test_statem:light_state", TraceOutput, 2),
    assert_trace_no_match("test_statem:heavy_state", TraceOutput),
    is_process_alive(NewPid) andalso exit(NewPid, kill), % Cleanup spawned proc
    ok.
%%======================================================================
%% Documentation: Tracing the handle_call/3 functions of a given module for all new processes, and those of an existing one registered with gproc:
%% recon_trace:calls({Mod,handle_call,3}, {10,100}, [{pid, [{via, gproc, Name}, new]}])
%%---
%% Test: Tracing test_statem for new processes and one via custom process register.
%%---
%% Function: recon_trace:calls({test_statem, handle_call, 3}, {10, 100}, [{pid, [{via, fake_reg, ts_test}, new]}])
%%======================================================================
trace_handle_call_new_and_custom_registry_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),
    try
        case test_statem:get_state() of
            {ok,light_state,_} -> test_statem:switch_state();
            {ok,heavy_state,_} -> ok
        end,
        fake_reg:start(),
        {ok, NewPid} = gen_statem:start({via, fake_reg, ts_test}, test_statem, [], []),

        recon_trace:calls([{test_statem, light_state, '_'}, {test_statem, heavy_state, '_'}], 10,
                                  [{pid, [{via, fake_reg, ts_test}, new]}, {io_server, FH}, {use_dbg, true}, {scope,local}]),

        gen_statem:call({via, fake_reg, ts_test}, get_value),
        gen_statem:call(NewPid, get_value),

        %% Call from old process - should NOT trace
        test_statem:get_state(),
        test_statem:get_state(),
        test_statem:get_state(),
        timer:sleep(100),
        recon_trace:clear(),

        {ok, TraceOutput} = file:read_file(FileName),
        %% Check calls from the new process ARE traced
        count_trace_match("test_statem:light_state", TraceOutput, 2),
        assert_trace_no_match("test_statem:heavy_state", TraceOutput)
    after
        gen_statem:stop({via, fake_reg, ts_test}),
        fake_reg:stop()
    end.
%%======================================================================
%% Documentation: Show the result of a given function call: recon_trace:calls({Mod,Fun,fun(_) -> return_trace() end}, Max, Opts)
%%---
%% Test: Show the result of test_statem:get_state/0 calls.
%%---
%% Function: recon_trace:calls({test_statem, get_state, fun(_) -> return_trace() end}, 10)
%%---
%% NOTE: Maybe there is a way to transform fun_shell directly in recon as in erlang shell
%%======================================================================
trace_return_shellfun_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),
    case test_statem:get_state() of
        {ok,light_state,_} -> ok;
        {ok,heavy_state,_} -> test_statem:switch_state()
    end,

    MatchSpec = dbg:fun2ms(fun(_) -> return_trace() end),

    recon_trace:calls({test_statem, get_state, MatchSpec}, 10,
                              [{io_server, FH}, use_dbg, {scope,local}]),

    {ok,light_state, N} = test_statem:get_state(),

    timer:sleep(100),
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(FileName),
    %% Check for the call and its return value
    assert_trace_match("test_statem:get_state/0 --> {ok,light_state,"++integer_to_list(N)++"}", TraceOutput),
    ok.
%%======================================================================
%% Documentation: Show the result of a given function call:
%%                recon_trace:calls({Mod,Fun,[{'_', [], [{return_trace}]}]}, Max, Opts),
%%---
%% Test: Show the result of test_statem:get_state/0 calls (using match spec).
%%---
%% Function: recon_trace:calls({test_statem, get_state, [{'_', [], [{return_trace}]}]}, 10)
%%======================================================================
trace_return_matchspec_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),

    case test_statem:get_state() of
        {ok,heavy_state,_} -> ok;
        {ok,light_state,_} -> test_statem:switch_state()
    end,
    %% Trace the API function test_statem:get_state/1 using match spec
    recon_trace:calls({test_statem, get_state,
                               [{'_', [], [{return_trace}]}]}, 10, [ {io_server, FH}, {use_dbg, true}, {scope,local}]),

    {ok,heavy_state, N} = test_statem:get_state(),
    timer:sleep(100),
    recon_trace:clear(),
    {ok, TraceOutput} = file:read_file(FileName),

    %% Check for the call and its return value
    assert_trace_match("test_statem:get_state/0 --> {ok,heavy_state,"++integer_to_list(N)++"}", TraceOutput),
    ok.
%%======================================================================
%% Documentation: A short-hand version for this pattern of 'match anything, trace everything'
%%                for a function is recon_trace:calls({Mod, Fun, return_trace}).
%%---
%% Test: Show the result of test_statem:get_state/0 calls (shorthand).
%%---
%% Function: recon_trace:calls({test_statem, get_state, return_trace}, 10).
%%======================================================================
trace_return_shorthand_test(Config) ->
    {FH, FileName} = proplists:get_value(file, Config),
    case test_statem:get_state() of
        {ok,light_state,_} -> ok;
        {ok,heavy_state,_} -> test_statem:switch_state()
    end,
    %% Trace the API function test_statem:get_state/1 using shorthand
    recon_trace:calls({test_statem, get_state, return_trace}, 10,
                              [{io_server, FH}, {use_dbg, true}, {scope,local}]),
    {ok,light_state, N} = test_statem:get_state(),
    timer:sleep(100),
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(FileName),

    %% Check for the call and its return value
    assert_trace_match("test_statem:get_state/0 --> {ok,light_state,"++integer_to_list(N)++"}", TraceOutput),
    ok.
%%======================================================================
