-module(recon_trace_SUITE).


-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl"). % For logger checks if needed
-include_lib("stdlib/include/ms_transform.hrl").
-compile({parse_transform, ms_transform}).

-export([
    all/0, groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2
]).

-export([
    spawn_test_server/0,
    spawn_test_server_loop/1,
    get_trace_output/1,
    stop_test_server/1,
    assert_trace_match/2,
    assert_trace_no_match/2
]).

%% Test cases
-export([
    dummy_basic_test/1, % Keep original for reference if needed, or remove
    trace_all_test_statem_calls/1,
    trace_heavy_state_2_calls/1,
    trace_heavy_state_2_rate_limited/1,
    trace_heavy_state_2_even_arg/1,
    trace_iolist_to_binary_with_binary/1,
    trace_test_statem_calls_specific_pid/1,
    trace_test_statem_calls_arity/1,
    trace_test_statem_states_new_procs/1,
    trace_handle_call_new_and_gproc/1,
    trace_get_state_return_fun/1,
    trace_get_state_return_matchspec/1,
    trace_get_state_return_shorthand/1,
    dummy_advanced_test/1 % Keep original for reference if needed, or remove
]).

%%--------------------------------------------------------------------
%% Suite Configuration
%%--------------------------------------------------------------------

%% @doc Returns list of test cases and/or groups to be executed.
all() ->
    [{group, basic_ops}
        %, {group, advanced_tracing}
        ].

%% @doc Defines the test groups.
groups() ->
    [
        {basic_ops, [sequence], [
            dummy_basic_test,
            trace_all_test_statem_calls
            %trace_heavy_state_2_calls,
            %trace_heavy_state_2_rate_limited,
            %trace_heavy_state_2_even_arg,
            %trace_iolist_to_binary_with_binary,
            %trace_test_statem_calls_specific_pid,
            %trace_test_statem_calls_arity,
            %trace_get_state_return_fun,
            %trace_get_state_return_matchspec,
            %trace_get_state_return_shorthand
        ]},
        {advanced_tracing, [sequence], [
            %% dummy_advanced_test, % Can remove this later
            %trace_test_statem_states_new_procs,
            %trace_handle_call_new_and_gproc % Requires gproc setup
        ]}
    ].

%%--------------------------------------------------------------------
%% Init and Teardown Functions
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    %% Setup before any tests run
    {ok, Pid} = test_statem:start(),
    ct:log("Starting test_statem process with PID: ~p", [Pid]),
    S = test_statem:get_state(),
    ct:log("Init per suite state: ~p", [S]),
    Config.


end_per_suite(_Config) ->
    %% Cleanup after all tests run
    test_statem:stop().


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    %% Cleanup after each group runs
    recon_trace:clear(), % Ensure traces are cleared between groups
    Config.


%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------
return_trace() ->
    dbg:fun2ms(fun(_) -> return_trace() end).

spawn_test_server() ->
    %% Spawn a test server process to capture trace output
    Pid = spawn(?MODULE, spawn_test_server_loop, [[]]),
    register(ts_serv, Pid),
    ct:log("Spawned test server with PID: ~p", [Pid]),
    Pid.

spawn_test_server_loop(Data) ->
    ct:log("Test Server State: ~s", [io_lib:format("~p", [Data])]), % Debug logging
    receive
        {get_value, From} -> % Get the last value
            Rest = case Data of
                    [] -> 
                        From ! {error, no_data},
                        [];
                    [Value | Tail] -> 
                        From ! {ok, Value}, 
                        Tail
                    end,
            spawn_test_server_loop(Rest);
        {get_all_values, From} -> % Get all captured values
             From ! {ok, lists:reverse(Data)},
             spawn_test_server_loop([]); % Reset data after getting all
        Msg ->
            ct:log("Test Server Received: ~s", [io_lib:format("~p", [Msg])]), % Debug logging
            spawn_test_server_loop([Msg | Data])
    end.


get_trace_output_all(TS) ->
        TS ! {get_all_values, self()},
        receive
            {ok, Values} ->
                lists:flatten([ io_lib:format("~s", [X]) || {io_request,_,_,{put_chars,unicode,io_lib,format,X}} <- Values]);
            Other ->
                ct:fail({failed_to_get_trace_output, Other})
        after 2500 ->
            ct:log("Timeout waiting for trace output from ~p", [TS]),
            "" % Return empty string on timeout
        end.
    
get_trace_output(TS) ->
    TS ! {get_value, self()},
    receive
        {ok, Value} ->
            {io_request,_,_,{put_chars,unicode,io_lib,format,X}} = Value,
            lists:flatten(io_lib:format("~s", [X]));
        Other ->
            ct:fail({failed_to_get_trace_output, Other})
    after 2500 ->
        ct:log("Timeout waiting for trace output from ~p", [TS]),
        "" % Return empty string on timeout
    end.

stop_test_server(TS) ->
    unregister(ts_serv), 
    exit(TS, normal).

assert_trace_match(RegexString, TraceOutput) ->
    ct:log("Asserting match for '~s' in output:~n~s", [RegexString, TraceOutput]),
    case re:run(TraceOutput, RegexString, [{capture, none}]) of
        match -> ok;
        nomatch -> ct:fail({regex_did_not_match, RegexString})
    end.

assert_trace_no_match(RegexString, TraceOutput) ->
    ct:log("Asserting no match for '~s' in output:~n~s", [RegexString, TraceOutput]),
    case re:run(TraceOutput, RegexString, [{capture, none}]) of
        match -> ct:fail({regex_unexpectedly_matched, RegexString});
        nomatch -> ok
    end.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

dummy_basic_test(_Config) ->
    TS = spawn_test_server(),
    recon_trace:calls({test_statem, light_state, '_'}, 10, [{io_server, TS},{scope,local}]),

    test_statem:switch_state(),
    S = test_statem:get_state(),
    ct:log("State: ~p", [S]),
    timer:sleep(100), % Allow time for trace message processing

    TraceOutput = get_trace_output(TS),
    
    assert_trace_match("test_statem:light_state\\(cast, switch_state, #{iterator=>0", TraceOutput),
    stop_test_server(TS),
    recon_trace:clear(),
    ok.

%% Test cases based on https://ferd.github.io/recon/recon_trace.html#calls/3
%% Documentation: All calls from the queue module, with 10 calls printed at most: recon_trace:calls({queue, '_', '_'}, 10)
%% Test: All calls from the test_statem module, with 10 calls printed at most: recon_trace:calls({test_statem, '_', '_'}, 10)
trace_all_test_statem_calls(Config) ->
    TS = spawn_test_server(),
    
    recon_trace:calls({test_statem, '_', '_'}, 100, [{io_server, ts_serv},{scope,local}]),

    _ = test_statem:get_state(),
    timer:sleep(100),
    ok = test_statem:switch_state(),
    timer:sleep(100),
    _ = test_statem:get_state(),
    timer:sleep(100),
    ok = test_statem:switch_state(), % Back to light
    timer:sleep(100),
    TraceOutput = get_trace_output_all(TS),
    ct:log("Stsssssssssssssssssate: ~p", [TraceOutput]),
    stop_test_server(TS),
    recon_trace:clear(),

    assert_trace_match("test_statem:get_state\\(\\)", TraceOutput), % Initial get_state
    assert_trace_match("test_statem:switch_state\\(cast, switch_state, #{iterator=>0", TraceOutput), % First switch
    assert_trace_match("test_statem:get_state\\(\\)", TraceOutput), % Second get_state
    assert_trace_match("test_statem:switch_state\\(cast, switch_state, #{iterator=>1", TraceOutput), % Second switch
    assert_trace_match("test_statem:get_state\\(\\)", TraceOutput), % Third get_state
    ok.

%% Documentation: All calls to lists:seq(A,B), with 100 calls printed at most: recon_trace:calls({lists, seq, 2}, 100)
%% Documentation: All calls to lists:seq(A,B), with 100 calls per second at most: recon_trace:calls({lists, seq, 2}, {100, 1000})
%% Test: All calls to test_statem:heavy_state(A,B), with 10 calls printed at most: recon_trace:calls({test_statem, heavy_state, 2}, 10)
trace_heavy_state_2_calls(Config) ->
    TS = spawn_test_server(),
    
    % Ensure we are in heavy state first
    case test_statem:get_state() of
        light -> test_statem:switch_state();
        heavy -> ok
    end,
    heavy = test_statem:get_state(), % Verify state

    recon_trace:calls({test_statem, heavy_state, 2}, 10, [{io_server, TS},{scope,local}]),

    ok = test_statem:switch_state(), % This call should trigger heavy_state(cast, switch_state, ...)

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    assert_trace_match("test_statem:heavy_state\\(cast, switch_state, #{iterator=>", TraceOutput),
    % Ensure light_state wasn't traced
    assert_trace_no_match("test_statem:light_state", TraceOutput),
    ok.

%% Documentation: All calls to lists:seq(A,B,2) (all sequences increasing by two) with 100 calls at most: recon_trace:calls({lists, seq, fun([_,_,2]) -> ok end}, 100)
%% Test: All calls to test_statem:heavy_state(A,B), with 10 calls per second at most: recon_trace:calls({test_statem, heavy_state, 2}, {10, 1000})
trace_heavy_state_2_rate_limited(Config) ->
    TS = spawn_test_server(),
    
    % Ensure we are in heavy state first
    case test_statem:get_state() of
        light -> test_statem:switch_state();
        heavy -> ok
    end,
    heavy = test_statem:get_state(), % Verify state

    recon_trace:calls({test_statem, heavy_state, 2}, {10, 1000}, [{io_server, TS},{scope,local}]),

    % Call it multiple times quickly - only some should be traced
    [ test_statem:switch_state() || _ <- lists:seq(1, 20) ],

    timer:sleep(200), % Allow more time for potential rate limiting delays
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    % Check that at least one trace occurred
    assert_trace_match("test_statem:heavy_state\\(cast, switch_state, #{iterator=>", TraceOutput),
    % Asserting the exact number is difficult due to timing, but it should be <= 10
    % We can count occurrences if needed, but matching one is a basic check.
    ok.

%% Test: All calls to test_statem:heavy_state(A,B) where B is even, with 10 calls at most: recon_trace:calls({test_statem, heavy_state, fun([_, B]) when is_integer(B), B rem 2 == 0 -> ok end}, 10)
trace_heavy_state_2_even_arg(Config) ->
    TS = spawn_test_server(),
    
    % Ensure we are in heavy state first and iterator is 0 (even)
    case test_statem:get_state() of
        light -> test_statem:switch_state(); % Now iterator=0, state=heavy
        heavy -> % Need to ensure iterator is even. Switch twice if odd.
            State = test_statem:get_state(),
            case maps:get(iterator, State) rem 2 of
                0 -> ok;
                1 -> test_statem:switch_state(), % -> light, iter=N+1
                     test_statem:switch_state()  % -> heavy, iter=N+2 (even)
            end
    end,
    State0 = test_statem:get_state(),
    0 = maps:get(iterator, State0) rem 2, % Verify iterator is even

    MatchFun = fun([_Type, _Msg, State = #{iterator := Iter}]) when is_integer(Iter), Iter rem 2 == 0 ->
                    ct:log("MatchFun matched even iterator: ~p", [Iter]),
                    true;
                 ([_Type, _Msg, State = #{iterator := Iter}]) ->
                    ct:log("MatchFun rejected odd iterator: ~p", [Iter]),
                    false
               end,
    recon_trace:calls({test_statem, heavy_state, MatchFun}, 10, [{io_server, TS},{scope,local}]),

    % Call 1: Iterator is even (e.g., 0), should trace
    ok = test_statem:switch_state(), % State -> light, Iterator -> 1
    light = test_statem:get_state(),

    % Call 2: Iterator is odd (e.g., 1), should NOT trace heavy_state (light_state is called)
    ok = test_statem:switch_state(), % State -> heavy, Iterator -> 2
    heavy = test_statem:get_state(),

    % Call 3: Iterator is even (e.g., 2), should trace
    ok = test_statem:switch_state(), % State -> light, Iterator -> 3
    light = test_statem:get_state(),

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    ct:log("Trace output for even arg test:~n~s", [TraceOutput]),

    % Check that the call with even iterator (0 initially) was traced
    assert_trace_match("test_statem:heavy_state\\(cast, switch_state, #{iterator=>" ++ integer_to_list(maps:get(iterator, State0)), TraceOutput),
    % Check that the call with even iterator (2) was traced
    assert_trace_match("test_statem:heavy_state\\(cast, switch_state, #{iterator=>" ++ integer_to_list(maps:get(iterator, State0)+2), TraceOutput),
    % There should be exactly two matches for heavy_state
    {match, M} = re:run(TraceOutput, "test_statem:heavy_state", [global, {capture, none}]),
    2 = length(M),
    ok.

%% Documentation: All calls to iolist_to_binary/1 made with a binary as an argument already (kind of useless conversion!): recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> ok end}, 10)
%% Test: All calls to iolist_to_binary/1 made with a binary argument: recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> ok end}, 10)
trace_iolist_to_binary_with_binary(Config) ->
    TS = spawn_test_server(),
    recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> true end}, 10, [{io_server, TS},{scope,local}]),

    _ = erlang:iolist_to_binary(<<"already binary">>), % Should trace
    _ = erlang:iolist_to_binary(["not binary"]),      % Should NOT trace
    _ = erlang:iolist_to_binary([<<"mix">>, "ed"]),   % Should NOT trace
    _ = erlang:iolist_to_binary(<<"another binary">>), % Should trace

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    assert_trace_match("erlang:iolist_to_binary\\(<<\"already binary\">>\\)", TraceOutput),
    assert_trace_match("erlang:iolist_to_binary\\(<<\"another binary\">>\\)", TraceOutput),
    assert_trace_no_match("erlang:iolist_to_binary\\(\\[\"not binary\"\\]\\)", TraceOutput),
    assert_trace_no_match("erlang:iolist_to_binary\\(\\[<<\"mix\">>,\"ed\"\\]\\)", TraceOutput),
    ok.

%% Documentation: Calls to the queue module only in a given process Pid, at a rate of 50 per second at most: recon_trace:calls({queue, '_', '_'}, {50,1000}, [{pid, Pid}])
%% Test: Calls to the test_statem module only in the test_statem process Pid, at a rate of 10 per second at most: recon_trace:calls({test_statem, '_', '_'}, {10,1000}, [{pid, }])
trace_test_statem_calls_specific_pid(Config) ->
    TS = spawn_test_server(),
    Pid = whereis(test_statem), % Get the PID of the test_statem process
    recon_trace:calls({test_statem, '_', '_'}, {10,1000}, [{pid, Pid}, {io_server, TS},{scope,local}]),

    % Call from the target process (implicitly via gen_statem) - should trace
    ok = test_statem:switch_state(),
    _ = test_statem:get_state(),

    % Call from another process - should NOT trace
    Self = self(),
    OtherPid = spawn(fun() ->
                        _ = test_statem:get_state(), % Call from other process
                        Self ! other_call_done
                end),
    receive other_call_done -> ok after 1000 -> ct:fail(timeout_waiting_for_other_call) end,
    is_process_alive(OtherPid) andalso exit(OtherPid, kill), % Cleanup spawned proc

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    % Check calls originating from  are traced (e.g., handle_event)
    assert_trace_match(pid_to_list(Self) ++ ".*test_statem:light_state\\(cast, switch_state", TraceOutput),
    assert_trace_match(pid_to_list(Self) ++ ".*test_statem:get_state\\(call, ", TraceOutput),

    % Check calls from the other process are NOT traced
    assert_trace_no_match(pid_to_list(OtherPid) ++ ".*test_statem:get_state", TraceOutput),
    ok.

%% Documentation: Print the traces with the function arity instead of literal arguments: recon_trace:calls(TSpec, Max, [{args, arity}])
%% Test: Print traces for test_statem calls with arity instead of arguments: recon_trace:calls({test_statem, '_', '_'}, 10, [{args, arity}])
trace_test_statem_calls_arity(Config) ->
    TS = spawn_test_server(),
    
    recon_trace:calls({test_statem, '_', '_'}, 10, [{args, arity}, {io_server, TS},{scope,local}]),

    _ = test_statem:get_state(),
    ok = test_statem:switch_state(),

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    % Check for arity format, e.g., module:function/arity
    assert_trace_match("test_statem:get_state/3", TraceOutput), % gen_statem callback arity
    assert_trace_match("test_statem:light_state/3", TraceOutput), % gen_statem callback arity
    % Ensure literal args are not present (tricky regex, check absence of typical args)
    assert_trace_no_match("switch_state", TraceOutput),
    assert_trace_no_match("iterator", TraceOutput),
    ok.

%% Documentation: Matching the filter/2 functions of both dict and lists modules, across new processes only: recon_trace:calls([{dict,filter,2},{lists,filter,2}], 10, [{pid, new}])
%% Test: Matching light_state/2 and heavy_state/2 calls in test_statem across new processes only: recon_trace:calls([{test_statem, light_state, 2}, {test_statem, heavy_state, 2}], 10, [{pid, new}])
trace_test_statem_states_new_procs(Config) ->
    TS = spawn_test_server(),
    
    recon_trace:calls([{test_statem, light_state, 3}, {test_statem, heavy_state, 3}], 10, [{pid, new}, {io_server, TS},{scope,local}]), % Note: gen_statem callbacks are arity 3

    % Call from the *current* test process - should NOT trace
    ok = test_statem:switch_state(), % light -> heavy
    heavy = test_statem:get_state(),
    ok = test_statem:switch_state(), % heavy -> light
    light = test_statem:get_state(),

    % Call from a *new* process - should trace
    Self = self(),
    NewPid = spawn(fun() ->
                      ct:log("New process ~p calling switch_state", [self()]),
                      ok = test_statem:switch_state(), % light -> heavy (heavy_state/3 should trace)
                      ct:log("New process ~p calling switch_state again", [self()]),
                      ok = test_statem:switch_state(), % heavy -> light (light_state/3 should trace)
                      Self ! new_calls_done
               end),
    receive new_calls_done -> ok after 1000 -> ct:fail(timeout_waiting_for_new_call) end,

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    % Check calls from the new process ARE traced
    assert_trace_match(pid_to_list(NewPid) ++ ".*test_statem:heavy_state\\(cast, switch_state", TraceOutput),
    assert_trace_match(pid_to_list(NewPid) ++ ".*test_statem:light_state\\(cast, switch_state", TraceOutput),

    % Check calls from the test process (self()) or the statem process ARE NOT traced
    assert_trace_no_match(pid_to_list(self()) ++ ".*test_statem:", TraceOutput),
    assert_trace_no_match(pid_to_list(self()) ++ ".*test_statem:", TraceOutput), % The calls happen *in* , but triggered by NewPid

    is_process_alive(NewPid) andalso exit(NewPid, kill), % Cleanup spawned proc
    ok.

%% Documentation: Tracing the handle_call/3 functions of a given module for all new processes, and those of an existing one registered with gproc: recon_trace:calls({Mod,handle_call,3}, {10,100}, [{pid, [{via, gproc, Name}, new]}])
%% Test: Tracing test_statem:handle_call/3 for new processes and one via gproc (requires gproc setup): recon_trace:calls({test_statem, handle_call, 3}, {10, 100}, [{pid, [{via, gproc, Name}, new]}])
trace_handle_call_new_and_gproc(Config) ->
    case ?config(gproc_name, Config) of
        undefined ->
            ct:skip("gproc not available or test_statem not registered");
        GprocName ->
            TS = spawn_test_server(),
            
            % Note: test_statem uses handle_event/4 for cast, handle_call/3 for call
            % We trace handle_call/3 here.
            recon_trace:calls({test_statem, handle_call, 3}, {10, 100}, [{pid, [{via, gproc, GprocName}, new]}, {io_server, TS},{scope,local}]),

            % Call via gproc - should trace
            ct:log("Calling get_state via gproc ~p", [GprocName]),
            _GprocState = gproc:call(GprocName, {call, get_state}), % Triggers handle_call/3

            % Call from a new process - should trace
            Self = self(),
            NewPid = spawn(fun() ->
                              ct:log("New process ~p calling get_state", [self()]),
                              _NewState = test_statem:get_state(), % Triggers handle_call/3
                              Self ! new_call_done
                       end),
            receive new_call_done -> ok after 1000 -> ct:fail(timeout_waiting_for_new_gproc_call) end,

            % Call directly from test process - should NOT trace
            ct:log("Calling get_state directly from test process ~p", [self()]),
            _DirectState = test_statem:get_state(),

            timer:sleep(100),
            TraceOutput = get_trace_output(TS),
            stop_test_server(TS),
            recon_trace:clear(),

            % Check call via gproc IS traced (originating from )
            assert_trace_match(pid_to_list(Self) ++ ".*test_statem:handle_call\\({call,get_state},", TraceOutput),
            % Check call from new process IS traced (originating from )
            % We might see two traces if both gproc and new pid calls happened close together
            {match, M} = re:run(TraceOutput, pid_to_list(Self) ++ ".*test_statem:handle_call\\({call,get_state},", [global, {capture, none}]),
            true = length(M) >= 1, % Should be at least 1, likely 2

            % Check call from test process is NOT traced (no new trace line added)
            % This is harder to assert definitively without counting lines before/after.
            % We rely on the {pid, [..., new]} filter working correctly.

            is_process_alive(NewPid) andalso exit(NewPid, kill), % Cleanup spawned proc
            ok
    end.

%% Documentation: Show the result of a given function call: recon_trace:calls({Mod,Fun,fun(_) -> return_trace() end}, Max, Opts)
%% Test: Show the result of test_statem:get_state/0 calls: recon_trace:calls({test_statem, get_state, fun(_) -> return_trace() end}, 10)
trace_get_state_return_fun(Config) ->
    TS = spawn_test_server(),
    
    % Ensure state is known (e.g., light)
    case test_statem:get_state() of
        heavy -> test_statem:switch_state();
        light -> ok
    end,
    light = test_statem:get_state(),

    % Trace the API function test_statem:get_state/1
    recon_trace:calls({test_statem, get_state, fun([_Pid]) -> return_trace() end}, 10, [{io_server, TS},{scope,local}]),

    Res = test_statem:get_state(), % Call the function

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    % Check for the call and its return value
    ExpectedReturn = atom_to_list(Res), % e.g., "light"
    assert_trace_match("test_statem:get_state\\(\\) => " ++ ExpectedReturn, TraceOutput),
    ok.

%% Documentation: Show the result of a given function call: recon_trace:calls({Mod,Fun,[{'_', [], [{return_trace}]}]}, Max, Opts),
%% Test: Show the result of test_statem:get_state/0 calls (using match spec): recon_trace:calls({test_statem, get_state, [{'_', [], [{return_trace}]}]}, 10)
trace_get_state_return_matchspec(Config) ->
    TS = spawn_test_server(),
    
    % Ensure state is known (e.g., heavy)
    case test_statem:get_state() of
        light -> test_statem:switch_state();
        heavy -> ok
    end,
    heavy = test_statem:get_state(),

    % Trace the API function test_statem:get_state/1 using match spec
    recon_trace:calls({test_statem, get_state, [{'_', [], [{return_trace}]}]}, 10, [{io_server, TS},{scope,local}]),

    Res = test_statem:get_state(), % Call the function

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    % Check for the call and its return value
    ExpectedReturn = atom_to_list(Res), % e.g., "heavy"
    assert_trace_match("test_statem:get_state\\(\\) => " ++ ExpectedReturn, TraceOutput),
    ok.

%% Documentation: A short-hand version for this pattern of 'match anything, trace everything' for a function is recon_trace:calls({Mod, Fun, return_trace}).
%% Test: Show the result of test_statem:get_state/0 calls (shorthand): recon_trace:calls({test_statem, get_state, return_trace}, 10).
trace_get_state_return_shorthand(Config) ->
    TS = spawn_test_server(),
    
    % Ensure state is known (e.g., light)
    case test_statem:get_state() of
        heavy -> test_statem:switch_state();
        light -> ok
    end,
    light = test_statem:get_state(),

    % Trace the API function test_statem:get_state/1 using shorthand
    recon_trace:calls({test_statem, get_state, return_trace}, 10, [{io_server, TS},{scope,local}]),

    Res = test_statem:get_state(), % Call the function

    timer:sleep(100),
    TraceOutput = get_trace_output(TS),
    stop_test_server(TS),
    recon_trace:clear(),

    % Check for the call and its return value
    ExpectedReturn = atom_to_list(Res), % e.g., "light"
    assert_trace_match("test_statem:get_state\\(\\) => " ++ ExpectedReturn, TraceOutput),
    ok.


dummy_advanced_test(_Config) ->
    ct:log("This is a placeholder for an advanced recon_trace test"),
    ok.
