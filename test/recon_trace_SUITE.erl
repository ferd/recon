-module(recon_trace_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-compile({parse_transform, ms_transform}).

-export([
    all/0, groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2
]).

-export([
    count_trace_match/3,
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
    trace_handle_call_new_and_custom_registry/1,
    trace_get_state_return_fun/1,
    trace_get_state_return_matchspec/1,
    trace_get_state_return_shorthand/1
]).

%%--------------------------------------------------------------------
%% Suite Configuration
%%--------------------------------------------------------------------

%% @doc Returns list of test cases and/or groups to be executed.
all() ->
    [{group, basic_ops}].

%% @doc Defines the test groups.
groups() ->
    [
        {basic_ops, [sequence], [
            dummy_basic_test,
            trace_all_test_statem_calls,
            trace_heavy_state_2_calls,
            trace_heavy_state_2_rate_limited,
            trace_heavy_state_2_even_arg,
            trace_iolist_to_binary_with_binary,
            trace_test_statem_calls_specific_pid,
            trace_test_statem_states_new_procs,
            trace_handle_call_new_and_custom_registry,
            trace_test_statem_calls_arity,
            trace_get_state_return_fun,
            trace_get_state_return_matchspec,
            trace_get_state_return_shorthand
        ]}
    ].

%%--------------------------------------------------------------------
%% Init and Teardown Functions
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, Pid} = test_statem:start(),
    ct:log("Starting test_statem process with PID: ~p", [Pid]),
    S = test_statem:get_state(),
    ct:log("Init per suite state: ~p", [S]),
    Config.


end_per_suite(__Config) ->
    %% Cleanup after all tests run
    test_statem:stop().


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    %% Cleanup after each group runs
    recon_trace:clear(), % Ensure traces are cleared between groups
    Config.
init_per_testcase(_TestName, Config) ->
    %% Cleanup before each test runs
    recon_trace:clear(), % Ensure traces are cleared between tests
    Config.

end_per_testcase(_TestName, Config) ->
    %% Cleanup after each test runs
    recon_trace:clear(), % Ensure traces are cleared between tests
    Config.

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

dummy_basic_test(_Config) ->
    LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try       
        recon_trace:calls({test_statem, light_state, '_'}, 10, [{io_server, FH},{scope,local}]),
        test_statem:switch_state(),
        S = test_statem:get_state(),
        ct:log("State: ~p", [S]),
        timer:sleep(100), % Allow time for trace message processing

        {ok, TraceOutput} = file:read_file(LogFileName),
        assert_trace_match("test_statem:light_state\\(cast, switch_state, #{iterator=>0", TraceOutput)
     after
        file:close(FH)
    end,
    
    ok.

%% Test cases based on https://ferd.github.io/recon/recon_trace.html#calls/3
%% Documentation: All calls from the queue module, with 10 calls printed at most: recon_trace:calls({queue, '_', '_'}, 10)
%% Test: All calls from the test_statem module, with 10 calls printed at most: recon_trace:calls({test_statem, '_', '_'}, 10)
trace_all_test_statem_calls(_Config) ->
    LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try       
    recon_trace:calls({test_statem, '_', '_'}, 100, [{io_server, FH},{scope,local}]),

    timer:sleep(100),
    ok = test_statem:switch_state(),
    _ = test_statem:get_state(),
    timer:sleep(100),
    ok = test_statem:switch_state(), % Back to light
    timer:sleep(100),
    lists:foreach(fun(_)->test_statem:get_state(), timer:sleep(50) end, lists:seq(1, 7)), % Call get_state multiple times
    
    {ok, TraceOutput} = file:read_file(LogFileName),

    count_trace_match("test_statem:get_state\\(\\)", TraceOutput,8), % Initial get_state
    count_trace_match("test_statem:light_state\\(cast, switch_state, #{iterator=>", TraceOutput,1), % First switch
    count_trace_match("test_statem:heavy_state\\(cast, switch_state, #{iterator=>", TraceOutput,1) % Second switch


         after
       file:close(FH)
    end,
    
    ok.

%% Documentation: All calls to lists:seq(A,B), with 100 calls per second at most: recon_trace:calls({lists, seq, 2}, {100, 1000})
%% Test: All calls to test_statem:heavy_state(A,B), with 3 calls printed at most: recon_trace:calls({test_statem, heavy_state, 2}, 10)
trace_heavy_state_2_calls(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    
    % Ensure we are in heavy state first
    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
    end,
    
    recon_trace:calls({test_statem, heavy_state, 3}, 3, [ {io_server, FH},{scope,local}]),

    timer:sleep(2000),
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(LogFileName),
    
   
    count_trace_match("test_statem:heavy_state", TraceOutput, 3),
    %% Ensure light_state wasn't traced
    assert_trace_no_match("test_statem:light_state", TraceOutput)

    after
      file:close(FH)
    end,
    
    ok.

%% Documentation: All calls to lists:seq(A,B,2) (all sequences increasing by two) with 100 calls at most: recon_trace:calls({lists, seq, fun([_,_,2]) -> ok end}, 100)
%% Test: All calls to test_statem:heavy_state, with 1 calls per second at most: recon_trace:calls({test_statem, heavy_state, 3}, {1, 1000})
trace_heavy_state_2_rate_limited(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    

    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
    end,
   
    recon_trace:calls({test_statem, heavy_state, 3}, {1, 1000}, [ {io_server, FH},{scope,local}]),
    
    timer:sleep(2200), % Allow more time for potential rate limiting delays
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(LogFileName),
    
    
    % Check that at least one trace occurred
    count_trace_match("test_statem:heavy_state", TraceOutput, 2)
    % Asserting the exact number is difficult due to timing, but it should be <= 10
    % We can count occurrences if needed, but matching one is a basic check.
         after
   
    file:close(FH)
    end,
    
    ok.

%% Test: All calls to test_statem:heavy_state(A,B) where B is even, with 10 calls at most: recon_trace:calls({test_statem, heavy_state, fun([_, B]) when is_integer(B), B rem 2 == 0 -> ok end}, 10)
trace_heavy_state_2_even_arg(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    
    % Ensure we are in heavy state first and iterator is 0 (even)
    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
  
    end,
    MatchSpec = dbg:fun2ms(fun([_,_,#{iterator:=N}]) when N rem 2 == 0 -> return_trace() end),

    recon_trace:calls({test_statem, heavy_state, MatchSpec}, 10, [{io_server, FH},{scope,local}]),
      
           
    timer:sleep(1900),
    recon_trace:clear(),
    {ok, TraceOutput} = file:read_file(LogFileName),
    
    count_trace_match("test_statem:heavy_state\\(timeout", TraceOutput, 3)
         after
  
    file:close(FH)
    end,
 
    
    ok.

%% Documentation: All calls to iolist_to_binary/1 made with a binary as an argument already (kind of useless conversion!): recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> ok end}, 10)
%% Test: All calls to iolist_to_binary/1 made with a binary argument: recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> ok end}, 10)
trace_iolist_to_binary_with_binary(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    
    MatchSpec = dbg:fun2ms(fun([X]) when is_binary(X) -> return_trace() end),    
    recon_trace:calls({erlang, iolist_to_binary, MatchSpec}, 10, [ {io_server, FH},{scope,local}]),

    _ = erlang:iolist_to_binary(<<"already binary">>), % Should trace
    _ = erlang:iolist_to_binary(["not binary"]),      % Should NOT trace
    _ = erlang:iolist_to_binary([<<"mix">>, "ed"]),   % Should NOT trace
    _ = erlang:iolist_to_binary(<<"another binary">>), % Should trace

    timer:sleep(100),
   {ok, TraceOutput} = file:read_file(LogFileName),
    
    recon_trace:clear(),

    assert_trace_match("erlang:iolist_to_binary\\(<<\"already binary\">>\\)", TraceOutput),
    assert_trace_match("erlang:iolist_to_binary\\(<<\"another binary\">>\\)", TraceOutput),
    assert_trace_no_match("erlang:iolist_to_binary\\(\\[\"not binary\"\\]\\)", TraceOutput),
    assert_trace_no_match("erlang:iolist_to_binary\\(\\[<<\"mix\">>,\"ed\"\\]\\)", TraceOutput)
         after
  
    file:close(FH)
    end,
    
    ok.

%% Documentation: Calls to the queue module only in a given process Pid, at a rate of 50 per second at most: recon_trace:calls({queue, '_', '_'}, {50,1000}, [{pid, Pid}])
%% Test: Calls to the test_statem module only in the test_statem process Pid, at a rate of 10 per second at most: recon_trace:calls({test_statem, '_', '_'}, {10,1000}, [{pid, }])
trace_test_statem_calls_specific_pid(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try
    
    %%default statem state is heavy_state
    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
    end,    
    %% new statem in light state
    {ok, Pid} = gen_statem:start(test_statem, [], []),
        
    recon_trace:calls({test_statem, '_', '_'}, {10,1000}, [{pid, Pid},  {io_server, FH},{scope,local}]),

    gen_statem:call(Pid, get_value),
    gen_statem:call(Pid, get_value),
    
    % Call from another process - should NOT trace
    test_statem:get_state(),
    test_statem:get_state(),
    test_statem:get_state(),
    timer:sleep(100),
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(LogFileName),
    
    
    % Check calls originating from  are traced (e.g., handle_event)
    count_trace_match(".*test_statem:light_state", TraceOutput,2),
    
    % Check calls from the other process are NOT traced
    assert_trace_no_match(".*test_statem:heavy_state", TraceOutput),
    
    is_process_alive(Pid) andalso exit(Pid, kill) % Cleanup spawned proc

         after
  
    file:close(FH)
    end,
    
    ok.

%% Documentation: Print the traces with the function arity instead of literal arguments: recon_trace:calls(TSpec, Max, [{args, arity}])
%% Test: Print traces for test_statem calls with arity instead of arguments: recon_trace:calls({test_statem, '_', '_'}, 10, [{args, arity}])
trace_test_statem_calls_arity(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    
    recon_trace:calls({test_statem, '_', '_'}, 10, [{args, arity},  {io_server, FH},{scope,local}]),

    test_statem:get_state(),
    ok = test_statem:switch_state(),
    ok = test_statem:switch_state(),
    timer:sleep(100),
    recon_trace:clear(),
   {ok, TraceOutput} = file:read_file(LogFileName),
    
    

    % Check for arity format, e.g., module:function/arity
    assert_trace_match("test_statem:get_state/0", TraceOutput), % gen_statem callback arity
    assert_trace_match("test_statem:light_state/3", TraceOutput), % gen_statem callback arity
    % Ensure literal args are not present (tricky regex, check absence of typical args)
    assert_trace_no_match("switch_state\\(", TraceOutput),
    assert_trace_no_match("iterator", TraceOutput)
         after

    file:close(FH)
    end,
    
    ok.

%% Documentation: Matching the filter/2 functions of both dict and lists modules, across new processes only: recon_trace:calls([{dict,filter,2},{lists,filter,2}], 10, [{pid, new}])
%% Test: Matching light_state/2 and heavy_state/2 calls in test_statem across new processes only: recon_trace:calls([{test_statem, light_state, 2}, {test_statem, heavy_state, 2}], 10, [{pid, new}])
trace_test_statem_states_new_procs(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
    end,   
        
    recon_trace:calls([{test_statem, light_state, '_'}, {test_statem, heavy_state, '_'}], 10, [{pid, new},  {io_server, FH},{scope,local}]),
   
    {ok, heavy_state,_} = test_statem:get_state(),
%% Call from a *new* process - should trace
    {ok, NewPid} = gen_statem:start(test_statem, [], []),
        
    gen_statem:call(NewPid, get_value),
    gen_statem:call(NewPid, get_value),

    % Call from old process - should NOT trace
    test_statem:get_state(),
    test_statem:get_state(),
    test_statem:get_state(),
    
    timer:sleep(1000),
    recon_trace:clear(),

   {ok, TraceOutput} = file:read_file(LogFileName),
    

    % Check calls from the new process ARE traced
    count_trace_match("test_statem:light_state", TraceOutput, 2),
    assert_trace_no_match("test_statem:heavy_state", TraceOutput),

    is_process_alive(NewPid) andalso exit(NewPid, kill) % Cleanup spawned proc
         after
    file:close(FH)
    end,
    
    ok.

%% Documentation: Tracing the handle_call/3 functions of a given module for all new processes, and those of an existing one registered with gproc: recon_trace:calls({Mod,handle_call,3}, {10,100}, [{pid, [{via, gproc, Name}, new]}])
%% Test: Tracing test_statem:handle_call/3 for new processes and one via gproc (requires gproc setup): recon_trace:calls({test_statem, handle_call, 3}, {10, 100}, [{pid, [{via, gproc, Name}, new]}])
trace_handle_call_new_and_custom_registry(__Config) ->
    LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    case test_statem:get_state() of
        {ok,light_state,_} -> test_statem:switch_state();
        {ok,heavy_state,_} -> ok
    end,
    fake_reg:start(),
    {ok, NewPid} = gen_statem:start({via, fake_reg, ts_test}, test_statem, [], []),
    
    recon_trace:calls([{test_statem, light_state, '_'}, {test_statem, heavy_state, '_'}], 10,
                    [{pid, [{via, fake_reg, ts_test}, new]}, {io_server, FH},{scope,local}]),
        
    gen_statem:call({via, fake_reg, ts_test}, get_value),
    gen_statem:call(NewPid, get_value),

    % Call from old process - should NOT trace
    test_statem:get_state(),
    test_statem:get_state(),
    test_statem:get_state(),
    
    timer:sleep(100),
    recon_trace:clear(),

   {ok, TraceOutput} = file:read_file(LogFileName),
    
    
    % Check calls from the new process ARE traced
    count_trace_match("test_statem:light_state", TraceOutput, 2),
    assert_trace_no_match("test_statem:heavy_state", TraceOutput),

    gen_statem:stop({via, fake_reg, ts_test})

         after
    fake_reg:stop(),
    file:close(FH)
    end.

%% Documentation: Show the result of a given function call: recon_trace:calls({Mod,Fun,fun(_) -> return_trace() end}, Max, Opts)
%% Test: Show the result of test_statem:get_state/0 calls: recon_trace:calls({test_statem, get_state, fun(_) -> return_trace() end}, 10)
trace_get_state_return_fun(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    
    % Ensure state is known (e.g., light)
    case test_statem:get_state() of
        {ok,light_state,_} -> ok;
        {ok,heavy_state,_} -> test_statem:switch_state()
    end,
 
    MatchSpec = dbg:fun2ms(fun(_) -> return_trace() end),
    % Trace the API function test_statem:get_state/1
    recon_trace:calls({test_statem, get_state, MatchSpec}, 10, [ {io_server, FH},{scope,local}]),

    {ok,light_state, N} = test_statem:get_state(),

    timer:sleep(100),
    recon_trace:clear(),

   {ok, TraceOutput} = file:read_file(LogFileName),
    

    % Check for the call and its return value
    assert_trace_match("test_statem:get_state/0 --> {ok,light_state,"++integer_to_list(N)++"}", TraceOutput)
         after
    file:close(FH)
    end,
    
    ok.

%% Documentation: Show the result of a given function call: recon_trace:calls({Mod,Fun,[{'_', [], [{return_trace}]}]}, Max, Opts),
%% Test: Show the result of test_statem:get_state/0 calls (using match spec): recon_trace:calls({test_statem, get_state, [{'_', [], [{return_trace}]}]}, 10)
trace_get_state_return_matchspec(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    
    case test_statem:get_state() of
        {ok,heavy_state,_} -> ok;
        {ok,light_state,_} -> test_statem:switch_state()
    end,

    % Trace the API function test_statem:get_state/1 using match spec
    recon_trace:calls({test_statem, get_state, [{'_', [], [{return_trace}]}]}, 10, [ {io_server, FH},{scope,local}]),

    {ok,heavy_state, N} = test_statem:get_state(),
    timer:sleep(100),
    recon_trace:clear(),

    {ok, TraceOutput} = file:read_file(LogFileName),
    

    % Check for the call and its return value
    
    assert_trace_match("test_statem:get_state/0 --> {ok,heavy_state,"++integer_to_list(N)++"}", TraceOutput)        
         after
    file:close(FH)
    end,
    
    ok.

%% Documentation: A short-hand version for this pattern of 'match anything, trace everything' for a function is recon_trace:calls({Mod, Fun, return_trace}).
%% Test: Show the result of test_statem:get_state/0 calls (shorthand): recon_trace:calls({test_statem, get_state, return_trace}, 10).
trace_get_state_return_shorthand(_Config) ->
      LogFileName = "test_statem_"++atom_to_list(?FUNCTION_NAME)++".log",
    {ok, FH} = file:open(LogFileName, [write]),
    try      
    
    case test_statem:get_state() of
        {ok,light_state,_} -> ok;
        {ok,heavy_state,_} -> test_statem:switch_state()
    end,
  

    % Trace the API function test_statem:get_state/1 using shorthand
    recon_trace:calls({test_statem, get_state, return_trace}, 10, [ {io_server, FH},{scope,local}]),

    {ok,light_state, N} = test_statem:get_state(),
    
    timer:sleep(100),
    recon_trace:clear(),

   {ok, TraceOutput} = file:read_file(LogFileName),
    

    % Check for the call and its return value
    assert_trace_match("test_statem:get_state/0 --> {ok,light_state,"++integer_to_list(N)++"}", TraceOutput)
         after
    file:close(FH)
    end,
    
    ok.


