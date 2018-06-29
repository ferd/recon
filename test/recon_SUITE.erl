%%% Test suite for the recon module. Because many of the tests in
%%% here depend on runtime properties and this is *not* transparent,
%%% the tests are rather weak and more or less check for interface
%%% conformance and obvious changes more than anything.
-module(recon_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-ifdef(OTP_RELEASE).
-define(FILES_IMPL, nif).
-define(ERROR_LOGGER_MATCH(_),  ).
-define(REDUCTIONS_MATCH(X), X).
-else.
-define(FILES_IMPL, port).
-define(ERROR_LOGGER_MATCH(X), X,).
-define(REDUCTIONS_MATCH(_), []).
-endif.

all() -> [{group,info}, proc_count, proc_window, bin_leak,
          node_stats_list, get_state, source, tcp, udp, files, port_types,
          inet_count, inet_window, binary_memory, scheduler_usage].

groups() -> [{info, [], [info3, info4, info1, info2,
                         port_info1, port_info2]}].

init_per_group(info, Config) ->
    Self = self(),
    Pid = spawn(fun() ->
         {ok, TCP} = gen_tcp:listen(0, []),
         {ok, UDP} = gen_udp:open(0),
         Self ! {TCP, UDP},
         timer:sleep(infinity)
     end),
    receive
        {TCP, UDP} -> [{pid, Pid}, {tcp, TCP}, {udp, UDP} | Config]
    end.

end_per_group(info, Config) ->
    exit(?config(pid, Config), kill).

init_per_testcase(files, Config) ->
    case ?FILES_IMPL of
        nif -> {skip, "files can no longer be listed in OTP-21 and above"};
        port -> Config
    end;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

info3(Config) ->
    Pid = ?config(pid, Config),
    {A,B,C} = pid_to_triple(Pid),
    Info1 = recon:info(Pid),
    Info2 = recon:info(A,B,C),
    %% Reduction count is unreliable
    ?assertMatch(?REDUCTIONS_MATCH(
                    [{work, [{reductions,_}]}, {work, [{reductions,_}]}]
                 ), (Info1 -- Info2) ++ (Info2 -- Info1)).

info4(Config) ->
    Pid = ?config(pid, Config),
    Keys = [meta, signals, location, memory_used, work,
            links, monitors, messages,
            [links, monitors, messages]],
    {A,B,C} = pid_to_triple(Pid),
    lists:map(fun(Key) ->
                  Info = recon:info(Pid, Key),
                  Info = recon:info(A,B,C, Key)
              end,
              Keys).

info1(Config) ->
    Pid = ?config(pid, Config),
    Categories = [{meta, [registered_name, dictionary, group_leader, status]},
                  {signals, [links, monitors, monitored_by, trap_exit]},
                  {location, [initial_call, current_stacktrace]},
                  {memory_used, [memory, message_queue_len, heap_size,
                                 total_heap_size, garbage_collection]},
                  {work, [reductions]}],
    [] = lists:flatten(
        [K || {Cat,List} <- Categories,
              K <- List,
              Info <- [recon:info(Pid)],
              undefined == proplists:get_value(K, proplists:get_value(Cat,Info))
        ]),
    register(info1, Pid),
    Res1 = recon:info(info1),
    Res2 = recon:info(whereis(info1)),
    Res3 = recon:info(pid_to_triple(whereis(info1))),
    Res4 = recon:info(lists:flatten(io_lib:format("~p",[Pid]))),
    unregister(info1),
    L = lists:usort(Res1 ++ Res2 ++ Res3 ++ Res4),
    ?assertMatch(?REDUCTIONS_MATCH([{work,[{reductions,_}]},
                                    {work,[{reductions,_}]},
                                    {work,[{reductions,_}]}]), L -- Res1),
    ?assertMatch(?REDUCTIONS_MATCH([{work,[{reductions,_}]},
                                    {work,[{reductions,_}]},
                                    {work,[{reductions,_}]}]), L -- Res2),
    ?assertMatch(?REDUCTIONS_MATCH([{work,[{reductions,_}]},
                                    {work,[{reductions,_}]},
                                    {work,[{reductions,_}]}]), L -- Res3),
    ?assertMatch(?REDUCTIONS_MATCH([{work,[{reductions,_}]},
                                    {work,[{reductions,_}]},
                                    {work,[{reductions,_}]}]), L -- Res4),
    ok.

info2(Config) ->
    Pid = ?config(pid, Config),
    Categories = [{meta, [registered_name, dictionary, group_leader, status]},
                  {signals, [links, monitors, monitored_by, trap_exit]},
                  {location, [initial_call, current_stacktrace]},
                  {memory_used, [memory, message_queue_len, heap_size,
                                 total_heap_size, garbage_collection]},
                  {work, [reductions]}],
    %% registered_name is special -- only returns
    %% [] when passed through by info/2. Because we pass terms through
    %% according to the docs, we have to respect that
    [] = recon:info(Pid, registered_name),
    %% Register to get the expected tuple
    register(info2, Pid),
    Keys = lists:flatten([K || {_,L} <- Categories, K <- L]),
    %% check that individual category call works for all terms
    [] = lists:flatten(
        [K || {Cat, List} <- Categories,
              K <- List,
              {GetCat,Info} <- [recon:info(Pid, Cat)],
              Cat =:= GetCat,
              undefined =:= proplists:get_value(K, Info)]
    ),
    %% Can get a list of arguments
    true = lists:sort(Keys)
           =:=
           lists:sort(proplists:get_keys(recon:info(Pid, Keys))),
    true = length(Keys)
           =:=
           length([1 || K1 <- Keys, {K2,_} <- [recon:info(Pid, K1)],
                        K1 == K2]),
    unregister(info2).

proc_count(_Config) ->
    Res = recon:proc_count(memory, 10),
    true = proc_attrs(Res),
    %% greatest to smallest
    true = lists:usort(fun({P1,V1,_},{P2,V2,_}) -> {V1,P1} >= {V2,P2} end,
                       Res) =:= Res,
    10 = length(Res),
    15 = length(recon:proc_count(reductions, 15)).

proc_window(_Config) ->
    Res = recon:proc_window(reductions, 10, 100),
    true = proc_attrs(Res),
    %% we can't check order easily because stuff doesn't move
    %% fast enough on a test node to show up here
    10 = length(Res),
    15 = length(recon:proc_window(memory, 15, 100)).

bin_leak(_Config) ->
    Res = recon:bin_leak(5),
    5 = length(Res),
    true = proc_attrs(Res),
    true = lists:any(fun({_,Val,_})-> Val =/= 0  end, Res),
    %% all results are =< 0, and ordered from smallest to biggest
    lists:foldl(fun({_,Current,_}, Next) when Current =< Next -> Current end,
                0,
                lists:reverse(Res)).

%% This function implicitly tests node_stats/4
node_stats_list(_Config) ->
    Res = recon:node_stats_list(2,100),
    2 = length([1 || {[{process_count,_},
                       {run_queue,_},
                       ?ERROR_LOGGER_MATCH({error_logger_queue_len,_})
                       {memory_total,_},
                       {memory_procs,_},
                       {memory_atoms,_},
                       {memory_bin,_},
                       {memory_ets,_}|_],
                      [{bytes_in,_},
                       {bytes_out,_},
                       {gc_count,_},
                       {gc_words_reclaimed,_},
                       {reductions,_},
                       {scheduler_usage,[_|_]}|_]} <- Res]).

get_state(_Config) ->
    Res = recon:get_state(kernel_sup),
    Res = recon:get_state(whereis(kernel_sup)),
    Res = recon:get_state(pid_to_triple(whereis(kernel_sup))),
    state = element(1,Res).

%% Skip on remote-loading, too hard

source(_Config) ->
    Pat = <<"find this sentence in this file's source">>,
    {_,_} = binary:match(iolist_to_binary(recon:source(?MODULE)), Pat).

tcp(_Config) ->
    {ok, Listen} = gen_tcp:listen(0, []),
    true = lists:member(Listen, recon:tcp()).

udp(_Config) ->
    {ok, Port} = gen_udp:open(0),
    true = lists:member(Port, recon:udp()).

%% SCTP not supported everywhere, skipped.

files(Config) ->
    Len = length(recon:files()),
    {ok, _IoDevice} = file:open(filename:join(?config(priv_dir, Config), "a"),
                                [write]),
    true = Len + 1 =:= length(recon:files()).

port_types(_Config) ->
    lists:all(fun({[_|_],N}) when is_integer(N), N > 0 -> true end,
              recon:port_types()).

inet_count(_Config) ->
    [gen_tcp:listen(0,[]) || _ <- lists:seq(1,100)],
    Res = recon:inet_count(oct, 10),
    %% all results are =< 0, and ordered from biggest to smaller
    lists:foldl(fun({_,Current,_}, Next) when Current >= Next -> Current end,
                0,
                lists:reverse(Res)),
    true = inet_attrs(Res),
    %% greatest to smallest
    10 = length(Res),
    15 = length(recon:inet_count(cnt, 15)).

inet_window(_Config) ->
    [gen_tcp:listen(0,[]) || _ <- lists:seq(1,100)],
    Res = recon:inet_window(oct, 10, 100),
    %% all results are =< 0, and ordered from biggest to smaller
    lists:foldl(fun({_,Current,_}, Next) when Current >= Next -> Current end,
                0,
                lists:reverse(Res)),
    true = inet_attrs(Res),
    %% greatest to smallest
    10 = length(Res),
    15 = length(recon:inet_window(cnt, 15, 100)).

%% skip RPC

port_info1(Config) ->
    TCP = ?config(tcp, Config),
    UDP = ?config(tcp, Config),
    TCPInfo = recon:port_info(TCP),
    UDPInfo = recon:port_info(UDP),
    %% type is the only specific value supported for now
    Cats = lists:sort([meta, signals, io, memory_used, type]),
    %% Too many options for inet stuff.
    Cats = lists:sort(proplists:get_keys(TCPInfo)),
    Cats = lists:sort(proplists:get_keys(UDPInfo)).

port_info2(Config) ->
    TCP = ?config(tcp, Config),
    UDP = ?config(tcp, Config),
    %% Not testing the whole set, but heeeh. Good enough.
    {io, [{input,_},{output,_}]} = recon:port_info(TCP, io),
    {io, [{input,_},{output,_}]} = recon:port_info(UDP, io).

%% binary_memory is a created attribute that counts the amount
%% of memory held by refc binaries, usable in info/2-4 and
%% in proc_count/proc_window.
binary_memory(_Config) ->
    %% we just don't want them to crash like it happens with
    %% non-existing attributes.
    ?assertError(_, recon:proc_count(fake_attribute, 10)),
    ?assertError(_, recon:proc_window(fake_attribute, 10, 100)),
    recon:proc_count(binary_memory, 10),
    recon:proc_window(binary_memory, 10, 100),
    %% And now for info, it should work in lists, which can contain
    %% duplicates, or in single element calls.
    %% Note: we allocate the binary before spawning the process but
    %%       use it in a closure to avoid race conditions on allocation for
    %%       the test.
    Bin = <<1:999999>>,
    Pid1 = spawn_link(fun() -> timer:sleep(100000) end),
    Pid2 = spawn_link(fun() -> timer:sleep(100000), Bin end),
    {binary_memory, 0} = recon:info(Pid1, binary_memory),
    {binary_memory, N} = recon:info(Pid2, binary_memory),
    true = N > 0,
    Res1 = recon:info(Pid1, [binary, binary_memory, binary]),
    Res2 = recon:info(Pid2, [binary_memory, binary, binary_memory]),
    %% we expect everything to look as a single call to process_info/2
    [{binary,X}, {binary_memory,_}, {binary,X}] = Res1,
    [{binary_memory,Y}, {binary,_}, {binary_memory,Y}] = Res2.

%% Just check that we get many schedulers and that the usage values are all
%% between 0 and 1 inclusively. We don't care for edge cases like a
%% scheduler disappearing halfway through a run.
scheduler_usage(_Config) ->
    List = recon:scheduler_usage(100),
    ?assertEqual(length(List), length(
                 [1 || {Id,Rate} <- List,
                       is_integer(Id), Id > 0,
                       Rate >= 0, Rate =< 1])
    ). 

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

pid_to_triple(Pid) when is_pid(Pid) ->
    "<0."++Rest = lists:flatten(io_lib:format("~p",[Pid])),
    {B,C} = lists:foldl(fun($>, Acc) -> Acc;
                           ($., B) -> {B,0};
                           (N, {B,C}) -> {B,(C*10)+(N-$0)};
                           (N, B) -> (B*10)+(N-$0)
                        end,
                        0,
                        Rest),
    {0,B,C}.

proc_attrs(L) ->
    lists:all(fun({Pid,_Val,List}) -> is_pid(Pid) andalso is_list(List) end,
              L).

inet_attrs(L) ->
    lists:all(fun({Port,_Val,List}) -> is_port(Port) andalso is_list(List) end,
              L).
