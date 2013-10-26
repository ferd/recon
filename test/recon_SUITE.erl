%%% Test suite for the recon module. Because many of the tests in
%%% here depend on runtime properties and this is *not* transparent,
%%% the tests are rather weak and more or less check for interface
%%% conformance and obvious changes more than anything.
-module(recon_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [{group,info}, proc_count, proc_window, bin_leak,
          node_stats_list, get_state, source, tcp, udp, files, port_types,
          inet_count, inet_window].

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

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

info3(Config) ->
    Pid = ?config(pid, Config),
    {A,B,C} = pid_to_triple(Pid),
    Info = recon:info(Pid),
    Info = recon:info(A,B,C).


info4(Config) ->
    Pid = ?config(pid, Config),
    Keys = [meta, signals, location, memory_used, work,
            links, monitors, reductions, messages,
            [links, monitors, reductions, messages]],
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
    Res = recon:info(info1),
    Res = recon:info(whereis(info1)),
    Res = recon:info(pid_to_triple(whereis(info1))),
    Res = recon:info(lists:flatten(io_lib:format("~p",[Pid]))),
    unregister(info1).

info2(Config) ->
    Pid = ?config(pid, Config),
    Categories = [{meta, [registered_name, dictionary, group_leader, status]},
                  {signals, [links, monitors, monitored_by, trap_exit]},
                  {location, [initial_call, current_stacktrace]},
                  {memory_used, [memory, message_queue_len, heap_size,
                                 total_heap_size, garbage_collection]},
                  {work, [reductions]}],
    %% registered_name is special -- only returns
    %% [] when passed through by info/2. Because we pass terms through
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
    true = lists:usort(fun({_,V1,_},{_,V2,_}) -> V1 >= V2 end,
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
    %% all results are =< 0, and ordered from smallest to biggest
    lists:foldl(fun({_,Current,_}, Next) when Current =< Next -> Current end,
                0,
                lists:reverse(Res)).

%% This function implicitly tests node_stats/4
node_stats_list(_Config) ->
    Res = recon:node_stats_list(2,100),
    2 = length([1 || {[{process_count,_},
                       {run_queue,_},
                       {error_logger_queue_len,_},
                       {memory_total,_},
                       {memory_procs,_},
                       {memory_atoms,_},
                       {memory_bin,_},
                       {memory_ets,_}],
                      [{bytes_in,_},
                       {bytes_out,_},
                       {gc_count,_},
                       {gc_words_reclaimed,_},
                       {reductions,_}]} <- Res]).

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
