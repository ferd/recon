%%% Test suite for recon_alloc. Because many of the tests in
%%% here depend on memory allocation and this is *not* transparent,
%%% the tests are rather weak and more or less check for interface
%%% conformance and obvious changes more than anything.
-module(recon_alloc_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [memory, fragmentation, cache_hit_rates, average_block_sizes,
          sbcs_to_mbcs, allocators, snapshots, units].

memory(_Config) ->
    %% Freeze memory values for tests
    recon_alloc:snapshot(),
    %% memory returns values for 'used', 'allocated', 'unused', and 'usage'.
    Used = recon_alloc:memory(used),
    Alloc = recon_alloc:memory(allocated),
    Unused = recon_alloc:memory(unused),
    Usage = recon_alloc:memory(usage),
    Types = recon_alloc:memory(allocated_types),
    Instances = recon_alloc:memory(allocated_instances),
    %% equivalent to memory(_, current)
    Used = recon_alloc:memory(used, current),
    Alloc = recon_alloc:memory(allocated, current),
    Unused = recon_alloc:memory(unused, current),
    Usage = recon_alloc:memory(usage, current),
    Types = recon_alloc:memory(allocated_types, current),
    Instances = recon_alloc:memory(allocated_instances, current),
    %% relationships, and variation rates
    Alloc = Used + Unused,
    Usage = Used/Alloc,
    true = Alloc > 0,
    true = Usage > 0,
    true = Unused > 0,
    %% Current vs. Max
    true = Used =< recon_alloc:memory(used, max),
    true = Alloc =< recon_alloc:memory(allocated, max),
    true = Types =< recon_alloc:memory(allocated_types, max),
    true = Instances =< recon_alloc:memory(allocated_instances, max),
    %% Key presences in allocateds
    [{binary_alloc,_},
     {driver_alloc,_},
     {eheap_alloc,_},
     {ets_alloc,_},
     {fix_alloc,_},
     {ll_alloc,_},
     {sl_alloc,_},
     {std_alloc,_},
     {temp_alloc,_}] = Types,
    MaxInstance = lists:max(proplists:get_keys(Instances)),
    true = lists:sort(proplists:get_keys(Instances))
         =:= lists:seq(0,MaxInstance),
    %% allocate a bunch of memory and see if the memory used goes
    %% up and relationships change accordingly.
    [spawn(fun() -> lists:seq(1,1000), timer:sleep(1000) end)
     || _ <- lists:seq(1,100)],
    recon_alloc:snapshot(),
    true = Used < recon_alloc:memory(used),
    true = Alloc < recon_alloc:memory(allocated)
         orelse Usage < recon_alloc:memory(usage),
    %% Cleanup
    recon_alloc:snapshot_clear().

fragmentation(_Config) ->
    %% Values returned are of the format [{K, V}] and supports both
    %% searches by 'current' and 'max'
    Current = recon_alloc:fragmentation(current),
    Max = recon_alloc:fragmentation(max),
    true = allocdata(Current),
    true = allocdata(Max),
    true = Max =/= Current,
    Keys = [sbcs_usage, sbcs_block_size, sbcs_carriers_size, mbcs_usage,
            mbcs_block_size, mbcs_carriers_size],
    true = each_keys(Keys, Current),
    true = each_keys(Keys, Max).

cache_hit_rates(_Config) ->
    Cache = recon_alloc:cache_hit_rates(),
    true = allocdata(Cache),
    true = each_keys([hit_rate, hits, calls], Cache).

average_block_sizes(_Config) ->
    CurrentSizes = recon_alloc:average_block_sizes(current),
    MaxSizes = recon_alloc:average_block_sizes(max),
    true = lists:all(fun({K,V}) -> is_atom(K) andalso is_list(V) end,
                     CurrentSizes),
    true = each_keys([mbcs, sbcs], CurrentSizes),
    true = lists:all(fun({K,V}) -> is_atom(K) andalso is_list(V) end,
                     MaxSizes),
    true = each_keys([mbcs, sbcs], MaxSizes).

sbcs_to_mbcs(_Config) ->
    Check = fun(Ratio) ->
        true = lists:all(fun({{Alloc,N},_}) ->
                            is_atom(Alloc) andalso is_integer(N)
                        end,
                        Ratio),
        true = lists:all(fun({_,infinity}) -> true;
                            ({_,0}) -> true;
                            ({_,N}) -> is_float(N)
                        end,
                        Ratio)
    end,
    Check(recon_alloc:sbcs_to_mbcs(current)),
    Check(recon_alloc:sbcs_to_mbcs(max)).


allocators(_Config) ->
    true = allocdata(recon_alloc:allocators()).

snapshots(Config) ->
    File = filename:join(?config(priv_dir, Config), "snapshot"),
    undefined = recon_alloc:snapshot(),
    true = is_snapshot(recon_alloc:snapshot()),
    true = is_snapshot(recon_alloc:snapshot_clear()),
    undefined = recon_alloc:snapshot_clear(),
    ok = recon_alloc:snapshot_print(),
    ok = recon_alloc:snapshot_save(File),
    undefined = recon_alloc:snapshot_get(),
    undefined = recon_alloc:snapshot(),
    ok = recon_alloc:snapshot_print(),
    ok = recon_alloc:snapshot_save(File),
    _ = recon_alloc:snapshot_clear(),
    undefined = recon_alloc:snapshot_load(File),
    true = is_snapshot(recon_alloc:snapshot_load(File)),
    true = is_snapshot(recon_alloc:snapshot_get()),
    %% Also supporting another dump format
    file:write_file(
      File,
      io_lib:format("~p.~n", [{erlang:memory(),
                                [{A,erlang:system_info({allocator,A})}
                                 || A <- erlang:system_info(alloc_util_allocators)++[sys_alloc,mseg_alloc]]}
                             ])),
    _ = recon_alloc:snapshot_clear(),
    undefined = recon_alloc:snapshot_load(File),
    true = is_snapshot(recon_alloc:snapshot_get()),
    recon_alloc:snapshot_clear().

units(_Config) ->
    recon_alloc:snapshot(),
    ByteMem = recon_alloc:memory(used),
    ByteAvg = [X || {_,[{_,X}|_]} <- recon_alloc:average_block_sizes(max)],
    recon_alloc:set_unit(byte),
    ByteMem = recon_alloc:memory(used),
    ByteAvg = [X || {_,[{_,X}|_]} <- recon_alloc:average_block_sizes(max)],
    recon_alloc:set_unit(kilobyte),
    true = ByteMem/1024 == recon_alloc:memory(used),
    true = [X/1024 || X <- ByteAvg]
        == [X || {_,[{_,X}|_]} <- recon_alloc:average_block_sizes(max)],
    recon_alloc:set_unit(megabyte),
    true = ByteMem/(1024*1024) == recon_alloc:memory(used),
    true = [X/(1024*1024) || X <- ByteAvg]
        == [X || {_,[{_,X}|_]} <- recon_alloc:average_block_sizes(max)],
    recon_alloc:set_unit(gigabyte),
    true = ByteMem/(1024*1024*1024) == recon_alloc:memory(used),
    true = [X/(1024*1024*1024) || X <- ByteAvg]
        == [X || {_,[{_,X}|_]} <- recon_alloc:average_block_sizes(max)],
    recon_alloc:snapshot_clear().

%%% Helpers
allocdata(L) ->
    Validate = fun({{Allocator,N}, List}) ->
        is_atom(Allocator) andalso is_integer(N)
        andalso
        lists:all(fun({_,_}) -> true; (_) -> false end, List)
    end,
    lists:all(Validate, L).

each_keys(Keys,ListOfLists) ->
    lists:all(fun({_K,L}) ->
                lists:all(fun(Key) ->
                             undefined =/= proplists:get_value(Key,L)
                          end,
                          Keys)
              end,
              ListOfLists).

is_snapshot({Mem,Snap}) ->
    lists:all(fun({K,V}) -> is_atom(K) andalso is_integer(V) end, Mem)
    andalso
    allocdata(Snap).
