%%% Functions to deal with Erlang's memory allocators
%%% http://www.erlang.org/doc/man/erts_alloc.html
-module(recon_alloc).
%% specific allocators. Not included: 
%% - sys_alloc (malloc)
%% - mseg_alloc used by other allocs, mmap-only. Caches allocations.
-define(ALLOCATORS, [temp_alloc,    % used for temporary allocations
                     eheap_alloc,   % heap data (i.e. process heaps)
                     binary_alloc,  % global binary heap
                     ets_alloc,     % ETS data
                     driver_alloc,  % driver data
                     sl_alloc,      % short lived memory blocks
                     ll_alloc,      % long-libed data (i.e. Erlang code)
                     fix_alloc,     % frequently used fixed-size data
                     std_alloc      % other memory blocks
                    ]).

%% sbcs: single-block carriers
%% mbcs: multiblock carriers
-define(CURRENT_POS, 2). % pos in sizes tuples
-define(MAX_POS, 4). % pos in sizes tuples

-export([memory/1,
         fragmentation/1, fragmentation/2,
         cache_hit_rates/0, cache_hit_rates/1,
         average_sizes/0, average_sizes/1
         ]).
-export([dump_to_allocators/1]).

%% @doc the value for `memory(allocated)' value should match the one
%% reported by the OS. Otherwise, memory has to be allocated outside
%% of the regular allocators -- a leak in NIFs or Drivers maybe.
-spec memory(used | allocated | unused) -> pos_integer()
    ;       (usage) -> number().
memory(used) ->
    erlang:memory(total);
memory(allocated) ->
    AllocProps = [Prop || {_Alloc,_N,Prop} <- allocators()],
    lists:sum(lists:map(fun(Props) ->
        SbcsProps = proplists:get_value(sbcs, Props),
        MbcsProps = proplists:get_value(mbcs, Props),
        {carriers_size,Sbcs,_,_} = lists:keyfind(carriers_size,1, SbcsProps),
        {carriers_size,Mbcs,_,_} = lists:keyfind(carriers_size,1, MbcsProps),
        Sbcs+Mbcs
    end, AllocProps));
memory(unused) ->
    memory(allocated) - memory(used);
memory(usage) ->
    memory(used) / memory(allocated).

%% compare the current block size to the carrier size of each
%% allocator instance for both mbcs and sbcs
%% We should sort for both low % of usage and large total usage, in
%% order to find relevant leaky allocators.
fragmentation(Keyword) -> fragmentation(Keyword, allocators()).
fragmentation(Keyword, Allocs) ->
    Pos = case Keyword of
        current -> ?CURRENT_POS;
        max -> ?MAX_POS
    end,
    WeighedData = [begin
      LS = proplists:get_value(sbcs, Props),
      BlockSbcs = element(Pos, lists:keyfind(blocks_size,1,LS)),
      CarSbcs = element(Pos, lists:keyfind(carriers_size,1,LS)),
      LM = proplists:get_value(mbcs,Props),
      BlockMbcs = element(Pos, lists:keyfind(blocks_size,1,LM)),
      CarMbcs = element(Pos, lists:keyfind(carriers_size,1,LM)),
      {Weight, Vals} = weighed_values({BlockSbcs,CarSbcs},
                                      {BlockMbcs,CarMbcs}),
      {Weight, {Allocator,N}, Vals}
    end || {Allocator, N, Props} <- Allocs],
    [{Key,Val} || {_W, Key, Val} <- lists:reverse(lists:sort(WeighedData))].

cache_hit_rates() ->
    cache_hit_rates(erlang:system_info({allocator,mseg_alloc})).

cache_hit_rates(Instances) ->
    WeighedData = [begin
      Mem = proplists:get_value(memkind, Props),
      {_,Hits} = lists:keyfind(cache_hits, 1, proplists:get_value(status,Mem)),
      {_,_,Alloc} = lists:keyfind(mseg_alloc,1,proplists:get_value(calls,Mem)),
      HitRate = usage(Hits,Alloc),
      Weight = (1.00 - HitRate)*Alloc,
      {Weight, {instance,N}, [{hit_rate,HitRate}, {hits,Hits}, {alloc,Alloc}]}
    end || {instance, N, Props} <- Instances],
    [{Key,Val} || {_W,Key,Val} <- lists:reverse(lists:sort(WeighedData))].

average_sizes() -> average_sizes(allocators()).

average_sizes(Allocs) ->
    Dict = lists:foldl(fun({Instance,_,Props},Dict0) ->
      LS = proplists:get_value(sbcs, Props),
      CarSbcs = element(?CURRENT_POS, lists:keyfind(carriers,1,LS)),
      SizeSbcs = element(?CURRENT_POS, lists:keyfind(carriers_size,1,LS)),
      LM = proplists:get_value(mbcs,Props),
      CarMbcs = element(?CURRENT_POS, lists:keyfind(carriers,1,LM)),
      SizeMbcs = element(?CURRENT_POS, lists:keyfind(carriers_size,1,LM)),
      Dict1 = dict:update_counter({Instance,sbcs,count},CarSbcs,Dict0),
      Dict2 = dict:update_counter({Instance,sbcs,size},SizeSbcs,Dict1),
      Dict3 = dict:update_counter({Instance,mbcs,count},CarMbcs,Dict2),
      Dict4 = dict:update_counter({Instance,mbcs,size},SizeMbcs,Dict3),
      Dict4
    end,
    dict:new(),
    Allocs),
    average_group(average_calc(lists:sort(dict:to_list(Dict)))).

average_calc([]) ->
    [];
average_calc([{{Instance,Type,count},Ct},{{Instance,Type,size},Size}|Rest]) ->
    case {Size,Ct} of
        {0,0} -> [{Instance, Type, 0} | average_calc(Rest)];
        _ -> [{Instance,Type,Size/Ct} | average_calc(Rest)]
    end.

average_group([]) -> [];
average_group([{Instance,Type1,N},{Instance,Type2,M} | Rest]) ->
    [{Instance,[{Type1,N},{Type2,M}]} | average_group(Rest)].

allocators() ->
    [{A,N,Props} || A <- ?ALLOCATORS,
                    {_,N,Props} <- erlang:system_info({allocator,A})].

%% Convert an existing allocator dump to a format this module can manage
dump_to_allocators([]) -> [];
dump_to_allocators([{Name, Instances}|Rest]) ->
    case lists:member(Name, ?ALLOCATORS) of
        true ->
            [{Name,N,Props} || {instance,N,Props} <- Instances]
            ++
            dump_to_allocators(Rest);
        false ->
            dump_to_allocators(Rest)
    end.

%%% Private
%% sort on small usage vs. large size.
weighed_values({SbcsBlockSize, SbcsCarrierSize},
               {MbcsBlockSize, MbcsCarrierSize}) ->
    SbcsUsage = usage(SbcsBlockSize, SbcsCarrierSize),
    MbcsUsage = usage(MbcsBlockSize, MbcsCarrierSize),
    SbcsWeight = (1.00 - SbcsUsage)*SbcsCarrierSize,
    MbcsWeight = (1.00 - MbcsUsage)*MbcsCarrierSize,
    Weight = SbcsWeight + MbcsWeight,
    {Weight, [{sbcs_usage, SbcsUsage},
              {mbcs_usage, MbcsUsage},
              {sbcs_block_size, SbcsBlockSize},
              {sbcs_carriers_size, SbcsCarrierSize},
              {mbcs_block_size, MbcsBlockSize},
              {mbcs_carriers_size, MbcsCarrierSize}]}.
              
usage(0,0) -> 1.00;
%usage(N,0) -> ???;
usage(Block,Carrier) -> Block/Carrier.
