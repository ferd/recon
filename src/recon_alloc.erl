%%% @doc Functions to deal with Erlang's memory allocators
%%% http://www.erlang.org/doc/man/erts_alloc.html
%%% This module is not to be considered stable yet.
%%%
%%% Glossary:
%%%  - `sbcs': single-block carriers.
%%%  - `mbcs': multiblock carriers.
%%%
%%% All sizes returned by this module are in bytes.
%%%
%%% @todo extend definitions and docs
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

-type allocator() :: temp_alloc | eheap_alloc | binary_alloc | ets_alloc
                   | driver_alloc | sl_alloc | ll_alloc | fix_alloc
                   | std_alloc.
-type instance() :: non_neg_integer().
-type allocdata(T) :: {{allocator(), instance()}, T}.
-export_type([allocator/0, instance/0, allocdata/1]).

%% sbcs: single-block carriers
%% mbcs: multiblock carriers
-define(CURRENT_POS, 2). % pos in sizes tuples
-define(MAX_POS, 4). % pos in sizes tuples

-export([memory/1, fragmentation/1, cache_hit_rates/0, average_sizes/0]).

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%

%% @doc reports one of multiple possible memory values for the entire
%% node depending on what is to be reported:
%%
%% - `memory(used)' reports the memory that is actively used for allocated
%%   Erlang data;
%% - `memory(allocated)' reports the memory that is reserved by the VM. It
%%   includes the memory used, but also the memory yet-to-be-used but still
%%   given by the OS. This is the amount you want if you're dealing with
%%   ulimit and OS-reported values.
%% - `memory(unused)' reports the amount of memory reserved by the VM that
%%   is not being allocated. Equivalent to `memory(allocated) - memory(used)'.
%% - `memory(usage)' returns a percentage (0.0 .. 1.0) of `used/allocated'
%%   memory ratios.
%%
%% The memory reported by `memory(allocated)' should roughly
%% match what the OS reports. If this amount is different by a large margin,
%% it may be the sign that someone is allocating memory in C directly, outside
%% of Erlang's own allocator -- a big warning sign.
%%
%% Also note that low memory usages can be the sign of fragmentation in
%% memory, in which case exploring which specific allocator is at fault
%% is recommended (see {@link fragmentation/1})
-spec memory(used | allocated | unused) -> pos_integer()
    ;       (usage) -> number().
memory(used) ->
    erlang:memory(total);
memory(allocated) ->
    AllocProps = [Prop || {_Alloc,Prop} <- allocators()],
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

%% @doc Compares the block sizes to the carrier sizes, both for
%% single block (`sbcs') and multiblock (`mbcs') carriers.
%%
%% The returned results are sorted by a weight system that is
%% somewhat likely to return the most fragmented allocators first,
%% based on their percentage of use and the total size of the carriers,
%% for both `sbcs' and `mbcs'.
%%
%% The values can both be returned for `current' allocator values, and
%% for `max' allocator values. The current values hold the present allocation
%% numbers, and max values, the values at the peak. Comparing both together
%% can give an idea of whether the node is currently being at its memory peak
%% when possibly leaky, or if it isn't. This information can in turn
%% influence the tuning of allocators to better fit sizes.
-spec fragmentation(current | max) -> [allocdata([{atom(), term()}])].
fragmentation(Keyword) ->
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
    end || {{Allocator, N}, Props} <- allocators()],
    [{Key,Val} || {_W, Key, Val} <- lists:reverse(lists:sort(WeighedData))].

%% @doc looks at the `mseg_alloc' allocator (allocator used by all the
%% allocators in {@link allocator()}) and returns information relative to
%% the cache hit rates. Unless memory has expected spiky behaviour, it should
%% usually be above 0.80 (80%).
%% The values returned are sorted by a weight combining the lower cache hit
%% joined to the largest memory values allocated.
-spec cache_hit_rates() -> [{{instance,instance()}, [{Key,Val}]}] when
    Key :: hit_rate | hits | alloc,
    Val :: term().
cache_hit_rates() ->
    WeighedData = [begin
      Mem = proplists:get_value(memkind, Props),
      {_,Hits} = lists:keyfind(cache_hits, 1, proplists:get_value(status,Mem)),
      {_,_,Alloc} = lists:keyfind(mseg_alloc,1,proplists:get_value(calls,Mem)),
      HitRate = usage(Hits,Alloc),
      Weight = (1.00 - HitRate)*Alloc,
      {Weight, {instance,N}, [{hit_rate,HitRate}, {hits,Hits}, {alloc,Alloc}]}
    end || {instance, N, Props} <- erlang:system_info({allocator,mseg_alloc})],
    [{Key,Val} || {_W,Key,Val} <- lists:reverse(lists:sort(WeighedData))].

%% @doc Checks all allocators in {@link allocator()} and returns the average
%% carrier sized being used for `mbcs' and `sbcs'. This value is interesting
%% to use because it will tell us how used most carriers are individually being
%% used for. This can be related to the VM's largest multiblock carrier size
%% (`lmbcs') and smallest multiblock carrier size (`smbcs') to specify
%% allocation strategies regarding the block sizes to be used.
%% @todo explain what values could lead to what strategies
-spec average_sizes() -> [{allocator(), [{Key,Val}]}] when
    Key :: mbcs | sbcs,
    Val :: number().
average_sizes() ->
    Dict = lists:foldl(fun({{Instance,_},Props},Dict0) ->
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
    allocators()),
    average_group(average_calc(lists:sort(dict:to_list(Dict)))).

%% @doc returns a dump of all allocator settings and values
-spec allocators() -> [allocdata(term())].
allocators() ->
    [{{A,N},Props} || A <- ?ALLOCATORS,
                      {_,N,Props} <- erlang:system_info({allocator,A})].

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

%% Sort on small usage vs. large size.
%% The weight cares about both the sbcs and mbcs values, and also
%% returns a proplist of possibly interesting values.
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
              
%% Returns the `BlockSize/CarrierSize' as a 0.0 -> 1.0 percentage,
%% but also takes 0/0 to be 100% to make working with sorting and
%% weights simpler.
usage(0,0) -> 1.00;
%usage(N,0) -> ???;
usage(Block,Carrier) -> Block/Carrier.

%% Calculation for the average of blocks being used.
average_calc([]) ->
    [];
average_calc([{{Instance,Type,count},Ct},{{Instance,Type,size},Size}|Rest]) ->
    case {Size,Ct} of
        {0,0} -> [{Instance, Type, 0} | average_calc(Rest)];
        _ -> [{Instance,Type,Size/Ct} | average_calc(Rest)]
    end.

%% Regrouping/merging values together in proplists
average_group([]) -> [];
average_group([{Instance,Type1,N},{Instance,Type2,M} | Rest]) ->
    [{Instance,[{Type1,N},{Type2,M}]} | average_group(Rest)].

