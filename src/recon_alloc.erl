%%% @doc Functions to deal with
%%% <a href="http://www.erlang.org/doc/man/erts_alloc.html">Erlang's memory
%%% allocators</a>, or particularly, to try to present the allocator data
%%% in a way that makes it simpler to discover possible problems.
%%%
%%% Tweaking Erlang memory allocators and their behaviour is a very tricky
%%% ordeal whenever you have to give up the default settings. This module
%%% (and its documentation) will try and provide helpful pointers to help
%%% in this task.
%%%
%%% This module should mostly be helpful to figure out <em>if</em> there is
%%% a problem, but will offer little help to figure out <em>what</em> is wrong.
%%%
%%% To figure this out, you need to dig deeper into the allocator data
%%% (obtainable with {@link allocators/0}), and/or have some precise knowledge
%%% about the type of load and work done by the VM to be able to assess what
%%% each reaction to individual tweak should be.
%%%
%%% A lot of trial and error might be required to figure out if tweaks have
%%% helped or not, ultimately.
%%%
%%% In order to help do offline debugging of memory allocator problems
%%% recon_alloc also has a few functions that store snapshots of the
%%% memory statistics.
%%% These snapshots can be used to freeze the current allocation values so that
%%% they do not change during analysis while using the regular functionality of
%%% this module, so that the allocator values can be saved, or that
%%% they can be shared, dumped, and reloaded for further analysis using files.
%%% See {@link snapshot_load/1} for a simple use-case.
%%%
%%% Glossary:
%%% <dl>
%%%   <dt>sys_alloc</dt>
%%%   <dd>System allocator, usually just malloc</dd>
%%%
%%%   <dt>mseg_alloc</dt>
%%%   <dd>Used by other allocators, can do mmap. Caches allocations</dd>
%%%
%%%   <dt>temp_alloc</dt>
%%%   <dd>Used for temporary allocations</dd>
%%%
%%%   <dt>eheap_alloc</dt>
%%%   <dd>Heap data (i.e. process heaps) allocator</dd>
%%%
%%%   <dt>binary_alloc</dt>
%%%   <dd>Global binary heap allocator</dd>
%%%
%%%   <dt>ets_alloc</dt>
%%%   <dd>ETS data allocator</dd>
%%%
%%%   <dt>driver_alloc</dt>
%%%   <dd>Driver data allocator</dd>
%%%
%%%   <dt>sl_alloc</dt>
%%%   <dd>Short-lived memory blocks allocator</dd>
%%%
%%%   <dt>ll_alloc</dt>
%%%   <dd>Long-lived data (i.e. Erlang code itself) allocator</dd>
%%%
%%%   <dt>fix_alloc</dt>
%%%   <dd>Frequently used fixed-size data allocator</dd>
%%%
%%%  <dt>std_alloc</dt>
%%%  <dd>Allocator for other memory blocks</dd>
%%%
%%%  <dt>carrier</dt>
%%%  <dd>When a given area of memory is allocated by the OS to the
%%%    VM (through sys_alloc or mseg_alloc), it is put into a 'carrier'. There
%%%    are two kinds of carriers: multiblock and single block. The default
%%%    carriers data is sent to are multiblock carriers, owned by a specific
%%%    allocator (ets_alloc, binary_alloc, etc.). The specific allocator can
%%%    thus do allocation for specific Erlang requirements within bits of
%%%    memory that has been preallocated before. This allows more reuse,
%%%    and we can even measure the cache hit rates {@link cache_hit_rates/0}.
%%%
%%%    There is however a threshold above which an item in memory won't fit
%%%    a multiblock carrier. When that happens, the specific allocator does
%%%    a special allocation to a single block carrier. This is done by the
%%%    allocator basically asking for space directly from sys_alloc or
%%%    mseg_alloc rather than a previously multiblock area already obtained
%%%    before.
%%%
%%%    This leads to various allocation strategies where you decide to
%%%    choose:
%%%    <ol>
%%%      <li>which multiblock carrier you're going to (if at all)</li>
%%%      <li>which block in that carrier you're going to</li>
%%%    </ol>
%%%
%%%    See <a href="http://www.erlang.org/doc/man/erts_alloc.html">the official
%%%    documantation on erts_alloc</a> for more details.
%%%  </dd>
%%%
%%%  <dt>mbcs</dt>
%%%  <dd>Multiblock carriers.</dd>
%%%
%%%  <dt>sbcs</dt>
%%%  <dd>Single block carriers.</dd>
%%%
%%%  <dt>lmbcs</dt>
%%%  <dd>Largest multiblock carrier size</dd>
%%%
%%%  <dt>smbcs</dt>
%%%  <dd>Smallest multiblock carrier size</dd>
%%%
%%%  <dt>sbct</dt>
%%%  <dd>Single block carrier threshold</dd>
%%% </dl>
%%%
%%% All sizes returned by this module are in bytes.
%%%
-module(recon_alloc).
-define(UTIL_ALLOCATORS, [temp_alloc,
                          eheap_alloc,
                          binary_alloc,
                          ets_alloc,
                          driver_alloc,
                          sl_alloc,
                          ll_alloc,
                          fix_alloc,
                          std_alloc
                         ]).

-type allocator() :: temp_alloc | eheap_alloc | binary_alloc | ets_alloc
                   | driver_alloc | sl_alloc | ll_alloc | fix_alloc
                   | std_alloc.
-type instance() :: non_neg_integer().
-type allocdata(T) :: {{allocator(), instance()}, T}.
-export_type([allocator/0, instance/0, allocdata/1]).

-define(CURRENT_POS, 2). % pos in sizes tuples for current value
-define(MAX_POS, 4). % pos in sizes tuples for max value

%% Regular usage
-export([memory/1, fragmentation/1, cache_hit_rates/0, average_sizes/0,
         sbcs_to_mbcs/0, allocators/0]).

%% Snapshot handling
-type memory() :: [{atom(),atom()}].
-type snapshot() :: {memory(),[allocdata(term())]}.

-export_type([memory/0, snapshot/0]).

-export([snapshot/0,  snapshot_clear/0,
         snapshot_print/0, snapshot_get/0,
         snapshot_save/1,  snapshot_load/1]).

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%

%% @doc reports one of multiple possible memory values for the entire
%% node depending on what is to be reported:
%%
%% <ul>
%%   <li>`memory(used)' reports the memory that is actively used for allocated
%%       Erlang data;</li>
%%   <li>`memory(allocated)' reports the memory that is reserved by the VM. It
%%       includes the memory used, but also the memory yet-to-be-used but still
%%       given by the OS. This is the amount you want if you're dealing with
%%       ulimit and OS-reported values.</li>
%%   <li>`memory(unused)' reports the amount of memory reserved by the VM that
%%       is not being allocated.
%%       Equivalent to `memory(allocated) - memory(used)'.</li>
%%   <li>`memory(usage)' returns a percentage (0.0 .. 1.0) of `used/allocated'
%%       memory ratios.</li>
%% </ul>
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
    mem(total);
memory(allocated) ->
    AllocProps = [Prop || {_Alloc,Prop} <- util_alloc()],
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
%% influence the tuning of allocators to better fit sizes of blocks and/or
%% carriers.
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
    end || {{Allocator, N}, Props} <- util_alloc()],
    [{Key,Val} || {_W, Key, Val} <- lists:reverse(lists:sort(WeighedData))].

%% @doc looks at the `mseg_alloc' allocator (allocator used by all the
%% allocators in {@link allocator()}) and returns information relative to
%% the cache hit rates. Unless memory has expected spiky behaviour, it should
%% usually be above 0.80 (80%).
%%
%% Cache can be tweaked using three VM flags: `+MMmcs', `+MMrmcbf', and
%% `+MMamcbf'.
%%
%% `+MMmcs' stands for the maximum amount of cached memory segments. Its
%% default value is '10' and can be anything from 0 to 30. Increasing
%% it first and verifying if cache hits get better should be the first
%% step taken.
%%
%% The two other options specify what are the maximal values of a segment
%% to cache, in relative (in percent) and absolute terms (in kilobytes),
%% respectively. Increasing these may allow more segments to be cached, but
%% should also add overheads to memory allocation. An Erlang node that has
%% limited memory and increases these values may make things worse on
%% that point.
%%
%% The values returned by this function are sorted by a weight combining
%% the lower cache hit joined to the largest memory values allocated.
-spec cache_hit_rates() -> [{{instance,instance()}, [{Key,Val}]}] when
    Key :: hit_rate | hits | calls,
    Val :: term().
cache_hit_rates() ->
    WeighedData = [begin
      Mem = proplists:get_value(memkind, Props),
      {_,Hits} = lists:keyfind(cache_hits, 1, proplists:get_value(status,Mem)),
      {_,Giga,Ones} = lists:keyfind(mseg_alloc,1,proplists:get_value(calls,Mem)),
      Calls = 1000000000*Giga + Ones,
      HitRate = usage(Hits,Calls),
      Weight = (1.00 - HitRate)*Calls,
      {Weight, {instance,N}, [{hit_rate,HitRate}, {hits,Hits}, {calls,Calls}]}
    end || {{_, N}, Props} <- alloc([mseg_alloc])],
    [{Key,Val} || {_W,Key,Val} <- lists:reverse(lists:sort(WeighedData))].

%% @doc Checks all allocators in {@link allocator()} and returns the average
%% carrier sized being used for `mbcs' and `sbcs'. This value is interesting
%% to use because it will tell us how used most carriers are individually being
%% used for. This can be related to the VM's largest multiblock carrier size
%% (`lmbcs') and smallest multiblock carrier size (`smbcs') to specify
%% allocation strategies regarding the block sizes to be used.
%%
%% This function isn't exceptionally useful unless you know you have some
%% specific problem, say with sbcs/mbcs ratios (see {@link sbcs_to_mbcs/0})
%% or fragmentation for a specific allocator, and want to figure out what
%% values to pick to increase or decrease sizes compared to the currently
%% configured value.
%%
%% Do note that values for `lmbcs' and `smbcs' are going to be rounded up
%% to the next power of two when configuring them.
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
    util_alloc()),
    average_group(average_calc(lists:sort(dict:to_list(Dict)))).

%% @doc compares the amount of single block carriers (`sbcs') vs the
%% number of multiblock carriers (`mbcs') for each individual allocator in
%% {@link allocator()}.
%%
%% When a specific piece of data is allocated, it is compared to a threshold,
%% called the 'single block carrier threshold' (`sbct'). When the data is
%% larger than the `sbct', it gets sent to a single block carrier. When the
%% data is smaller than the `sbct', it gets placed into a multiblock carrier.
%%
%% mbcs are to be prefered to sbcs because they basically represent pre-
%% allocated memory, whereas sbcs will map to one call to sys_alloc (often
%% just malloc) or mmap, which is more expensive than redistributing data
%% that was obtain for multiblock carriers. Moreover, the VM is able to do
%% specific work with mbcs that should help reduce fragmentation in ways
%% sys_alloc or mmap usually won't.
%%
%% Ideally, most of the data should fit inside multiblock carriers. If
%% most of the data ends up in `sbcs', you may need to adjust the multiblock
%% carrier sizes, specifically the maximal value (`lmbcs') and the threshold
%% (`sbct'). On 32 bit VMs, `sbct' is limited to 8MBs, but 64 bit VMs can go
%% to pretty much any practical size.
%%
%% Given the value returned is a ratio of sbcs/mbcs, the higher the value,
%% the worst the condition. The list is sorted accordingly.
-spec sbcs_to_mbcs() -> [allocdata(term())].
sbcs_to_mbcs() ->
    Pos = ?CURRENT_POS,
    WeightedList = [begin
      LS = proplists:get_value(sbcs, Props),
      LM = proplists:get_value(mbcs,Props),
      Sbcs = element(Pos, lists:keyfind(blocks,1,LS)),
      Mbcs = element(Pos, lists:keyfind(blocks,1,LM)),
      Ratio = case {Sbcs, Mbcs} of
        {0,0} -> 0;
        {_,0} -> infinity; % that is bad!
        {_,_} -> Sbcs / Mbcs
      end,
      {Ratio, {Allocator,N}}
     end || {{Allocator, N}, Props} <- util_alloc()],
    [{Alloc,Ratio} || {Ratio,Alloc} <- lists:reverse(lists:sort(WeightedList))].

%% @doc returns a dump of all allocator settings and values
-spec allocators() -> [allocdata(term())].
allocators() ->
    {_libc,_Vsn,Allocators,_Opts} = erlang:system_info(allocator),
    %% versions is deleted in order to allow the use of the orddict api,
    %% and never really having come across a case where it was useful to know.
    [{{A,N},proplists:delete(versions,Props)} ||
        A <- Allocators,
        {_,N,Props} <- erlang:system_info({allocator,A})].

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Snapshot handling %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Take a new snapshot of the current memory allocator statistics.
%% The snapshot is stored in the process dictionary of the calling process,
%% with all the limitations that it implies (i.e. no garbage-collection).
%% To unsert the snapshot, see {@link snapshot_clear/1}.
-spec snapshot() -> snapshot() | undefined.
snapshot() ->
    put(recon_alloc_snapshot, snapshot_int()).

%% @doc clear the current snapshot in the process dictionary, if present,
%% and return the value it had before being unset.
%% @end
%% Maybe we should use erlang:delete(Key) here instead?
-spec snapshot_clear() -> snapshot() | undefined.
snapshot_clear() ->
    put(recon_alloc_snapshot, undefined).

%% @doc print a dump of the current snapshot stored by {@link snapshot/0}
%% Prints `undefined' if no snapshot has been taken.
-spec snapshot_print() -> ok.
snapshot_print() ->
    io:format("~p.~n",[snapshot_get()]).

%% @doc returns the current snapshot stored by {@link snapshot/0}.
%% Returns `undefined' if no snapshot has been taken.
-spec snapshot_get() -> snapshot() | undefined.
snapshot_get() ->
    get(recon_alloc_snapshot).

%% @doc save the current snapshot taken by {@link snapshot/0} to a file. 
%% If there is no current snapshot, a snaphot of the current allocator
%% statistics will be written to the file.
-spec snapshot_save(Filename) -> ok when
      Filename :: file:name().
snapshot_save(Filename) ->
    Snapshot = case snapshot_get() of
                   undefined ->
                       snapshot_int();
                   Snap ->
                       Snap
               end,
    case file:write_file(Filename,io_lib:format("~p.~n",[Snapshot])) of
        ok -> ok;
        {error,Reason} ->
            erlang:error(Reason,[Filename])
    end.


%% @doc load a snapshot from a given file. The format of the data in the
%% file can be either the same as output by {@link snapshot_save()},
%% or the output obtained by calling
%%  `{erlang:memory(),[{A,erlang:system_info({allocator,A})} || A <- element(3,erlang:system_info(allocator))]}.'
%% and storing it in a file.
%% If the latter option is taken, please remember to add a full stop at the end
%% of the resulting Erlang term, as this function uses `file:consult/1' to load
%% the file.
%%
%% Example usage:
%%
%%```On target machine:
%%     1> recon_alloc:snapshot().
%%     unefined
%%     2> recon_alloc:memory(used).
%%     18411064
%%     3> recon_alloc:snapshot_save("recon_snapshot.terms").
%%     ok
%%
%%   On other machine:
%%     1> recon_alloc:snapshot_load("recon_snapshot.terms").
%%     undefined
%%     2> recon_alloc:memory(used).
%%     18411064'''
%%
-spec snapshot_load(Filename) -> snapshot() | undefined when
      Filename :: file:name().
snapshot_load(Filename) ->
    {ok,[Terms]} = file:consult(Filename),
    Snapshot =
        case Terms of
            %% We handle someone using
            %% {erlang:memory(),
            %%  [{A,erlang:system_info({allocator,A})} ||
            %%     A <- element(3,erlang:system_info(allocator))]}.
            %% to dump data.
            {M,[{Alloc,_D}|_] = Allocs} when is_atom(Alloc) ->
                {M,[{{A,N},proplists:delete(versions,Props)} ||
                       {A,Instances} <- Allocs,
                       {_, N, Props} <- Instances]};
            %% We assume someone used recon_alloc:snapshot() to store this one
            Terms ->
                Terms
        end,
    put(recon_alloc_snapshot,Snapshot).

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

%% Sort on small usage vs large size.
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

%% Create a new snapshot
snapshot_int() ->
    {erlang:memory(),allocators()}.

%% If no snapshot has been taken/loaded then we use current values
snapshot_get_int() ->
    case snapshot_get() of
        undefined ->
            snapshot_int();
        Snapshot ->
            Snapshot
    end.

%% Get the alloc part of a snapshot
alloc() ->
    {_Mem,Allocs} = snapshot_get_int(),
    Allocs.
alloc(Type) ->
    [{{T,Instance},Props} || {{T,Instance},Props} <- alloc(),
                             lists:member(T,Type)].

%% Get only alloc_util allocs
util_alloc() ->
    alloc(?UTIL_ALLOCATORS).

%% Get the erlang:memory part of a snapshot
mem() ->
    {Mem,_Allocs} = snapshot_get_int(),
    Mem.
mem(Type) ->
    orddict:fetch(Type,mem()).
