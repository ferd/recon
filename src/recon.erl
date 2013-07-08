-module(recon).
-export([info/1,info/3]).
-export([reductions/1, reductions/2]).
-export([remote_load/1, remote_load/2]).
-export([tcp/0, udp/0, sctp/0, files/0, port_types/0]).

%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%

%% Allows to be similar to process_info/1, but excludes
%% the mailbox, which has a tendency to grow. links could still
%% be problematic for size, but not nearly as much as messages.
info(A,B,C) -> info(pid(A,B,C)).

-spec info(pid()|atom()) -> [{atom(), [{atom(),term()}]},...].
info(Name) when is_atom(Name) ->
    info(whereis(Name));
info(Pid) when is_pid(Pid) ->
    Info = fun(List) -> erlang:process_info(Pid, List) end,
    [{meta, Info([registered_name, dictionary, group_leader, status])},
     {signals, Info([links, monitors, monitored_by, trap_exit])},
     {location, Info([initial_call, current_stacktrace])},
     {memory, Info([memory, message_queue_len, heap_size, total_heap_size,
                    garbage_collection])},
     {work, Info([reductions])}].

%% fetches reduction counts of prpcesses
-spec reductions(pos_integer()) -> [term()].
reductions(Num) ->
    lists:sublist(lists:usort(
        fun({_,A,_},{_,B,_}) -> A > B end,
        reductions()
    ), Num).

%% Fetches reduction counts of processes over a sliding window
-spec reductions(non_neg_integer(), pos_integer()) -> [term()].
reductions(Time, Num) ->
    Sample = fun reductions/0,
    {First,Last} = recon_lib:sample(Time, Sample),
    lists:sublist(lists:usort(
        fun({_,A,_},{_,B,_}) -> A > B end,
        recon_lib:sliding_window(First, Last)
    ), Num).

%% Loads one or more modules remotely, in a diskless manner
remote_load(Mod) -> remote_load(nodes(), Mod).

remote_load(Nodes=[_|_], Mod) when is_atom(Mod) ->
    {Mod, Bin, File} = code:get_object_code(Mod), 
    rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]);
remote_load(Nodes=[_|_], Modules) when is_list(Modules) ->
    [remote_load(Nodes, Mod) || Mod <- Modules];
remote_load(Node, Mod) ->
    remote_load([Node], Mod).

tcp() -> recon_lib:port_list(name, "tcp_inet").

udp() -> recon_lib:port_list(name, "udp_inet").

sctp() -> recon_lib:port_list(name, "sctp_inet").

files() -> recon_lib:port_list(name, "efile").

port_types() ->
    lists:usort(
        %% sorts by biggest count, smallest type
        fun({KA,VA}, {KB,VB}) -> {VA,KB} > {VB,KA} end,
        recon_lib:count([Name || {_, Name} <- recon_lib:port_list(name)])
    ).
    
%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
pid(X, Y, Z) ->
    list_to_pid("<" ++ integer_to_list(X) ++ "." ++
		integer_to_list(Y) ++ "." ++
		integer_to_list(Z) ++ ">").

reductions() ->
    [{Pid, Reds, {Curr, Init}}
     || Pid <- processes() -- [self()],
        {_, Reds} <- [process_info(Pid, reductions)],
        {_, Curr} <- [process_info(Pid, current_function)],
        {_, Init} <- [process_info(Pid, initial_call)]].
