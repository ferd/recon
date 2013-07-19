-module(recon).
-export([info/1,info/3]).
-export([reductions/1, reductions/2,
         memory/1, memory/2,
         proc_count/2, proc_window/3]).
-export([remote_load/1, remote_load/2]).
-export([tcp/0, udp/0, sctp/0, files/0, port_types/0]).

%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%

%%% Process Info %%%

%% @doc Equivalent to info(<A.B.C>) where A, B, and C are integers part
%% of a pid
-spec info(N,N,N) -> [{atom(), [{atom(),term()}]},...] when
      N :: non_neg_integer().
info(A,B,C) -> info(pid(A,B,C)).

%% @doc Allows to be similar to `erlang:process_info/1', but excludes fields
%% such as the mailbox, which have a tendency to grow and be unsafe when called
%% in production systems. Also includes a few more fields than what is usually
%% given (`monitors', `monitored_by', etc.), and separates the fields in a more
%% readable format based on the type of information contained.
%%
%% Moreover, it will fetch and read information on local processes that were
%% registered locally (an atom), globally (`{global, Name}'), or through
%% another registry supported in the `{via, Module, Name}' syntax (must have a
%% `Module:whereis_name/1' function). Pids can also be passed in as a string
%% (`"<0.39.0>"') and will be converted to be used.
-spec info(Name) -> [{Type, [{Key, Value}]},...] when
      Name :: pid() | atom() | string()
           | {global, term()} | {via, module(), term()},
      Type :: meta | signals | location | memory | work,
      Key :: registered_name | dictionary | group_leader | status
           | links | monitors | monitored_by | trap_exit | initial_call
           | current_stacktrace | memory | message_queue_len | heap_size
           | total_heap_size | garbage_collection | reductions,
      Value :: term().
info(Name) when is_atom(Name) ->
    info(whereis(Name));
info(List = "<0."++_) ->
    info(list_to_pid(List));
info({global, Name}) ->
    info(global:whereis_name(Name));
info({via, Module, Name}) ->
    info(Module:whereis_name(Name));
info(Pid) when is_pid(Pid) ->
    Info = fun(List) -> erlang:process_info(Pid, List) end,
    [{meta, Info([registered_name, dictionary, group_leader, status])},
     {signals, Info([links, monitors, monitored_by, trap_exit])},
     {location, Info([initial_call, current_stacktrace])},
     {memory, Info([memory, message_queue_len, heap_size, total_heap_size,
                    garbage_collection])},
     {work, Info([reductions])}].

%% fetches reduction counts of processes
-spec reductions(pos_integer()) -> [term()].
reductions(Num) -> proc_count(reductions, Num).

%% Fetches reduction counts of processes over a sliding window
-spec reductions(non_neg_integer(), pos_integer()) -> [term()].
reductions(Time, Num) -> proc_window(reductions, Time, Num).

%% Fetches memory of processes
-spec memory(pos_integer()) -> [term()].
memory(Num) -> proc_count(memory, Num).

%% Fetches memory deltas of processes over a sliding window
-spec memory(non_neg_integer(), pos_integer()) -> [term()].
memory(Time, Num) -> proc_window(memory, Time, Num).

%% Fetches a given attribute from all processes and returns
%% the biggest culprits over a sliding window
-spec proc_count(atom(), pos_integer()) -> [term()].
proc_count(AttrName, Num) ->
    lists:sublist(lists:usort(
        fun({_,A,_},{_,B,_}) -> A > B end,
        proc_attrs(AttrName)
    ), Num).

%% Fetches a given attribute from all processes and returns
%% the biggest culprits
-spec proc_window(atom(), non_neg_integer(), pos_integer()) -> [term()].
proc_window(AttrName, Time, Num) ->
    Sample = fun() -> proc_attrs(AttrName) end,
    {First,Last} = recon_lib:sample(Time, Sample),
    %% We could make a short dict of biggest entries to save on a full sort
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

proc_attrs(AttrName) ->
    [{Pid, Attr, {Curr, Init}}
     || Pid <- processes() -- [self()],
        [{_, Attr}, {_, Curr}, {_, Init}] <-
            [process_info(Pid, [AttrName, current_function, initial_call])]].
