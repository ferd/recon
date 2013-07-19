-module(recon).
-export([info/1,info/3,
         proc_count/2, proc_window/3,
         bin_leak/1]).
-export([get_state/1]).
-export([remote_load/1, remote_load/2]).
-export([tcp/0, udp/0, sctp/0, files/0, port_types/0]).

%%%%%%%%%%%%%
%%% TYPES %%%
%%%%%%%%%%%%%
-type proc_attrs() :: {pid(),
                       Attr::_,
                       [Name::atom()
                       |{current_function, mfa()}
                       |{initial_call, mfa()}, ...]}.

-export_type([proc_attrs/0]).
%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%

%%% Process Info %%%

%% @doc Equivalent to `info(<A.B.C>)' where `A', `B', and `C' are integers part
%% of a pid
-spec info(N,N,N) -> [{atom(), [{atom(),term()}]},...] when
      N :: non_neg_integer().
info(A,B,C) -> info(recon_lib:triple_to_pid(A,B,C)).

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

%% @doc Fetches a given attribute from all processes and returns
%% the biggest `Num' consumers.
%% @todo Implement this function so it only stores `Num' entries in
%% memory at any given time, instead of as many as there are
%% processes.
-spec proc_count(AttributeName, Num) -> [term()] when
      AttributeName :: atom(),
      Num :: non_neg_integer().
proc_count(AttrName, Num) ->
    lists:sublist(lists:usort(
        fun({_,A,_},{_,B,_}) -> A > B end,
        recon_lib:proc_attrs(AttrName)
    ), Num).

%% @doc Fetches a given attribute from all processes and returns
%% the biggest entries, over a sliding time window.
%% 
%% This function is particularly useful when processes on the node
%% are mostly short-lived, usually too short to inspect through other
%% tools, in order to figure out what kind of processes are eating
%% through a lot resources on a given node.
%%
%% It is important to see this function as a snapshot over a sliding
%% window. A program's timeline during sampling might look like this:
%%
%%  `--w---- [Sample1] ---x-------------y----- [Sample2] ---z--->'
%%
%% Some processes will live between `w' and die at `x', some between `y' and
%% `z', and some between `x' and `y'. These samples will not be too significant
%% as they're incomplete. If the majority of your processes run between a time
%% interval `x'...`y' (in absolute terms), you should make sure that your
%% sampling time is smaller than this so that for many processes, their
%% lifetime spans the equivalent of `w' and `z'. Not doing this can skew the
%% results: long-lived processes, that have 10 times the time to accumulate
%% data (say reductions) will look like bottlenecks when they're not one.
%%
%% Warning: this function depends on data gathered at two snapshots, and then
%% building a dictionary with entries to differentiate them. This can take a
%% heavy toll on memory when you have many dozens of thousands of processes.
-spec proc_window(AttributeName, Num, Milliseconds) -> [term()] when
      AttributeName :: atom(),
      Num :: non_neg_integer(),
      Milliseconds :: pos_integer().
proc_window(AttrName, Time, Num) ->
    Sample = fun() -> recon_lib:proc_attrs(AttrName) end,
    {First,Last} = recon_lib:sample(Time, Sample),
    lists:sublist(lists:usort(
        fun({_,A,_},{_,B,_}) -> A > B end,
        recon_lib:sliding_window(First, Last)
    ), Num).

%% @doc Refc binaries can be leaking when barely-busy processes route them
%% around and do little else, or when extremely busy processes reach a stable
%% amount of memory allocated and do the vast majority of their work with refc
%% binaries. When this happens, it may take a very long while before references
%% get deallocated and refc binaries get to be garbage collected, leading to
%% Out Of Memory crashes.
%% This function fetches the number of refc binary references in each process
%% of the node, garbage collects them, and compares the resulting number of
%% references in each of them. The function then returns the `N' processes
%% that freed the biggest amount of binaries, potentially highlighting leaks.
%% 
%% See <a href="http://www.erlang.org/doc/efficiency_guide/binaryhandling.html#id65722">The efficiency guide</a>
%% for more details on refc binaries
-spec bin_leak(pos_integer()) -> [proc_attrs()].
bin_leak(N) ->
    lists:sublist(
        lists:usort(
            fun({K1,V1,_},{K2,V2,_}) -> {V1,K1} =< {V2,K2} end,
            [try
                {_,Pre,Id} = recon_lib:proc_attrs(binary, Pid),
                erlang:garbage_collect(Pid),
                {_,Post,_} = recon_lib:proc_attrs(binary, Pid),
                {Pid, length(Post)-length(Pre), Id}
            catch
                _:_ -> {Pid, 0}
            end || Pid <- processes()]),
        N).

%%% OTP & Manipulations %%%

%% @doc Fetch the internal state of an OTP process.
%% Calls `sys:get_state/1' directly in R16B01+, and fetches
%% it dynamically on older versions of OTP.
-spec get_state(Name) -> term() when
      Name :: pid() | atom() | {global, term()} | {via, module(), term()}.
get_state(Proc) ->
    try 
        sys:get_state(Proc)
    catch
        error:undef ->
            case sys:get_status(Proc) of
                {status,_Pid,{module,gen_server},Data} ->
                    {data, Props} = lists:last(lists:nth(5, Data)),
                    proplists:get_value("State", Props);
                {status,_Pod,{module,gen_fsm},Data} ->
                    {data, Props} = lists:last(lists:nth(5, Data)),
                    proplists:get_value("StateData", Props)
            end
    end.

%%% Code & Stuff %%%

%% @doc Equivalent to `remote_load(nodes(), Mod)'.
-spec remote_load(module()) -> term().
remote_load(Mod) -> remote_load(nodes(), Mod).

%% @doc Loads one or more modules remotely, in a diskless manner.  Allows to
%% share code loaded locally with a remote node that doesn't have it
-spec remote_load(Nodes, module()) -> term() when
      Nodes :: [node(),...] | node().
remote_load(Nodes=[_|_], Mod) when is_atom(Mod) ->
    {Mod, Bin, File} = code:get_object_code(Mod), 
    rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]);
remote_load(Nodes=[_|_], Modules) when is_list(Modules) ->
    [remote_load(Nodes, Mod) || Mod <- Modules];
remote_load(Node, Mod) ->
    remote_load([Node], Mod).

%%% Ports Info %%%

%% @doc returns a list of all TCP ports (the data type) open on the node.
-spec tcp() -> [port()].
tcp() -> recon_lib:port_list(name, "tcp_inet").

%% @doc returns a list of all UDP ports (the data type) open on the node.
-spec udp() -> [port()].
udp() -> recon_lib:port_list(name, "udp_inet").

%% @doc returns a list of all SCTP ports (the data type) open on the node.
-spec sctp() -> [port()].
sctp() -> recon_lib:port_list(name, "sctp_inet").

%% @doc returns a list of all file handlers open on the node.
-spec files() -> [port()].
files() -> recon_lib:port_list(name, "efile").

%% @doc Shows a list of all different ports on the node with their respective
%% types.
-spec port_types() -> [{Type::string(),pos_integer()}].
port_types() ->
    lists:usort(
        %% sorts by biggest count, smallest type
        fun({KA,VA}, {KB,VB}) -> {VA,KB} > {VB,KA} end,
        recon_lib:count([Name || {_, Name} <- recon_lib:port_list(name)])
    ).
