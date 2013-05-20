-module(recon).
-export([info/1,info/3,reductions/2]).

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
    Info = fun(List) ->
        [{Key, Val} || Key <- List,
                       {_,Val} <- [erlang:process_info(Pid, Key)]]
    end,
    [{meta, Info([registered_name, dictionary, group_leader, status])},
     {signals, Info([links, monitors, monitored_by, trap_exit])},
     {location, Info([initial_call, current_stacktrace])},
     {memory, Info([memory, message_queue_len, heap_size, total_heap_size,
                    garbage_collection])},
     {work, Info([reductions])}].

%% Fetches reduction counts of processes over a sliding window
-spec reductions(non_neg_integer(), pos_integer()) -> [term()].
reductions(Time, Num) ->
    Sample = fun() ->
        [{Pid, Reds, {Curr, Init}}
         || Pid <- processes(),
            {_, Reds} <- [process_info(Pid, reductions)],
            {_, Curr} <- [process_info(Pid, current_function)],
            {_, Init} <- [process_info(Pid, initial_call)]]
    end,
    {First,Last} = recon_lib:sample(Time, Sample),
    lists:sublist(lists:usort(recon_lib:sliding_window(First, Last)), Num).

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
pid(X, Y, Z) ->
    list_to_pid("<" ++ integer_to_list(X) ++ "." ++
		integer_to_list(Y) ++ "." ++
		integer_to_list(Z) ++ ">").
