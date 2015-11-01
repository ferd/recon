%%% @author zhongwen <zhongwencool@gmail.com>
-module(recon_top).

-export([top/1]).
-export([loop/1]).

-define(DEFAULT_RANK_NUM, 20). %%fill full in 13.3 inch screen(24 core)
-define(CPU_ALARM_THRESHOLD, 0.8).

-define(STABLE_SYSTEM_ITEM, [system_version, process_limit, process_count,
  port_limit, port_count, ets_limit, logical_processors]).
-define(CHANGE_SYSTEM_ITEM, [used, allocated, unused]).

-spec top(pid()) -> stop.
top(Pid) ->
  Input = io:get_line(""),
  case  Input of
    "q\n" -> erlang:send(Pid, stop);
    "r\n" -> erlang:send(Pid, reductions), top(Pid);
    "b\n" -> erlang:send(Pid, binary_memory), top(Pid);
    "h\n" -> erlang:send(Pid, total_heap_size), top(Pid);
    "m\n" -> erlang:send(Pid, memory), top(Pid);
    "p\n" -> erlang:send(Pid, pause_or_unpause), top(Pid);
    _ -> top(Pid)
  end.

-spec loop(integer()) -> stop.
loop(Interal) ->
  clear_screen(),
  {UpTime, _} = erlang:statistics(wall_clock),
  StableInfo = get_stable_system_info(), %%don't need reflush the stable info everytime
  loop(Interal, memory, UpTime, StableInfo, erlang:make_ref(), true).

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

%% reflush the shell UI
loop(Interal, Type, _UpTime, StableInfo, LastTimeRef, false) ->
  io:format("\e[31;1m PAUSE  INPUT (p, r, b, h, m ) to resume or q to quit \e[0m~n"),
  erlang:cancel_timer(LastTimeRef),
  receive
    stop -> stop;
    pause_or_unpause ->
      clear_screen(),
      {NewUpTime, _} = erlang:statistics(wall_clock),
      loop(Interal, Type, NewUpTime, StableInfo, LastTimeRef, true);
    NewType ->
      {NewUpTime, _} = erlang:statistics(wall_clock),
      loop(Interal, NewType, NewUpTime, StableInfo, LastTimeRef, true)
  end;
loop(Interal, Type, UpTime, StableInfo, LastTimeRef, Status) ->
  move_cursor_to_top(),
  draw_system_status(UpTime, StableInfo),
  draw_node_stauts_status(),
  draw_process_rank(Type, ?DEFAULT_RANK_NUM),
  io:format("\e[31;1mINPUT:\e[0m\e[44m  ~s |  ~s |   ~s       |~s|   ~s    |      ~s         \e[49m|~n",
    ["r(reduction)", "q(quit)", "b(binary memory)", "h(total heap  size)", "m(memory)", "p(pause/unpause)"]),
  erlang:cancel_timer(LastTimeRef),
  TimeRef = erlang:send_after(Interal, self(), Type),
  receive
    stop -> stop;
    pause_or_unpause -> loop(Interal, Type, UpTime, StableInfo, TimeRef, not Status);
    Type -> loop(Interal, Type, UpTime + Interal, StableInfo, TimeRef, Status);
    NewType ->
      {NewUpTime, _} = erlang:statistics(wall_clock),
      loop(Interal, NewType, NewUpTime, StableInfo, TimeRef, Status)
  end.

%Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace] Uptime:0 Days 12:0:35
%System Limit    | Limit           | System Count       | Count                | Memory Info          | Megabyte|                |
%Process Limit   | 262144          | Process Count      | 26                   | Use Mem              | 13.0731M                 |
%Port Limit      | 4               | Port Count         | 65536                | Allocted Mem         | 21.0444M                 |
%Ets Limit       | 2053            | Logical Processors | 4                    | Unuse Mem            | 7.0631M                  |
draw_system_status(UpTime, StableInfo) ->
  [Version, ProcLimit, ProcCount, PortCount,
    PortLimit, EtsLimit, LogicalProc] = StableInfo,
  [UseMem, AlloctedMem, UnunsedMem] = get_change_system_info(),
  FirstLine = (Version --"\n") ++ "\e[32;1m Uptime:" ++ uptime(UpTime) ++ "\e[0m",
  draw_system_info(FirstLine, ProcLimit, ProcCount, PortCount,
    PortLimit, EtsLimit, LogicalProc, UseMem, AlloctedMem, UnunsedMem).

%Memory          | Megabyte        | Process State      | Count                | Memory               | Megabyte                 |
%Total           | 13.0497M        | Reductions         | 55352                | IO Output            | 0.0000M                  |
%Process         | 3.0852M         | Process Count      | 26                   | IO Input             | 0.0000M                  |
%Atom            | 0.0180M         | Run Queue          | 0                    | Gc Count             | 0.0000M                  |
%Ets             | 0.0277M         | Error Log Queue    | 0                    | Gc Words Reclaimed   | 0.0000M                  |
%Binary          | 0.0164M                                                                                                       |
%|01[|||||||||||||||||||                                 36.68%]  |03[|||||||||||||||||||||||||                           49.17%]|
%|02[|||||||||||||||||||                                 37.51%]  |04[|||||||||||||||||||||||||||||||||||||||||||         83.68%]|
draw_node_stauts_status() ->
  [{ProcessSum, MemSum}] = recon:node_stats_list(1, 0),
  draw_memory_process_info(ProcessSum, MemSum),
  draw_scheduler_usage(MemSum),
  ok.

%| Pid                |    Memory| Initial Call             |Reductions| Msg Que|Current Function                                |
%| <0.27.0>           |    163536| erlang:apply/2           |     10011| 0      |shell:shell_rep/4                               |
%| code_server        |    142800| erlang:apply/2           |    121193| 0      |code_server:loop/1                              |
%| kernel_sup         |     58544| proc_lib:init_p/5        |      2016| 0      |gen_server:loop/6                               |
%| erl_prim_loader    |     55064| erlang:apply/2           |    200025| 0      |erl_prim_loader:loop/3                          |
%| <0.26.0>           |     42448| group:server/3           |    169998| 0      |group:more_data/5                               |
%| init               |     24632| otp_ring0:start/2        |      2939| 0      |init:loop/1                                     |
%| application_control|     13776| erlang:apply/2           |       404| 0      |gen_server:loop/6                               |
%| user_drv           |     11944| user_drv:server/2        |     75225| 0      |user_drv:server_loop/6                          |
%| <0.9.0>            |      7016| proc_lib:init_p/5        |        44| 0      |application_master:main_loop/2                  |
%| error_logger       |      7016| proc_lib:init_p/5        |       227| 0      |gen_event:fetch_msg/5                           |
%| <0.33.0>           |      6984| erlang:apply/2           |       601| 0      |io:execute_request/2                            |
%| inet_db            |      5832| proc_lib:init_p/5        |       240| 0      |gen_server:loop/6                               |
%| user               |      2904| group:server/3           |        36| 0      |group:server_loop/3                             |
%| global_name_server |      2904| proc_lib:init_p/5        |        45| 0      |gen_server:loop/6                               |
%| <0.23.0>           |      2864| proc_lib:init_p/5        |        55| 0      |gen_server:loop/6                               |
%| standard_error     |      2864| erlang:apply/2           |         9| 0      |standard_error:server_loop/1                    |
%| standard_error_sup |      2864| proc_lib:init_p/5        |        34| 0      |gen_server:loop/6                               |
%| file_server_2      |      2864| proc_lib:init_p/5        |        89| 0      |gen_server:loop/6                               |
%| kernel_safe_sup    |      2824| proc_lib:init_p/5        |        56| 0      |gen_server:loop/6                               |
%| <0.28.0>           |      2824| proc_lib:init_p/5        |       233| 0      |gen_server:loop/6                               |

draw_process_rank(memory, Num) ->
  MemoryList = recon:proc_count(memory, Num),
  io:format("\e[46m| ~-19.19s|~10.10s| ~-25.25s|~10.10s| ~-7.7s|~-48.48s\e[49m|~n",
    ["Pid", "Memory", "Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, MemVal, Call = [IsName|_]} = lists:nth(Pos, MemoryList),
     [{_, Reductions}, {_, MsgQueueLen}] = recon:info(Pid, [reductions, message_queue_len]),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrPid = display_name_or_pid(IsName, Pid),
     io:format("| ~-19.19s|~10.10s| ~-25.25s|~10.10s| ~-7.7s|~-48.48s|~n",
       [NameOrPid, to_list(MemVal), InitialCall, to_list(Reductions), to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(binary_memory, Num) ->
  MemoryList = recon:proc_count(binary_memory, Num),
  io:format("\e[46m| ~-19.19s|~10.10s| ~-25.25s|~10.10s| ~-7.7s|~-48.48s\e[49m|~n",
    ["Pid", "Bin Memory", "Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, MemVal, Call = [IsName|_]} = lists:nth(Pos, MemoryList),
     [{_, Reductions}, {_, MsgQueueLen}] = recon:info(Pid, [reductions, message_queue_len]),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrPid = display_name_or_pid(IsName, Pid),
     io:format("| ~-19.19s|~10.10s| ~-25.25s|~10.10s| ~-7.7s|~-48.48s|~n",
       [NameOrPid, to_list(MemVal), InitialCall, to_list(Reductions), to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(reductions, Num) ->
  ReductionList = recon:proc_count(reductions, Num),
  io:format("\e[46m| ~-19.19s|~10.10s| ~-25.25s|~10.10s| ~-7.7s|~-48.48s\e[49m|~n",
    ["Pid", "Reductions", "Initial Call", "Memory", "Msg Queue", "Current Function"]),
  [begin
     {Pid, Reductions, Call = [IsName|_]} = lists:nth(Pos, ReductionList),
     [{_, Memory}, {_, MsgQueueLen}] = recon:info(Pid, [memory, message_queue_len]),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrPid = display_name_or_pid(IsName, Pid),
     io:format("| ~-19.19s|~10.10s| ~-25.25s|~10.10s| ~-7.7s|~-48.48s|~n",
       [NameOrPid, to_list(Reductions), InitialCall, to_list(Memory), to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(total_heap_size, Num) ->
  HeapList = recon:proc_count(total_heap_size, Num),
  io:format("\e[46m| ~-19.19s|~10.10s| ~-25.25s|~10.10s| ~-7.7s|~-48.48s\e[49m|~n",
    ["Pid", "Total Heap Size", "Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, HeapSize, Call = [IsName|_]} = lists:nth(Pos, HeapList),
     [{_, Reductions}, {_, MsgQueueLen}] = recon:info(Pid, [reductions, message_queue_len]),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrPid = display_name_or_pid(IsName, Pid),
     io:format("| ~-19.19s|~10.10s| ~-25.25s|~10.10s| ~-7.7s|~-48.48s|~n",
       [NameOrPid, to_list(HeapSize), InitialCall, to_list(Reductions), to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)].

draw_system_info(FirstLine, ProcLimit, ProcCount, PortCount,
    PortLimit, EtsLimit, LogicalProc, UseMem, AlloctedMem, UnunsedMem) ->
  io:format("~s~n", [FirstLine]),
  io:format("\e[46m~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s\e[49m|~n",
    ["System Limit", "Limit", "System Count", "Count", "Memory Info", "Megabyte|"]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s|~n",
    ["Process Limit", ProcLimit, "Process Count", ProcCount, "Use Mem", UseMem]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s|~n",
    ["Port Limit", PortLimit, "Port Count", PortCount, "Allocted Mem", AlloctedMem]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s|~n",
    ["Ets Limit", EtsLimit, "Logical Processors", LogicalProc, "Unuse Mem", UnunsedMem]).

draw_memory_process_info(ProcessSum, MemSum) ->
  TotalMem = to_megabyte_list(proplists:get_value(memory_total, ProcessSum)),
  ProcMem = to_megabyte_list(proplists:get_value(memory_procs, ProcessSum)),
  AtomMem = to_megabyte_list(proplists:get_value(memory_atoms, ProcessSum)),
  BinMem = to_megabyte_list(proplists:get_value(memory_bin, ProcessSum)),
  EtsMem = to_megabyte_list(proplists:get_value(memory_ets, ProcessSum)),
  ProcessCount = integer_to_list(proplists:get_value(process_count, ProcessSum)),
  Runqueue = integer_to_list(proplists:get_value(run_queue, ProcessSum)),
  ErrorLogCount = integer_to_list(proplists:get_value(error_logger_queue_len, ProcessSum)),
  BytesIn = to_megabyte_list(proplists:get_value(bytes_in, MemSum)),
  BytesOut = to_megabyte_list(proplists:get_value(bytes_out, MemSum)),
  GcCount = to_megabyte_list(proplists:get_value(gc_count, MemSum)),
  GcWordsReclaimed = to_megabyte_list(proplists:get_value(gc_words_reclaimed, MemSum)),
  Reductions = integer_to_list(proplists:get_value(reductions, MemSum)),
  io:format("\e[46m~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s\e[49m|~n",
    ["Memory", "Megabyte", "Process State", "Count", "Memory", "Megabyte"]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s|~n",
    ["Total", TotalMem, "Reductions", Reductions, "IO Output", BytesOut]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s|~n",
    ["Process", ProcMem, "Process Count", ProcessCount, "IO Input", BytesIn]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s|~n",
    ["Atom", AtomMem, "Run Queue", Runqueue, "Gc Count", GcCount]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-25.25s|~n",
    ["Ets", EtsMem, "Error Log Queue", ErrorLogCount, "Gc Words Reclaimed", GcWordsReclaimed]),
  io:format("~-15.15s | ~-110.110s|~n", ["Binary", BinMem]).

draw_scheduler_usage(MemSum) ->
  SchedulerUsage = proplists:get_value(scheduler_usage, MemSum),
  SchedulerNum = erlang:length(SchedulerUsage),
  HalfSchedulerNum = SchedulerNum div 2,
  [begin
     Percent1 = proplists:get_value(Seq, SchedulerUsage),
     Percent2 = proplists:get_value(Seq + HalfSchedulerNum, SchedulerUsage),
     CPU1 = to_list(Percent1) ++ "%",
     CPU2 = to_list(Percent2) ++ "%",
     CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq])),
     CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq + HalfSchedulerNum])),
     Process1 = lists:duplicate(trunc(Percent1 * 52), "|"),
     Process2 = lists:duplicate(trunc(Percent2 * 52), "|"),
     Format = format_alarm_color(Percent1, Percent2),
     io:format(Format,
       [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2])
   end|| Seq <- lists:seq(1, HalfSchedulerNum)].

get_current_initial_call(Call) ->
  {_, CurFun} = lists:keyfind(current_function, 1, Call),
  {_, InitialCall} = lists:keyfind(initial_call, 1, Call),
  {to_list(CurFun), to_list(InitialCall)}.

get_stable_system_info() ->
  [begin to_list(erlang:system_info(Item))end ||Item<- ?STABLE_SYSTEM_ITEM].

get_change_system_info() ->
  [begin to_megabyte_list(recon_alloc:memory(Item)) end || Item <- ?CHANGE_SYSTEM_ITEM].

format_alarm_color(Percent1, Percent2)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m] \e[31m |~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m]|~n";
format_alarm_color(Percent1, _Percent2)when Percent1 >= ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m] \e[32m |~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m]|~n";
format_alarm_color(_Percent1, Percent2)when Percent2 >= ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m] \e[31m |~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m]|~n";
format_alarm_color(_Percent1, _Percent2) ->
  "\e[32m|~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m] \e[32m |~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m]|~n".

display_name_or_pid(IsName, _Pid)when is_atom(IsName) -> atom_to_list(IsName);
display_name_or_pid(_IsName, Pid) -> erlang:pid_to_list(Pid).

to_megabyte_list(M) ->
  Val = trunc(M/(1024*1024)*1000),
  Integer = Val div 1000,
  Decmial = Val - Integer * 1000,
  lists:flatten(io_lib:format("~w.~4..0wM", [Integer, Decmial])).

clear_screen() ->
  io:format("\e[H\e[J").

move_cursor_to_top() ->
  io:format("\e[H").

to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Pid) when is_pid(Pid) -> erlang:pid_to_list(Pid);
to_list({Module, Fun, Arg}) ->
  atom_to_list(Module) ++ ":" ++
    atom_to_list(Fun) ++ "/" ++
    integer_to_list(Arg);
to_list(Float) when is_float(Float)->
  Val = trunc(Float*10000),
  Integer = Val div 100,
  Decmial = Val - Integer * 100,
  case Integer of
    100 -> "100.0";
    _ -> lists:flatten(io_lib:format("~2..0w.~2..0w", [Integer, Decmial]))
  end;
to_list(Val) -> Val.

uptime(UpTime) ->
  {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
  lists:flatten(io_lib:format("~p Days ~p:~p:~p", [D, H, M, S])).
